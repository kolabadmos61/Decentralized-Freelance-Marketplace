(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROJECT_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_ALREADY_EXISTS (err u104))
(define-constant ERR_MILESTONE_NOT_FOUND (err u105))
(define-constant ERR_INVALID_MILESTONE (err u106))
(define-constant ERR_DISPUTE_EXISTS (err u107))

(define-data-var project-counter uint u0)
(define-data-var milestone-counter uint u0)
(define-data-var dispute-counter uint u0)

(define-map projects
  uint
  {
    client: principal,
    freelancer: (optional principal),
    title: (string-ascii 100),
    description: (string-ascii 500),
    total-amount: uint,
    status: (string-ascii 20),
    created-at: uint,
    deadline: uint
  }
)

(define-map milestones
  uint
  {
    project-id: uint,
    description: (string-ascii 200),
    amount: uint,
    status: (string-ascii 20),
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map project-milestones
  uint
  (list 20 uint)
)

(define-map disputes
  uint
  {
    project-id: uint,
    milestone-id: uint,
    initiator: principal,
    reason: (string-ascii 300),
    status: (string-ascii 20),
    created-at: uint,
    resolved-at: (optional uint)
  }
)

(define-map escrow-balances
  uint
  uint
)

(define-map freelancer-profiles
  principal
  {
    name: (string-ascii 50),
    skills: (string-ascii 200),
    hourly-rate: uint,
    rating: uint,
    completed-projects: uint
  }
)

(define-map client-profiles
  principal
  {
    name: (string-ascii 50),
    company: (string-ascii 100),
    rating: uint,
    total-projects: uint
  }
)

(define-public (create-freelancer-profile (name (string-ascii 50)) (skills (string-ascii 200)) (hourly-rate uint))
  (begin
    (asserts! (is-none (map-get? freelancer-profiles tx-sender)) ERR_ALREADY_EXISTS)
    (ok (map-set freelancer-profiles tx-sender {
      name: name,
      skills: skills,
      hourly-rate: hourly-rate,
      rating: u0,
      completed-projects: u0
    }))
  )
)

(define-public (create-client-profile (name (string-ascii 50)) (company (string-ascii 100)))
  (begin
    (asserts! (is-none (map-get? client-profiles tx-sender)) ERR_ALREADY_EXISTS)
    (ok (map-set client-profiles tx-sender {
      name: name,
      company: company,
      rating: u0,
      total-projects: u0
    }))
  )
)

(define-public (create-project (title (string-ascii 100)) (description (string-ascii 500)) (total-amount uint) (deadline uint))
  (let
    (
      (project-id (+ (var-get project-counter) u1))
    )
    (begin
      (var-set project-counter project-id)
      (map-set projects project-id {
        client: tx-sender,
        freelancer: none,
        title: title,
        description: description,
        total-amount: total-amount,
        status: "open",
        created-at: stacks-block-height,
        deadline: deadline
      })
      (map-set escrow-balances project-id u0)
      (ok project-id)
    )
  )
)

(define-public (apply-to-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    (begin
      (asserts! (is-eq (get status project) "open") ERR_INVALID_STATUS)
      (asserts! (is-some (map-get? freelancer-profiles tx-sender)) ERR_NOT_AUTHORIZED)
      (ok (map-set projects project-id (merge project {
        freelancer: (some tx-sender),
        status: "assigned"
      })))
    )
  )
)

(define-public (fund-escrow (project-id uint) (amount uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (current-balance (default-to u0 (map-get? escrow-balances project-id)))
    )
    (begin
      (asserts! (is-eq (get client project) tx-sender) ERR_NOT_AUTHORIZED)
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (map-set escrow-balances project-id (+ current-balance amount))
      (ok true)
    )
  )
)

(define-public (create-milestone (project-id uint) (description (string-ascii 200)) (amount uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (milestone-id (+ (var-get milestone-counter) u1))
      (current-milestones (default-to (list) (map-get? project-milestones project-id)))
    )
    (begin
      (asserts! (is-eq (get client project) tx-sender) ERR_NOT_AUTHORIZED)
      (var-set milestone-counter milestone-id)
      (map-set milestones milestone-id {
        project-id: project-id,
        description: description,
        amount: amount,
        status: "pending",
        created-at: stacks-block-height,
        completed-at: none
      })
      (map-set project-milestones project-id (unwrap! (as-max-len? (append current-milestones milestone-id) u20) ERR_INVALID_MILESTONE))
      (ok milestone-id)
    )
  )
)

(define-public (submit-milestone (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones milestone-id) ERR_MILESTONE_NOT_FOUND))
      (project (unwrap! (map-get? projects (get project-id milestone)) ERR_PROJECT_NOT_FOUND))
    )
    (begin
      (asserts! (is-eq (some tx-sender) (get freelancer project)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status milestone) "pending") ERR_INVALID_STATUS)
      (ok (map-set milestones milestone-id (merge milestone {
        status: "submitted",
        completed-at: (some stacks-block-height)
      })))
    )
  )
)

(define-public (approve-milestone (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones milestone-id) ERR_MILESTONE_NOT_FOUND))
      (project (unwrap! (map-get? projects (get project-id milestone)) ERR_PROJECT_NOT_FOUND))
      (escrow-balance (default-to u0 (map-get? escrow-balances (get project-id milestone))))
    )
    (begin
      (asserts! (is-eq (get client project) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status milestone) "submitted") ERR_INVALID_STATUS)
      (asserts! (>= escrow-balance (get amount milestone)) ERR_INSUFFICIENT_FUNDS)
      (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (unwrap! (get freelancer project) ERR_NOT_AUTHORIZED))))
      (map-set escrow-balances (get project-id milestone) (- escrow-balance (get amount milestone)))
      (map-set milestones milestone-id (merge milestone {
        status: "approved"
      }))
      (ok true)
    )
  )
)

(define-public (create-dispute (project-id uint) (milestone-id uint) (reason (string-ascii 300)))
  (let
    (
      (dispute-id (+ (var-get dispute-counter) u1))
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (milestone (unwrap! (map-get? milestones milestone-id) ERR_MILESTONE_NOT_FOUND))
    )
    (begin
      (asserts! (or (is-eq (get client project) tx-sender) (is-eq (some tx-sender) (get freelancer project))) ERR_NOT_AUTHORIZED)
      (var-set dispute-counter dispute-id)
      (map-set disputes dispute-id {
        project-id: project-id,
        milestone-id: milestone-id,
        initiator: tx-sender,
        reason: reason,
        status: "open",
        created-at: stacks-block-height,
        resolved-at: none
      })
      (ok dispute-id)
    )
  )
)

(define-public (resolve-dispute (dispute-id uint) (winner (string-ascii 10)))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_EXISTS))
      (milestone (unwrap! (map-get? milestones (get milestone-id dispute)) ERR_MILESTONE_NOT_FOUND))
      (project (unwrap! (map-get? projects (get project-id dispute)) ERR_PROJECT_NOT_FOUND))
      (escrow-balance (default-to u0 (map-get? escrow-balances (get project-id dispute))))
    )
    (begin
      (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status dispute) "open") ERR_INVALID_STATUS)
      (if (is-eq winner "freelancer")
        (begin
          (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (unwrap! (get freelancer project) ERR_NOT_AUTHORIZED))))
          (map-set escrow-balances (get project-id dispute) (- escrow-balance (get amount milestone)))
        )
        (begin
          (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (get client project))))
          (map-set escrow-balances (get project-id dispute) (- escrow-balance (get amount milestone)))
        )
      )
      (map-set disputes dispute-id (merge dispute {
        status: "resolved",
        resolved-at: (some stacks-block-height)
      }))
      (ok true)
    )
  )
)

(define-public (complete-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    (begin
      (asserts! (is-eq (get client project) tx-sender) ERR_NOT_AUTHORIZED)
      (map-set projects project-id (merge project {
        status: "completed"
      }))
      (ok true)
    )
  )
)

(define-read-only (get-project (project-id uint))
  (map-get? projects project-id)
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestones milestone-id)
)

(define-read-only (get-project-milestones (project-id uint))
  (map-get? project-milestones project-id)
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes dispute-id)
)

(define-read-only (get-escrow-balance (project-id uint))
  (map-get? escrow-balances project-id)
)

(define-read-only (get-freelancer-profile (freelancer principal))
  (map-get? freelancer-profiles freelancer)
)

(define-read-only (get-client-profile (client principal))
  (map-get? client-profiles client)
)

(define-read-only (get-project-counter)
  (var-get project-counter)
)

(define-read-only (get-milestone-counter)
  (var-get milestone-counter)
)

(define-read-only (get-dispute-counter)
  (var-get dispute-counter)
)

(define-map freelancer-performance
  principal
  {
    total-milestones: uint,
    on-time-deliveries: uint,
    average-satisfaction: uint,
    total-ratings: uint,
    performance-score: uint
  }
)

(define-map milestone-performance
  uint
  {
    submitted-at: uint,
    approved-at: uint,
    deadline: uint,
    was-on-time: bool,
    client-rating: (optional uint)
  }
)

(define-public (track-milestone-performance (milestone-id uint) (deadline uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones milestone-id) ERR_MILESTONE_NOT_FOUND))
      (project (unwrap! (map-get? projects (get project-id milestone)) ERR_PROJECT_NOT_FOUND))
    )
    (begin
      (asserts! (is-eq (get client project) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status milestone) "pending") ERR_INVALID_STATUS)
      (map-set milestone-performance milestone-id {
        submitted-at: u0,
        approved-at: u0,
        deadline: deadline,
        was-on-time: false,
        client-rating: none
      })
      (ok true)
    )
  )
)

(define-public (submit-performance-rating (milestone-id uint) (rating uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones milestone-id) ERR_MILESTONE_NOT_FOUND))
      (project (unwrap! (map-get? projects (get project-id milestone)) ERR_PROJECT_NOT_FOUND))
      (performance (unwrap! (map-get? milestone-performance milestone-id) ERR_MILESTONE_NOT_FOUND))
      (freelancer-addr (unwrap! (get freelancer project) ERR_NOT_AUTHORIZED))
      (current-perf (default-to {total-milestones: u0, on-time-deliveries: u0, average-satisfaction: u0, total-ratings: u0, performance-score: u0} (map-get? freelancer-performance freelancer-addr)))
    )
    (begin
      (asserts! (is-eq (get client project) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status milestone) "approved") ERR_INVALID_STATUS)
      (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_STATUS) 
      (asserts! (is-none (get client-rating performance)) ERR_ALREADY_EXISTS)
      (asserts! (map-set milestone-performance milestone-id (merge performance {client-rating: (some rating)})) (err u999))
      (update-freelancer-performance freelancer-addr rating)
    )
  )
)

(define-private (update-freelancer-performance (freelancer-addr principal) (new-rating uint))
  (let
    (
      (current-perf (default-to {total-milestones: u0, on-time-deliveries: u0, average-satisfaction: u0, total-ratings: u0, performance-score: u0} (map-get? freelancer-performance freelancer-addr)))
      (new-total-ratings (+ (get total-ratings current-perf) u1))
      (new-avg-satisfaction (/ (+ (* (get average-satisfaction current-perf) (get total-ratings current-perf)) new-rating) new-total-ratings))
      (new-performance-score (calculate-performance-score (get on-time-deliveries current-perf) (get total-milestones current-perf) new-avg-satisfaction))
    )
    (begin
      (map-set freelancer-performance freelancer-addr {
        total-milestones: (get total-milestones current-perf),
        on-time-deliveries: (get on-time-deliveries current-perf),
        average-satisfaction: new-avg-satisfaction,
        total-ratings: new-total-ratings,
        performance-score: new-performance-score
      })
      (ok true)
    )
  )
)

(define-private (calculate-performance-score (on-time uint) (total uint) (satisfaction uint))
  (if (> total u0)
    (/ (+ (* (/ (* on-time u100) total) u60) (* satisfaction u40)) u100)
    u0
  )
)

(define-public (update-milestone-completion (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones milestone-id) ERR_MILESTONE_NOT_FOUND))
      (project (unwrap! (map-get? projects (get project-id milestone)) ERR_PROJECT_NOT_FOUND))
      (performance (unwrap! (map-get? milestone-performance milestone-id) ERR_MILESTONE_NOT_FOUND))
      (freelancer-addr (unwrap! (get freelancer project) ERR_NOT_AUTHORIZED))
      (current-perf (default-to {total-milestones: u0, on-time-deliveries: u0, average-satisfaction: u0, total-ratings: u0, performance-score: u0} (map-get? freelancer-performance freelancer-addr)))
      (completion-time (unwrap! (get completed-at milestone) ERR_INVALID_STATUS))
      (was-on-time (<= completion-time (get deadline performance)))
    )
    (begin
      (asserts! (is-eq (some tx-sender) (get freelancer project)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status milestone) "approved") ERR_INVALID_STATUS)
      (map-set milestone-performance milestone-id (merge performance {
        submitted-at: completion-time,
        approved-at: stacks-block-height,
        was-on-time: was-on-time
      }))
      (map-set freelancer-performance freelancer-addr {
        total-milestones: (+ (get total-milestones current-perf) u1),
        on-time-deliveries: (+ (get on-time-deliveries current-perf) (if was-on-time u1 u0)),
        average-satisfaction: (get average-satisfaction current-perf),
        total-ratings: (get total-ratings current-perf),
        performance-score: (calculate-performance-score (+ (get on-time-deliveries current-perf) (if was-on-time u1 u0)) (+ (get total-milestones current-perf) u1) (get average-satisfaction current-perf))
      })
      (ok true)
    )
  )
)

(define-read-only (get-freelancer-performance (freelancer-addr principal))
  (map-get? freelancer-performance freelancer-addr)
)

(define-read-only (get-milestone-performance (milestone-id uint))
  (map-get? milestone-performance milestone-id)
)

(define-read-only (get-performance-metrics (freelancer-addr principal))
  (let
    (
      (performance (map-get? freelancer-performance freelancer-addr))
    )
    (match performance
      perf (some {
        on-time-percentage: (if (> (get total-milestones perf) u0) (/ (* (get on-time-deliveries perf) u100) (get total-milestones perf)) u0),
        average-rating: (get average-satisfaction perf),
        total-projects: (get total-milestones perf),
        overall-score: (get performance-score perf)
      })
      none
    )
  )
)
