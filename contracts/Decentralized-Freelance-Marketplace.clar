(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROJECT_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_ALREADY_EXISTS (err u104))
(define-constant ERR_MILESTONE_NOT_FOUND (err u105))
(define-constant ERR_INVALID_MILESTONE (err u106))
(define-constant ERR_DISPUTE_EXISTS (err u107))

(define-constant ERR_AUTO_RELEASE_NOT_READY (err u108))
(define-constant DEFAULT_AUTO_RELEASE_BLOCKS u1008)

(define-data-var project-counter uint u0)
(define-data-var milestone-counter uint u0)
(define-data-var dispute-counter uint u0)

(define-data-var portfolio-counter uint u0)
(define-data-var endorsement-counter uint u0)

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

(define-map portfolio-items
  uint
  {
    freelancer: principal,
    project-id: uint,
    title: (string-ascii 80),
    description: (string-ascii 200),
    category: (string-ascii 30),
    client-testimonial: (string-ascii 300),
    created-at: uint
  }
)

(define-map freelancer-portfolios
  principal
  (list 10 uint)
)

(define-map skill-endorsements
  uint
  {
    freelancer: principal,
    endorser: principal,
    skill: (string-ascii 40),
    project-id: uint,
    strength: uint,
    created-at: uint
  }
)

(define-map freelancer-endorsements
  principal
  (list 15 uint)
)

(define-public (create-portfolio-item (project-id uint) (title (string-ascii 80)) (description (string-ascii 200)) (category (string-ascii 30)) (testimonial (string-ascii 300)))
  (let
    (
      (portfolio-id (+ (var-get portfolio-counter) u1))
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (current-items (default-to (list) (map-get? freelancer-portfolios tx-sender)))
    )
    (begin
      (asserts! (is-eq (some tx-sender) (get freelancer project)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status project) "completed") ERR_INVALID_STATUS)
      (var-set portfolio-counter portfolio-id)
      (map-set portfolio-items portfolio-id {
        freelancer: tx-sender,
        project-id: project-id,
        title: title,
        description: description,
        category: category,
        client-testimonial: testimonial,
        created-at: stacks-block-height
      })
      (map-set freelancer-portfolios tx-sender (unwrap! (as-max-len? (append current-items portfolio-id) u10) ERR_INVALID_STATUS))
      (ok portfolio-id)
    )
  )
)

(define-public (endorse-skill (freelancer-addr principal) (project-id uint) (skill (string-ascii 40)) (strength uint))
  (let
    (
      (endorsement-id (+ (var-get endorsement-counter) u1))
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (current-endorsements (default-to (list) (map-get? freelancer-endorsements freelancer-addr)))
    )
    (begin
      (asserts! (is-eq (get client project) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (some freelancer-addr) (get freelancer project)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status project) "completed") ERR_INVALID_STATUS)
      (asserts! (and (>= strength u1) (<= strength u5)) ERR_INVALID_STATUS)
      (var-set endorsement-counter endorsement-id)
      (map-set skill-endorsements endorsement-id {
        freelancer: freelancer-addr,
        endorser: tx-sender,
        skill: skill,
        project-id: project-id,
        strength: strength,
        created-at: stacks-block-height
      })
      (map-set freelancer-endorsements freelancer-addr (unwrap! (as-max-len? (append current-endorsements endorsement-id) u15) ERR_INVALID_STATUS))
      (ok endorsement-id)
    )
  )
)

(define-read-only (get-freelancer-portfolio (freelancer-addr principal))
  (map-get? freelancer-portfolios freelancer-addr)
)

(define-read-only (get-portfolio-item (portfolio-id uint))
  (map-get? portfolio-items portfolio-id)
)

(define-read-only (get-freelancer-endorsements (freelancer-addr principal))
  (map-get? freelancer-endorsements freelancer-addr)
)

(define-read-only (get-skill-endorsement (endorsement-id uint))
  (map-get? skill-endorsements endorsement-id)
)


(define-map milestone-auto-release
  uint
  {
    auto-release-at: uint,
    is-auto-released: bool
  }
)

(define-public (set-milestone-auto-release (milestone-id uint) (release-blocks uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones milestone-id) ERR_MILESTONE_NOT_FOUND))
      (project (unwrap! (map-get? projects (get project-id milestone)) ERR_PROJECT_NOT_FOUND))
      (release-block (+ stacks-block-height release-blocks))
    )
    (begin
      (asserts! (is-eq (get client project) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status milestone) "pending") ERR_INVALID_STATUS)
      (map-set milestone-auto-release milestone-id {
        auto-release-at: release-block,
        is-auto-released: false
      })
      (ok release-block)
    )
  )
)

(define-public (execute-auto-release (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones milestone-id) ERR_MILESTONE_NOT_FOUND))
      (project (unwrap! (map-get? projects (get project-id milestone)) ERR_PROJECT_NOT_FOUND))
      (auto-release-info (unwrap! (map-get? milestone-auto-release milestone-id) ERR_MILESTONE_NOT_FOUND))
      (escrow-balance (default-to u0 (map-get? escrow-balances (get project-id milestone))))
    )
    (begin
      (asserts! (is-eq (get status milestone) "submitted") ERR_INVALID_STATUS)
      (asserts! (>= stacks-block-height (get auto-release-at auto-release-info)) ERR_AUTO_RELEASE_NOT_READY)
      (asserts! (is-eq (get is-auto-released auto-release-info) false) ERR_ALREADY_EXISTS)
      (asserts! (>= escrow-balance (get amount milestone)) ERR_INSUFFICIENT_FUNDS)
      (asserts! (is-none (map-get? disputes milestone-id)) ERR_DISPUTE_EXISTS)
      (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (unwrap! (get freelancer project) ERR_NOT_AUTHORIZED))))
      (map-set escrow-balances (get project-id milestone) (- escrow-balance (get amount milestone)))
      (map-set milestones milestone-id (merge milestone { status: "auto-released" }))
      (map-set milestone-auto-release milestone-id (merge auto-release-info { is-auto-released: true }))
      (ok true)
    )
  )
)

(define-read-only (get-milestone-auto-release (milestone-id uint))
  (map-get? milestone-auto-release milestone-id)
)

(define-read-only (is-auto-release-ready (milestone-id uint))
  (match (map-get? milestone-auto-release milestone-id)
    auto-info (>= stacks-block-height (get auto-release-at auto-info))
    false
  )
)


(define-constant ERR_INSUFFICIENT_RESERVE (err u109))

(define-map milestone-reserves
  uint
  {
    reserved-amount: uint,
    is-released: bool,
    reserved-at: uint
  }
)

(define-map project-reserved-totals
  uint
  uint
)

(define-public (create-milestone-with-reserve (project-id uint) (description (string-ascii 200)) (amount uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (milestone-id (+ (var-get milestone-counter) u1))
      (current-milestones (default-to (list) (map-get? project-milestones project-id)))
      (escrow-balance (default-to u0 (map-get? escrow-balances project-id)))
      (total-reserved (default-to u0 (map-get? project-reserved-totals project-id)))
      (available-balance (- escrow-balance total-reserved))
    )
    (begin
      (asserts! (is-eq (get client project) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (>= available-balance amount) ERR_INSUFFICIENT_RESERVE)
      (var-set milestone-counter milestone-id)
      (map-set milestones milestone-id {
        project-id: project-id,
        description: description,
        amount: amount,
        status: "pending",
        created-at: stacks-block-height,
        completed-at: none
      })
      (map-set milestone-reserves milestone-id {
        reserved-amount: amount,
        is-released: false,
        reserved-at: stacks-block-height
      })
      (map-set project-reserved-totals project-id (+ total-reserved amount))
      (map-set project-milestones project-id (unwrap! (as-max-len? (append current-milestones milestone-id) u20) ERR_INVALID_MILESTONE))
      (ok milestone-id)
    )
  )
)

(define-public (release-milestone-reserve (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones milestone-id) ERR_MILESTONE_NOT_FOUND))
      (reserve-info (unwrap! (map-get? milestone-reserves milestone-id) ERR_MILESTONE_NOT_FOUND))
      (project-id (get project-id milestone))
      (total-reserved (default-to u0 (map-get? project-reserved-totals project-id)))
    )
    (begin
      (asserts! (is-eq (get status milestone) "approved") ERR_INVALID_STATUS)
      (asserts! (is-eq (get is-released reserve-info) false) ERR_ALREADY_EXISTS)
      (map-set milestone-reserves milestone-id (merge reserve-info {is-released: true}))
      (map-set project-reserved-totals project-id (- total-reserved (get reserved-amount reserve-info)))
      (ok true)
    )
  )
)

(define-read-only (get-milestone-reserve (milestone-id uint))
  (map-get? milestone-reserves milestone-id)
)

(define-read-only (get-available-escrow-balance (project-id uint))
  (let
    (
      (escrow-balance (default-to u0 (map-get? escrow-balances project-id)))
      (total-reserved (default-to u0 (map-get? project-reserved-totals project-id)))
    )
    (some (- escrow-balance total-reserved))
  )
)