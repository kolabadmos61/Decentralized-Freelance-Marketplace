# 🚀 Decentralized Freelance Marketplace

A blockchain-based freelance marketplace built on Stacks that enables secure project management through milestone-based escrow contracts.

## 📋 Overview

This smart contract creates a trustless environment where clients can hire freelancers and pay them through secure escrow contracts that release funds based on completed task milestones. The platform includes dispute resolution mechanisms and profile management for both clients and freelancers.

## ✨ Features

- 👤 **Profile Management**: Separate profiles for freelancers and clients
- 📝 **Project Creation**: Clients can create detailed project listings
- 🎯 **Milestone System**: Break projects into manageable milestones with specific payments
- 🔒 **Escrow Protection**: Secure fund holding until milestone completion
- ⚖️ **Dispute Resolution**: Built-in arbitration system for conflicts
- 💰 **Automatic Payments**: Funds released upon milestone approval

## 🛠️ Core Functions

### Profile Management
- `create-freelancer-profile` - Create a freelancer profile with skills and rates
- `create-client-profile` - Create a client profile with company information

### Project Management
- `create-project` - Create a new project listing
- `apply-to-project` - Freelancers can apply to open projects
- `fund-escrow` - Clients fund the project escrow
- `complete-project` - Mark project as completed

### Milestone System
- `create-milestone` - Create project milestones with specific amounts
- `submit-milestone` - Freelancers submit completed work
- `approve-milestone` - Clients approve and release milestone payments

### Dispute Resolution
- `create-dispute` - Initiate a dispute for problematic milestones
- `resolve-dispute` - Contract owner resolves disputes

## 📖 Usage Instructions

### 1. Setting Up Profiles

**For Freelancers:**
```clarity
(contract-call? .freelance-marketplace create-freelancer-profile 
  "John Developer" 
  "JavaScript, React, Node.js" 
  u50)
```

**For Clients:**
```clarity
(contract-call? .freelance-marketplace create-client-profile 
  "Jane Smith" 
  "Tech Startup Inc")
```

### 2. Creating a Project

```clarity
(contract-call? .freelance-marketplace create-project 
  "Build E-commerce Website" 
  "Need a full-stack e-commerce solution with payment integration" 
  u1000000 
  u1000)
```

### 3. Funding Escrow

```clarity
(contract-call? .freelance-marketplace fund-escrow u1 u1000000)
```

### 4. Creating Milestones

````clarity
