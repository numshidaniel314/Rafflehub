# 🎲 Rafflehub - Decentralized Digital Raffles

A smart contract platform for creating and managing verifiable digital raffles on the Stacks blockchain. Create transparent, trustless giveaways with automatic winner selection and prize distribution.

## ✨ Features

- 🎯 **Create Custom Raffles** - Set entry fees, duration, and participant limits
- 💰 **Automatic Prize Distribution** - Winners receive prizes automatically
- 🔒 **Transparent & Verifiable** - All raffle data stored on-chain
- 🎲 **Provably Fair** - Random winner selection using block data
- 💸 **Refund System** - Automatic refunds for cancelled raffles
- 🏆 **Platform Fee Management** - Configurable platform fees

## 🚀 Quick Start

### Creating a Raffle

```clarity
(contract-call? .Rafflehub create-raffle 
  "My Awesome Raffle" 
  "Win amazing prizes!" 
  u1000000  ;; 1 STX entry fee
  u144      ;; ~24 hours duration
  u100      ;; max 100 participants
)
```

### Entering a Raffle

```clarity
(contract-call? .Rafflehub enter-raffle u1)
```

### Selecting a Winner

```clarity
(contract-call? .Rafflehub select-winner u1)
```

## 📋 Contract Functions

### Public Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `create-raffle` | Create a new raffle | title, description, entry-fee, duration, max-participants |
| `enter-raffle` | Enter an existing raffle | raffle-id |
| `select-winner` | Select winner after raffle ends | raffle-id |
| `cancel-raffle` | Cancel raffle (creator only) | raffle-id |
| `set-platform-fee` | Update platform fee (owner only) | new-fee |

### Read-Only Functions

| Function | Description | Returns |
|----------|-------------|---------|
| `get-raffle` | Get raffle details | Raffle data |
| `get-raffle-participant` | Get participant info | Participant data |
| `has-entered-raffle` | Check if user entered | Boolean |
| `get-current-raffle-id` | Get latest raffle ID | Number |
| `is-raffle-active` | Check if raffle is active | Boolean |
| `get-raffle-status` | Get raffle status | String |
| `get-blocks-until-end` | Blocks remaining | Number |

## 🎮 Usage Examples

### 1. Free Raffle
```clarity
(contract-call? .Rafflehub create-raffle 
  "Free NFT Giveaway" 
  "Enter for a chance to win a rare NFT!" 
  u0        ;; Free entry
  u1008     ;; ~1 week
  u1000     ;; 1000 max participants
)
```

### 2. Paid Entry Raffle
```clarity
(contract-call? .Rafflehub create-raffle 
  "STX Prize Pool" 
  "Winner takes 95% of the pool!" 
  u500000   ;; 0.5 STX entry
  u144      ;; ~24 hours
  u50       ;; 50 max participants
)
```

### 3. Check Raffle Status
```clarity
(contract-call? .Rafflehub get-raffle-status u1)
;; Returns: "active", "ended", "completed", "cancelled", or "not-found"
```

## 🔧 Error Codes

| Code | Error | Description |
|------|-------|-------------|
| u100 | `ERR_NOT_AUTHORIZED` | Not authorized to perform action |
| u101 | `ERR_RAFFLE_NOT_FOUND` | Raffle doesn't exist |
| u102 | `ERR_RAFFLE_ENDED` | Raffle has ended |
| u103 | `ERR_RAFFLE_NOT_ENDED` | Raffle hasn't ended yet |
| u104 | `ERR_ALREADY_ENTERED` | Already entered this raffle |
| u105 | `ERR_INSUFFICIENT_PAYMENT` | Insufficient payment |
| u106 | `ERR_NO_PARTICIPANTS` | No participants in raffle |
| u107 | `ERR_WINNER_ALREADY_SELECTED` | Winner already selected |

