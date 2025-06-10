(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_RAFFLE_NOT_FOUND (err u101))
(define-constant ERR_RAFFLE_ENDED (err u102))
(define-constant ERR_RAFFLE_NOT_ENDED (err u103))
(define-constant ERR_ALREADY_ENTERED (err u104))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u105))
(define-constant ERR_NO_PARTICIPANTS (err u106))
(define-constant ERR_WINNER_ALREADY_SELECTED (err u107))
(define-constant ERR_INVALID_DURATION (err u108))
(define-constant ERR_INVALID_ENTRY_FEE (err u109))

(define-data-var raffle-counter uint u0)
(define-data-var platform-fee-percentage uint u5)

(define-map raffles
  uint
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    entry-fee: uint,
    end-block: uint,
    max-participants: uint,
    participant-count: uint,
    winner: (optional principal),
    prize-pool: uint,
    is-active: bool
  }
)

(define-map raffle-participants
  { raffle-id: uint, participant: principal }
  { entry-block: uint, entry-index: uint }
)

(define-map participant-entries
  { raffle-id: uint, participant: principal }
  bool
)

(define-map raffle-participant-list
  { raffle-id: uint, index: uint }
  principal
)

(define-public (create-raffle (title (string-ascii 100)) (description (string-ascii 500)) (entry-fee uint) (duration uint) (max-participants uint))
  (let
    (
      (raffle-id (+ (var-get raffle-counter) u1))
      (end-block (+ stacks-block-height duration))
    )
    (asserts! (> duration u0) ERR_INVALID_DURATION)
    (asserts! (> max-participants u0) ERR_INVALID_DURATION)
    (map-set raffles raffle-id
      {
        creator: tx-sender,
        title: title,
        description: description,
        entry-fee: entry-fee,
        end-block: end-block,
        max-participants: max-participants,
        participant-count: u0,
        winner: none,
        prize-pool: u0,
        is-active: true
      }
    )
    (var-set raffle-counter raffle-id)
    (ok raffle-id)
  )
)

(define-public (enter-raffle (raffle-id uint))
  (let
    (
      (raffle (unwrap! (map-get? raffles raffle-id) ERR_RAFFLE_NOT_FOUND))
      (current-block stacks-block-height)
      (participant-count (get participant-count raffle))
    )
    (asserts! (get is-active raffle) ERR_RAFFLE_ENDED)
    (asserts! (< current-block (get end-block raffle)) ERR_RAFFLE_ENDED)
    (asserts! (< participant-count (get max-participants raffle)) ERR_RAFFLE_ENDED)
    (asserts! (is-none (map-get? participant-entries { raffle-id: raffle-id, participant: tx-sender })) ERR_ALREADY_ENTERED)
    
    (if (> (get entry-fee raffle) u0)
      (try! (stx-transfer? (get entry-fee raffle) tx-sender (as-contract tx-sender)))
      true
    )
    
    (map-set participant-entries { raffle-id: raffle-id, participant: tx-sender } true)
    (map-set raffle-participants 
      { raffle-id: raffle-id, participant: tx-sender }
      { entry-block: current-block, entry-index: participant-count }
    )
    (map-set raffle-participant-list
      { raffle-id: raffle-id, index: participant-count }
      tx-sender
    )
    (map-set raffles raffle-id
      (merge raffle {
        participant-count: (+ participant-count u1),
        prize-pool: (+ (get prize-pool raffle) (get entry-fee raffle))
      })
    )
    (ok true)
  )
)

(define-public (select-winner (raffle-id uint))
  (let
    (
      (raffle (unwrap! (map-get? raffles raffle-id) ERR_RAFFLE_NOT_FOUND))
      (current-block stacks-block-height)
      (participant-count (get participant-count raffle))
    )
    (asserts! (>= current-block (get end-block raffle)) ERR_RAFFLE_NOT_ENDED)
    (asserts! (> participant-count u0) ERR_NO_PARTICIPANTS)
    (asserts! (is-none (get winner raffle)) ERR_WINNER_ALREADY_SELECTED)
    
    (let
      (
       
        (winner (unwrap! (map-get? raffle-participant-list { raffle-id: raffle-id, index: u1}) ERR_NO_PARTICIPANTS))
        (platform-fee (/ (* (get prize-pool raffle) (var-get platform-fee-percentage)) u100))
        (winner-prize (- (get prize-pool raffle) platform-fee))
      )
      (map-set raffles raffle-id
        (merge raffle {
          winner: (some winner),
          is-active: false
        })
      )
      
      (if (> winner-prize u0)
        (try! (as-contract (stx-transfer? winner-prize tx-sender winner)))
        true
      )
      
      (if (> platform-fee u0)
        (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
        true
      )
      
      (ok winner)
    )
  )
)



(define-private (refund-entry (index uint) (previous (response bool uint)))
  (let
    (
      (participant (map-get? raffle-participant-list { raffle-id: u1, index: index }))
    )
    (match participant
      participant-addr (as-contract (stx-transfer? u100 tx-sender participant-addr))
      previous
    )
  )
)

(define-read-only (get-raffle (raffle-id uint))
  (map-get? raffles raffle-id)
)

(define-read-only (get-raffle-participant (raffle-id uint) (participant principal))
  (map-get? raffle-participants { raffle-id: raffle-id, participant: participant })
)

(define-read-only (has-entered-raffle (raffle-id uint) (participant principal))
  (is-some (map-get? participant-entries { raffle-id: raffle-id, participant: participant }))
)

(define-read-only (get-participant-by-index (raffle-id uint) (index uint))
  (map-get? raffle-participant-list { raffle-id: raffle-id, index: index })
)

(define-read-only (get-current-raffle-id)
  (var-get raffle-counter)
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

(define-read-only (is-raffle-active (raffle-id uint))
  (match (map-get? raffles raffle-id)
    raffle (and (get is-active raffle) (< stacks-block-height (get end-block raffle)))
    false
  )
)

(define-read-only (get-raffle-status (raffle-id uint))
  (match (map-get? raffles raffle-id)
    raffle
    (if (is-some (get winner raffle))
      "completed"
      (if (get is-active raffle)
        (if (< stacks-block-height (get end-block raffle))
          "active"
          "ended"
        )
        "cancelled"
      )
    )
    "not-found"
  )
)
