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
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u110))
(define-constant ERR_SUBSCRIPTION_INACTIVE (err u111))
(define-constant ERR_INSUFFICIENT_BALANCE (err u112))
(define-constant ERR_INVALID_FREQUENCY (err u113))
(define-constant ERR_SUBSCRIPTION_ALREADY_EXISTS (err u114))
(define-constant ERR_INVALID_SUBSCRIPTION_DURATION (err u115))
(define-constant ERR_SUBSCRIPTION_EXPIRED (err u116))
(define-constant ERR_SUBSCRIPTION_SERIES_NOT_FOUND (err u117))
(define-constant ERR_INVALID_SERIES_PARAMETERS (err u118))
(define-constant ERR_SERIES_ALREADY_ACTIVE (err u119))
(define-constant ERR_SERIES_NOT_ACTIVE (err u120))
(define-constant ERR_INVALID_PRIZE_STRUCTURE (err u121))
(define-constant ERR_PRIZE_PERCENTAGES_INVALID (err u122))
(define-constant ERR_TOO_MANY_TIERS (err u123))
(define-constant ERR_TIER_NOT_FOUND (err u124))
(define-constant ERR_PRIZES_ALREADY_DISTRIBUTED (err u125))
(define-constant ERR_INSUFFICIENT_PARTICIPANTS_FOR_TIERS (err u126))

(define-data-var raffle-counter uint u0)
(define-data-var platform-fee-percentage uint u5)
(define-data-var subscription-counter uint u0)
(define-data-var series-counter uint u0)

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

(define-map subscription-series
  uint
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    entry-fee: uint,
    raffle-duration: uint,
    raffle-frequency: uint,
    max-participants: uint,
    series-start-block: uint,
    series-end-block: uint,
    current-raffle-id: (optional uint),
    next-raffle-block: uint,
    total-raffles: uint,
    completed-raffles: uint,
    is-active: bool,
    auto-create-enabled: bool
  }
)

(define-map subscriptions
  { series-id: uint, subscriber: principal }
  {
    subscription-id: uint,
    start-block: uint,
    end-block: uint,
    is-active: bool,
    deposit-balance: uint,
    auto-renew: bool,
    failed-entries: uint,
    total-entries: uint,
    last-entry-block: uint
  }
)

(define-map subscription-deposits
  { subscription-id: uint }
  { balance: uint, last-deposit-block: uint }
)

(define-map series-subscribers
  { series-id: uint, index: uint }
  principal
)

(define-map series-subscriber-count
  uint
  uint
)

;; Multi-tier prize system maps
(define-map raffle-prize-structure
  uint
  {
    total-tiers: uint,
    prizes-distributed: bool,
    total-prize-pool: uint,
    platform-fee-deducted: uint
  }
)

(define-map prize-tiers
  { raffle-id: uint, tier: uint }
  {
    percentage: uint,
    prize-amount: uint,
    winner: (optional principal),
    is-claimed: bool
  }
)

(define-map raffle-winners
  { raffle-id: uint, position: uint }
  principal
)

(define-map winner-positions
  { raffle-id: uint, winner: principal }
  uint
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

(define-public (create-subscription-series (title (string-ascii 100)) (description (string-ascii 500)) (entry-fee uint) (raffle-duration uint) (raffle-frequency uint) (max-participants uint) (series-duration uint))
  (let
    (
      (series-id (+ (var-get series-counter) u1))
      (series-start-block stacks-block-height)
      (series-end-block (+ stacks-block-height series-duration))
      (next-raffle-block (+ stacks-block-height raffle-frequency))
    )
    (asserts! (> raffle-duration u0) ERR_INVALID_DURATION)
    (asserts! (> raffle-frequency u0) ERR_INVALID_FREQUENCY)
    (asserts! (> max-participants u0) ERR_INVALID_SERIES_PARAMETERS)
    (asserts! (> series-duration raffle-frequency) ERR_INVALID_SUBSCRIPTION_DURATION)
    (asserts! (>= entry-fee u0) ERR_INVALID_ENTRY_FEE)
    
    (map-set subscription-series series-id
      {
        creator: tx-sender,
        title: title,
        description: description,
        entry-fee: entry-fee,
        raffle-duration: raffle-duration,
        raffle-frequency: raffle-frequency,
        max-participants: max-participants,
        series-start-block: series-start-block,
        series-end-block: series-end-block,
        current-raffle-id: none,
        next-raffle-block: next-raffle-block,
        total-raffles: u0,
        completed-raffles: u0,
        is-active: true,
        auto-create-enabled: true
      }
    )
    (map-set series-subscriber-count series-id u0)
    (var-set series-counter series-id)
    (ok series-id)
  )
)

(define-public (subscribe-to-series (series-id uint) (subscription-duration uint) (deposit-amount uint) (auto-renew bool))
  (let
    (
      (series (unwrap! (map-get? subscription-series series-id) ERR_SUBSCRIPTION_SERIES_NOT_FOUND))
      (subscription-id (+ (var-get subscription-counter) u1))
      (current-block stacks-block-height)
      (subscription-end-block (+ current-block subscription-duration))
      (subscriber-count (default-to u0 (map-get? series-subscriber-count series-id)))
    )
    (asserts! (get is-active series) ERR_SERIES_NOT_ACTIVE)
    (asserts! (< current-block (get series-end-block series)) ERR_SUBSCRIPTION_EXPIRED)
    (asserts! (> subscription-duration u0) ERR_INVALID_SUBSCRIPTION_DURATION)
    (asserts! (is-none (map-get? subscriptions { series-id: series-id, subscriber: tx-sender })) ERR_SUBSCRIPTION_ALREADY_EXISTS)
    
    (if (> deposit-amount u0)
      (try! (stx-transfer? deposit-amount tx-sender (as-contract tx-sender)))
      true
    )
    
    (map-set subscriptions { series-id: series-id, subscriber: tx-sender }
      {
        subscription-id: subscription-id,
        start-block: current-block,
        end-block: subscription-end-block,
        is-active: true,
        deposit-balance: deposit-amount,
        auto-renew: auto-renew,
        failed-entries: u0,
        total-entries: u0,
        last-entry-block: u0
      }
    )
    
    (map-set subscription-deposits { subscription-id: subscription-id }
      { balance: deposit-amount, last-deposit-block: current-block }
    )
    
    (map-set series-subscribers { series-id: series-id, index: subscriber-count } tx-sender)
    (map-set series-subscriber-count series-id (+ subscriber-count u1))
    (var-set subscription-counter subscription-id)
    (ok subscription-id)
  )
)

(define-public (create-next-raffle-in-series (series-id uint))
  (let
    (
      (series (unwrap! (map-get? subscription-series series-id) ERR_SUBSCRIPTION_SERIES_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (get is-active series) ERR_SERIES_NOT_ACTIVE)
    (asserts! (get auto-create-enabled series) ERR_SERIES_NOT_ACTIVE)
    (asserts! (>= current-block (get next-raffle-block series)) ERR_RAFFLE_NOT_ENDED)
    (asserts! (< current-block (get series-end-block series)) ERR_SUBSCRIPTION_EXPIRED)
    
    (let
      (
        (raffle-id (+ (var-get raffle-counter) u1))
        (raffle-end-block (+ current-block (get raffle-duration series)))
        (next-raffle-block (+ current-block (get raffle-frequency series)))
      )
      (map-set raffles raffle-id
        {
          creator: (get creator series),
          title: (get title series),
          description: (get description series),
          entry-fee: (get entry-fee series),
          end-block: raffle-end-block,
          max-participants: (get max-participants series),
          participant-count: u0,
          winner: none,
          prize-pool: u0,
          is-active: true
        }
      )
      
      (map-set subscription-series series-id
        (merge series {
          current-raffle-id: (some raffle-id),
          next-raffle-block: next-raffle-block,
          total-raffles: (+ (get total-raffles series) u1)
        })
      )
      
      (var-set raffle-counter raffle-id)
      (ok raffle-id)
    )
  )
)

(define-public (auto-enter-subscribers (series-id uint) (start-index uint) (end-index uint))
  (let
    (
      (series (unwrap! (map-get? subscription-series series-id) ERR_SUBSCRIPTION_SERIES_NOT_FOUND))
      (current-raffle-id (unwrap! (get current-raffle-id series) ERR_RAFFLE_NOT_FOUND))
      (current-block stacks-block-height)
      (entry-fee (get entry-fee series))
    )
    (asserts! (get is-active series) ERR_SERIES_NOT_ACTIVE)
    (asserts! (< current-block (get series-end-block series)) ERR_SUBSCRIPTION_EXPIRED)
    
    (fold auto-enter-single-subscriber
      (list start-index (+ start-index u1) (+ start-index u2) (+ start-index u3) (+ start-index u4))
      { series-id: series-id, raffle-id: current-raffle-id, entry-fee: entry-fee, current-block: current-block }
    )
    (ok true)
  )
)

(define-private (auto-enter-single-subscriber (index uint) (data { series-id: uint, raffle-id: uint, entry-fee: uint, current-block: uint }))
  (let
    (
      (series-id (get series-id data))
      (raffle-id (get raffle-id data))
      (entry-fee (get entry-fee data))
      (current-block (get current-block data))
      (subscriber-principal (map-get? series-subscribers { series-id: series-id, index: index }))
    )
    (match subscriber-principal
      subscriber
      (let
        (
          (subscription (map-get? subscriptions { series-id: series-id, subscriber: subscriber }))
        )
        (match subscription
          sub-data
          (if (and (get is-active sub-data) (>= (get deposit-balance sub-data) entry-fee))
            (let
              (
                (raffle (unwrap-panic (map-get? raffles raffle-id)))
                (participant-count (get participant-count raffle))
              )
              (if (and (< participant-count (get max-participants raffle)) 
                       (is-none (map-get? participant-entries { raffle-id: raffle-id, participant: subscriber })))
                (begin
                  (map-set participant-entries { raffle-id: raffle-id, participant: subscriber } true)
                  (map-set raffle-participants 
                    { raffle-id: raffle-id, participant: subscriber }
                    { entry-block: current-block, entry-index: participant-count }
                  )
                  (map-set raffle-participant-list
                    { raffle-id: raffle-id, index: participant-count }
                    subscriber
                  )
                  (map-set raffles raffle-id
                    (merge raffle {
                      participant-count: (+ participant-count u1),
                      prize-pool: (+ (get prize-pool raffle) entry-fee)
                    })
                  )
                  (map-set subscriptions { series-id: series-id, subscriber: subscriber }
                    (merge sub-data {
                      deposit-balance: (- (get deposit-balance sub-data) entry-fee),
                      total-entries: (+ (get total-entries sub-data) u1),
                      last-entry-block: current-block
                    })
                  )
                  data
                )
                data
              )
            )
            data
          )
          data
        )
      )
      data
    )
  )
)

(define-public (deposit-to-subscription (series-id uint) (amount uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { series-id: series-id, subscriber: tx-sender }) ERR_SUBSCRIPTION_NOT_FOUND))
      (subscription-id (get subscription-id subscription))
      (current-block stacks-block-height)
    )
    (asserts! (get is-active subscription) ERR_SUBSCRIPTION_INACTIVE)
    (asserts! (> amount u0) ERR_INVALID_ENTRY_FEE)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set subscriptions { series-id: series-id, subscriber: tx-sender }
      (merge subscription {
        deposit-balance: (+ (get deposit-balance subscription) amount)
      })
    )
    
    (map-set subscription-deposits { subscription-id: subscription-id }
      { balance: (+ (get deposit-balance subscription) amount), last-deposit-block: current-block }
    )
    
    (ok true)
  )
)

(define-public (cancel-subscription (series-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { series-id: series-id, subscriber: tx-sender }) ERR_SUBSCRIPTION_NOT_FOUND))
      (remaining-balance (get deposit-balance subscription))
    )
    (asserts! (get is-active subscription) ERR_SUBSCRIPTION_INACTIVE)
    
    (map-set subscriptions { series-id: series-id, subscriber: tx-sender }
      (merge subscription {
        is-active: false,
        deposit-balance: u0
      })
    )
    
    (if (> remaining-balance u0)
      (try! (as-contract (stx-transfer? remaining-balance tx-sender tx-sender)))
      true
    )
    
    (ok remaining-balance)
  )
)

(define-public (deactivate-series (series-id uint))
  (let
    (
      (series (unwrap! (map-get? subscription-series series-id) ERR_SUBSCRIPTION_SERIES_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator series)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active series) ERR_SERIES_NOT_ACTIVE)
    
    (map-set subscription-series series-id
      (merge series {
        is-active: false,
        auto-create-enabled: false
      })
    )
    
    (ok true)
  )
)

(define-read-only (get-subscription-series (series-id uint))
  (map-get? subscription-series series-id)
)

(define-read-only (get-subscription (series-id uint) (subscriber principal))
  (map-get? subscriptions { series-id: series-id, subscriber: subscriber })
)

(define-read-only (get-subscription-deposit (subscription-id uint))
  (map-get? subscription-deposits { subscription-id: subscription-id })
)

(define-read-only (get-series-subscriber-count (series-id uint))
  (default-to u0 (map-get? series-subscriber-count series-id))
)

(define-read-only (get-series-subscriber-by-index (series-id uint) (index uint))
  (map-get? series-subscribers { series-id: series-id, index: index })
)

(define-read-only (get-current-series-id)
  (var-get series-counter)
)

(define-read-only (get-current-subscription-id)
  (var-get subscription-counter)
)

(define-read-only (is-series-active (series-id uint))
  (match (map-get? subscription-series series-id)
    series (and (get is-active series) (< stacks-block-height (get series-end-block series)))
    false
  )
)

(define-read-only (can-create-next-raffle (series-id uint))
  (match (map-get? subscription-series series-id)
    series (and (get is-active series) 
                (get auto-create-enabled series)
                (>= stacks-block-height (get next-raffle-block series))
                (< stacks-block-height (get series-end-block series)))
    false
  )
)

;; Create a raffle with multi-tier prize structure
(define-public (create-multi-tier-raffle (title (string-ascii 100)) (description (string-ascii 500)) (entry-fee uint) (duration uint) (max-participants uint) (tier-percentages (list 10 uint)))
  (let
    (
      (raffle-id (+ (var-get raffle-counter) u1))
      (end-block (+ stacks-block-height duration))
      (total-tiers (len tier-percentages))
      (percentage-sum (fold + tier-percentages u0))
    )
    ;; Validate inputs
    (asserts! (> duration u0) ERR_INVALID_DURATION)
    (asserts! (> max-participants u0) ERR_INVALID_DURATION)
    (asserts! (and (> total-tiers u0) (<= total-tiers u10)) ERR_TOO_MANY_TIERS)
    (asserts! (is-eq percentage-sum u100) ERR_PRIZE_PERCENTAGES_INVALID)
    (asserts! (>= entry-fee u0) ERR_INVALID_ENTRY_FEE)
    
    ;; Create the base raffle
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
    
    ;; Initialize prize structure
    (map-set raffle-prize-structure raffle-id
      {
        total-tiers: total-tiers,
        prizes-distributed: false,
        total-prize-pool: u0,
        platform-fee-deducted: u0
      }
    )
    
    ;; Set up prize tiers manually for first 3 tiers
    (if (> total-tiers u0)
      (let ((tier-0-percentage (default-to u0 (element-at tier-percentages u0))))
        (map-set prize-tiers { raffle-id: raffle-id, tier: u0 }
          { percentage: tier-0-percentage, prize-amount: u0, winner: none, is-claimed: false }))
      true)
    
    (if (> total-tiers u1)
      (let ((tier-1-percentage (default-to u0 (element-at tier-percentages u1))))
        (map-set prize-tiers { raffle-id: raffle-id, tier: u1 }
          { percentage: tier-1-percentage, prize-amount: u0, winner: none, is-claimed: false }))
      true)
    
    (if (> total-tiers u2)
      (let ((tier-2-percentage (default-to u0 (element-at tier-percentages u2))))
        (map-set prize-tiers { raffle-id: raffle-id, tier: u2 }
          { percentage: tier-2-percentage, prize-amount: u0, winner: none, is-claimed: false }))
      true)
    
    (var-set raffle-counter raffle-id)
    (ok raffle-id)
  )
)

;; Distribute prizes to multiple winners
(define-public (distribute-multi-tier-prizes (raffle-id uint))
  (let
    (
      (raffle (unwrap! (map-get? raffles raffle-id) ERR_RAFFLE_NOT_FOUND))
      (prize-structure (unwrap! (map-get? raffle-prize-structure raffle-id) ERR_INVALID_PRIZE_STRUCTURE))
      (current-block stacks-block-height)
      (participant-count (get participant-count raffle))
      (total-tiers (get total-tiers prize-structure))
    )
    ;; Validate conditions
    (asserts! (>= current-block (get end-block raffle)) ERR_RAFFLE_NOT_ENDED)
    (asserts! (> participant-count u0) ERR_NO_PARTICIPANTS)
    (asserts! (>= participant-count total-tiers) ERR_INSUFFICIENT_PARTICIPANTS_FOR_TIERS)
    (asserts! (not (get prizes-distributed prize-structure)) ERR_PRIZES_ALREADY_DISTRIBUTED)
    
    ;; Calculate platform fee and net prize pool
    (let
      (
        (total-prize-pool (get prize-pool raffle))
        (platform-fee (/ (* total-prize-pool (var-get platform-fee-percentage)) u100))
        (net-prize-pool (- total-prize-pool platform-fee))
      )
      ;; Transfer platform fee
      (if (> platform-fee u0)
        (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
        true
      )
      
      ;; Select winners and distribute prizes
      (try! (select-and-distribute-winners raffle-id net-prize-pool total-tiers participant-count))
      
      ;; Update prize structure
      (map-set raffle-prize-structure raffle-id
        (merge prize-structure {
          prizes-distributed: true,
          total-prize-pool: net-prize-pool,
          platform-fee-deducted: platform-fee
        })
      )
      
      ;; Mark raffle as inactive
      (map-set raffles raffle-id
        (merge raffle { is-active: false })
      )
      
      (ok true)
    )
  )
)

;; Select winners and distribute prizes using simple approach
(define-private (select-and-distribute-winners (raffle-id uint) (net-prize-pool uint) (total-tiers uint) (participant-count uint))
  (let
    (
      (random-base (mod (+ stacks-block-height raffle-id) participant-count))
    )
    (distribute-prizes-iteratively raffle-id net-prize-pool total-tiers participant-count random-base)
  )
)

;; Distribute prizes to winners iteratively
(define-private (distribute-prizes-iteratively (raffle-id uint) (net-prize-pool uint) (total-tiers uint) (participant-count uint) (random-base uint))
  (let
    (
      (tier-0-winner-index (mod random-base participant-count))
      (tier-1-winner-index (mod (+ random-base u7) participant-count))
      (tier-2-winner-index (mod (+ random-base u17) participant-count))
    )
    (if (> total-tiers u0)
      (try! (distribute-single-prize raffle-id net-prize-pool u0 tier-0-winner-index))
      true
    )
    (if (> total-tiers u1)
      (try! (distribute-single-prize raffle-id net-prize-pool u1 (if (is-eq tier-1-winner-index tier-0-winner-index) (mod (+ tier-1-winner-index u1) participant-count) tier-1-winner-index)))
      true
    )
    (if (> total-tiers u2)
      (try! (distribute-single-prize raffle-id net-prize-pool u2 (get-unique-index tier-2-winner-index (list tier-0-winner-index tier-1-winner-index) participant-count)))
      true
    )
    (ok true)
  )
)

;; Get unique index that doesn't conflict with existing winners
(define-private (get-unique-index (candidate-index uint) (used-indices (list 10 uint)) (participant-count uint))
  (if (is-none (index-of used-indices candidate-index))
    candidate-index
    (mod (+ candidate-index u1) participant-count)
  )
)

;; Distribute prize for a single tier
(define-private (distribute-single-prize (raffle-id uint) (net-prize-pool uint) (tier uint) (winner-index uint))
  (let
    (
      (winner-principal (unwrap! (map-get? raffle-participant-list { raffle-id: raffle-id, index: winner-index }) ERR_NO_PARTICIPANTS))
      (tier-info (unwrap! (map-get? prize-tiers { raffle-id: raffle-id, tier: tier }) ERR_TIER_NOT_FOUND))
      (prize-amount (/ (* net-prize-pool (get percentage tier-info)) u100))
    )
    ;; Record winner
    (map-set raffle-winners { raffle-id: raffle-id, position: tier } winner-principal)
    (map-set winner-positions { raffle-id: raffle-id, winner: winner-principal } tier)
    
    ;; Update tier info with prize amount and winner
    (map-set prize-tiers { raffle-id: raffle-id, tier: tier }
      (merge tier-info {
        prize-amount: prize-amount,
        winner: (some winner-principal),
        is-claimed: false
      })
    )
    
    ;; Transfer prize to winner
    (if (> prize-amount u0)
      (try! (as-contract (stx-transfer? prize-amount tx-sender winner-principal)))
      true
    )
    
    (ok true)
  )
)

;; Claim prize for a specific tier (alternative to automatic distribution)
(define-public (claim-tier-prize (raffle-id uint) (tier uint))
  (let
    (
      (tier-info (unwrap! (map-get? prize-tiers { raffle-id: raffle-id, tier: tier }) ERR_TIER_NOT_FOUND))
      (winner (unwrap! (get winner tier-info) ERR_WINNER_ALREADY_SELECTED))
    )
    (asserts! (is-eq tx-sender winner) ERR_NOT_AUTHORIZED)
    (asserts! (not (get is-claimed tier-info)) ERR_PRIZES_ALREADY_DISTRIBUTED)
    (asserts! (> (get prize-amount tier-info) u0) ERR_INSUFFICIENT_PAYMENT)
    
    ;; Mark as claimed
    (map-set prize-tiers { raffle-id: raffle-id, tier: tier }
      (merge tier-info { is-claimed: true })
    )
    
    ;; Transfer prize
    (try! (as-contract (stx-transfer? (get prize-amount tier-info) tx-sender winner)))
    
    (ok (get prize-amount tier-info))
  )
)

;; Read-only functions for multi-tier system
(define-read-only (get-raffle-prize-structure (raffle-id uint))
  (map-get? raffle-prize-structure raffle-id)
)

(define-read-only (get-prize-tier-info (raffle-id uint) (tier uint))
  (map-get? prize-tiers { raffle-id: raffle-id, tier: tier })
)

(define-read-only (get-raffle-winner-by-position (raffle-id uint) (position uint))
  (map-get? raffle-winners { raffle-id: raffle-id, position: position })
)

(define-read-only (get-winner-position (raffle-id uint) (winner principal))
  (map-get? winner-positions { raffle-id: raffle-id, winner: winner })
)

(define-read-only (get-total-prize-tiers (raffle-id uint))
  (match (map-get? raffle-prize-structure raffle-id)
    structure (some (get total-tiers structure))
    none
  )
)

(define-read-only (are-prizes-distributed (raffle-id uint))
  (match (map-get? raffle-prize-structure raffle-id)
    structure (get prizes-distributed structure)
    false
  )
)


