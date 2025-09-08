;; RaffleTemplates.clar
;; A contract for creating and managing reusable raffle templates
;; Enables quick raffle creation with pre-configured settings

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_TEMPLATE_NOT_FOUND (err u201))
(define-constant ERR_TEMPLATE_ALREADY_EXISTS (err u202))
(define-constant ERR_INVALID_TEMPLATE_DATA (err u203))
(define-constant ERR_INVALID_DURATION (err u204))
(define-constant ERR_INVALID_PARTICIPANTS (err u205))
(define-constant ERR_INVALID_FEE (err u206))
(define-constant ERR_TEMPLATE_NAME_TOO_LONG (err u207))
(define-constant ERR_TEMPLATE_DESCRIPTION_TOO_LONG (err u208))
(define-constant ERR_INSUFFICIENT_PERMISSIONS (err u209))

;; Data Variables
(define-data-var template-counter uint u0)

;; Data Maps
(define-map templates
  uint
  {
    creator: principal,
    name: (string-ascii 50),
    title-template: (string-ascii 100),
    description-template: (string-ascii 500),
    default-entry-fee: uint,
    default-duration: uint,
    default-max-participants: uint,
    is-public: bool,
    created-block: uint,
    usage-count: uint,
    is-active: bool
  }
)

;; Map to track user's templates by name for quick lookup
(define-map user-templates
  { creator: principal, template-name: (string-ascii 50) }
  uint
)

;; Map to store template usage history
(define-map template-usage
  { template-id: uint, user: principal }
  {
    usage-count: uint,
    last-used-block: uint,
    total-raffles-created: uint
  }
)

;; Map to track template categories for better organization
(define-map template-categories
  uint
  (string-ascii 20)
)

;; Public Functions

;; Create a new raffle template
(define-public (create-template 
  (name (string-ascii 50))
  (title-template (string-ascii 100))
  (description-template (string-ascii 500))
  (default-entry-fee uint)
  (default-duration uint)
  (default-max-participants uint)
  (is-public bool)
  (category (string-ascii 20))
  )
  (let
    (
      (template-id (+ (var-get template-counter) u1))
      (current-block stacks-block-height)
    )
    ;; Input validation
    (asserts! (> (len name) u0) ERR_TEMPLATE_NAME_TOO_LONG)
    (asserts! (<= (len name) u50) ERR_TEMPLATE_NAME_TOO_LONG)
    (asserts! (> (len title-template) u0) ERR_INVALID_TEMPLATE_DATA)
    (asserts! (<= (len title-template) u100) ERR_INVALID_TEMPLATE_DATA)
    (asserts! (> (len description-template) u0) ERR_TEMPLATE_DESCRIPTION_TOO_LONG)
    (asserts! (<= (len description-template) u500) ERR_TEMPLATE_DESCRIPTION_TOO_LONG)
    (asserts! (> default-duration u0) ERR_INVALID_DURATION)
    (asserts! (> default-max-participants u0) ERR_INVALID_PARTICIPANTS)
    (asserts! (>= default-entry-fee u0) ERR_INVALID_FEE)
    
    ;; Check if template with this name already exists for this user
    (asserts! (is-none (map-get? user-templates { creator: tx-sender, template-name: name })) ERR_TEMPLATE_ALREADY_EXISTS)
    
    ;; Store the template
    (map-set templates template-id
      {
        creator: tx-sender,
        name: name,
        title-template: title-template,
        description-template: description-template,
        default-entry-fee: default-entry-fee,
        default-duration: default-duration,
        default-max-participants: default-max-participants,
        is-public: is-public,
        created-block: current-block,
        usage-count: u0,
        is-active: true
      }
    )
    
    ;; Store user template mapping
    (map-set user-templates { creator: tx-sender, template-name: name } template-id)
    
    ;; Store category if provided
    (if (> (len category) u0)
      (map-set template-categories template-id category)
      true
    )
    
    ;; Update counter
    (var-set template-counter template-id)
    
    (ok template-id)
  )
)

;; Create a raffle from a template with optional parameter overrides
(define-public (create-raffle-from-template 
  (template-id uint)
  (title-suffix (optional (string-ascii 50)))
  (custom-description (optional (string-ascii 500)))
  (entry-fee-override (optional uint))
  (duration-override (optional uint))
  (max-participants-override (optional uint))
  )
  (let
    (
      (template (unwrap! (map-get? templates template-id) ERR_TEMPLATE_NOT_FOUND))
      (current-block stacks-block-height)
      (usage-data (default-to { usage-count: u0, last-used-block: u0, total-raffles-created: u0 } 
                   (map-get? template-usage { template-id: template-id, user: tx-sender })))
    )
    ;; Check if template is active
    (asserts! (get is-active template) ERR_TEMPLATE_NOT_FOUND)
    
    ;; Check permissions for private templates
    (asserts! (or (get is-public template) (is-eq tx-sender (get creator template))) ERR_INSUFFICIENT_PERMISSIONS)
    
    ;; Build the final raffle parameters
    (let
      (
        (base-title (get title-template template))
        (final-title (match title-suffix
          suffix (if (<= (+ (len base-title) (len suffix) u1) u100)
                   (concat base-title (concat " " suffix))
                   base-title)
          base-title
        ))
        (final-description (default-to (get description-template template) custom-description))
        (final-entry-fee (default-to (get default-entry-fee template) entry-fee-override))
        (final-duration (default-to (get default-duration template) duration-override))
        (final-max-participants (default-to (get default-max-participants template) max-participants-override))
      )
      ;; Validate final parameters
      (asserts! (> final-duration u0) ERR_INVALID_DURATION)
      (asserts! (> final-max-participants u0) ERR_INVALID_PARTICIPANTS)
      (asserts! (>= final-entry-fee u0) ERR_INVALID_FEE)
      
      ;; Update template usage statistics
      (map-set templates template-id
        (merge template { usage-count: (+ (get usage-count template) u1) })
      )
      
      ;; Update user usage statistics
      (map-set template-usage { template-id: template-id, user: tx-sender }
        {
          usage-count: (+ (get usage-count usage-data) u1),
          last-used-block: current-block,
          total-raffles-created: (+ (get total-raffles-created usage-data) u1)
        }
      )
      
      ;; Call the main Rafflehub contract to create the actual raffle
      (as-contract (contract-call? .Rafflehub create-raffle 
        (unwrap-panic (as-max-len? final-title u100))
        final-description 
        final-entry-fee 
        final-duration 
        final-max-participants))
    )
  )
)

;; Update an existing template (only by creator)
(define-public (update-template
  (template-id uint)
  (new-title-template (optional (string-ascii 100)))
  (new-description-template (optional (string-ascii 500)))
  (new-default-entry-fee (optional uint))
  (new-default-duration (optional uint))
  (new-default-max-participants (optional uint))
  (new-is-public (optional bool))
  )
  (let
    (
      (template (unwrap! (map-get? templates template-id) ERR_TEMPLATE_NOT_FOUND))
    )
    ;; Only template creator can update
    (asserts! (is-eq tx-sender (get creator template)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active template) ERR_TEMPLATE_NOT_FOUND)
    
    ;; Update template with new values
    (map-set templates template-id
      (merge template
        {
          title-template: (default-to (get title-template template) new-title-template),
          description-template: (default-to (get description-template template) new-description-template),
          default-entry-fee: (default-to (get default-entry-fee template) new-default-entry-fee),
          default-duration: (default-to (get default-duration template) new-default-duration),
          default-max-participants: (default-to (get default-max-participants template) new-default-max-participants),
          is-public: (default-to (get is-public template) new-is-public)
        }
      )
    )
    
    (ok true)
  )
)

;; Deactivate a template (soft delete)
(define-public (deactivate-template (template-id uint))
  (let
    (
      (template (unwrap! (map-get? templates template-id) ERR_TEMPLATE_NOT_FOUND))
    )
    ;; Only template creator can deactivate
    (asserts! (is-eq tx-sender (get creator template)) ERR_NOT_AUTHORIZED)
    
    (map-set templates template-id
      (merge template { is-active: false })
    )
    
    (ok true)
  )
)

;; Read-only functions

;; Get template details
(define-read-only (get-template (template-id uint))
  (map-get? templates template-id)
)

;; Get template by creator and name
(define-read-only (get-template-by-name (creator principal) (template-name (string-ascii 50)))
  (match (map-get? user-templates { creator: creator, template-name: template-name })
    template-id (map-get? templates template-id)
    none
  )
)

;; Get template usage statistics
(define-read-only (get-template-usage (template-id uint) (user principal))
  (map-get? template-usage { template-id: template-id, user: user })
)

;; Get template category
(define-read-only (get-template-category (template-id uint))
  (map-get? template-categories template-id)
)

;; Get current template counter
(define-read-only (get-template-counter)
  (var-get template-counter)
)

;; Check if user can use template
(define-read-only (can-use-template (template-id uint) (user principal))
  (match (map-get? templates template-id)
    template (and (get is-active template) 
                  (or (get is-public template) (is-eq user (get creator template))))
    false
  )
)

;; Get template summary for listing
(define-read-only (get-template-summary (template-id uint))
  (match (map-get? templates template-id)
    template
    (some {
      id: template-id,
      creator: (get creator template),
      name: (get name template),
      category: (default-to "" (map-get? template-categories template-id)),
      usage-count: (get usage-count template),
      is-public: (get is-public template),
      is-active: (get is-active template)
    })
    none
  )
)
