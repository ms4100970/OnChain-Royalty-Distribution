;; Advanced Licensing System for Digital Rights Management
;; Enables artists to monetize works through differentiated usage licenses

;; License type constants
(define-constant LICENSE-PERSONAL u1)    ;; Personal use only
(define-constant LICENSE-COMMERCIAL u2)  ;; Commercial usage rights
(define-constant LICENSE-EXCLUSIVE u3)   ;; Exclusive rights with restrictions
(define-constant LICENSE-UNLIMITED u4)   ;; Unlimited usage rights

;; Error constants
(define-constant err-unauthorized (err u200))
(define-constant err-not-found (err u201))
(define-constant err-invalid-license-type (err u202))
(define-constant err-license-expired (err u203))
(define-constant err-usage-exceeded (err u204))
(define-constant err-not-transferable (err u205))
(define-constant err-invalid-duration (err u206))
(define-constant err-template-inactive (err u207))
(define-constant err-license-inactive (err u208))
(define-constant err-invalid-price (err u209))

;; Data variables
(define-data-var last-template-id uint u0)
(define-data-var last-license-id uint u0)
(define-data-var last-usage-id uint u0)

;; License template definitions - created by artists
(define-map license-templates
  { template-id: uint }
  {
    work-id: uint,
    artist-address: principal,
    license-type: uint,
    price: uint,
    duration-blocks: uint,    ;; 0 means unlimited duration
    max-uses: uint,          ;; 0 means unlimited uses
    transferable: bool,
    royalty-percentage: uint, ;; Additional royalty for license sales
    active: bool
  }
)

;; Purchased licenses - owned by licensees
(define-map licenses
  { license-id: uint }
  {
    template-id: uint,
    licensee: principal,
    purchase-block: uint,
    expiry-block: uint,      ;; 0 means no expiry
    uses-remaining: uint,    ;; 0 means unlimited
    active: bool
  }
)

;; License usage tracking
(define-map license-usage
  { license-id: uint, usage-id: uint }
  {
    used-by: principal,
    usage-block: uint,
    usage-description: (string-ascii 128)
  }
)

;; License transfer history
(define-map license-transfers
  { transfer-id: uint }
  {
    license-id: uint,
    from-address: principal,
    to-address: principal,
    transfer-block: uint,
    transfer-price: uint
  }
)

(define-data-var last-transfer-id uint u0)

;; Read-only functions
(define-read-only (get-license-template (template-id uint))
  (map-get? license-templates { template-id: template-id })
)

(define-read-only (get-license (license-id uint))
  (map-get? licenses { license-id: license-id })
)

(define-read-only (get-license-usage (license-id uint) (usage-id uint))
  (map-get? license-usage { license-id: license-id, usage-id: usage-id })
)

(define-read-only (is-license-valid (license-id uint))
  (let
    (
      (license (unwrap! (map-get? licenses { license-id: license-id }) (ok false)))
      (current-block stacks-block-height)
    )
    (ok (and
      (get active license)
      (or (is-eq (get expiry-block license) u0) (< current-block (get expiry-block license)))
      (or (is-eq (get uses-remaining license) u0) (> (get uses-remaining license) u0))
    ))
  )
)

(define-read-only (can-use-license (license-id uint) (user principal))
  (let
    (
      (license (unwrap! (map-get? licenses { license-id: license-id }) (ok false)))
    )
    (ok (and
      (unwrap-panic (is-license-valid license-id))
      (is-eq (get licensee license) user)
    ))
  )
)

;; Create license template - only by work artist
(define-public (create-license-template 
    (work-id uint) 
    (license-type uint) 
    (price uint) 
    (duration-blocks uint) 
    (max-uses uint) 
    (transferable bool)
    (royalty-percentage uint))
  (let
    (
      (new-template-id (+ (var-get last-template-id) u1))
    )
    ;; Validate license type
    (asserts! (and (>= license-type LICENSE-PERSONAL) (<= license-type LICENSE-UNLIMITED)) err-invalid-license-type)
    ;; Validate price
    (asserts! (> price u0) err-invalid-price)
    ;; Validate royalty percentage
    (asserts! (<= royalty-percentage u100) err-invalid-price)
    
    ;; Store template
    (var-set last-template-id new-template-id)
    (map-set license-templates
      { template-id: new-template-id }
      {
        work-id: work-id,
        artist-address: tx-sender,
        license-type: license-type,
        price: price,
        duration-blocks: duration-blocks,
        max-uses: max-uses,
        transferable: transferable,
        royalty-percentage: royalty-percentage,
        active: true
      }
    )
    (ok new-template-id)
  )
)

;; Purchase license from template
(define-public (purchase-license (template-id uint))
  (let
    (
      (template (unwrap! (map-get? license-templates { template-id: template-id }) err-not-found))
      (new-license-id (+ (var-get last-license-id) u1))
      (expiry-block (if (> (get duration-blocks template) u0) 
                       (+ stacks-block-height (get duration-blocks template)) 
                       u0))
      (license-price (get price template))
      (royalty-amount (/ (* license-price (get royalty-percentage template)) u100))
      (artist-amount (- license-price royalty-amount))
    )
    ;; Validate template is active
    (asserts! (get active template) err-template-inactive)
    
    ;; Process payment
    (try! (stx-transfer? artist-amount tx-sender (get artist-address template)))
    
    ;; Handle royalty distribution if applicable
    (if (> royalty-amount u0)
      (try! (stx-transfer? royalty-amount tx-sender (get artist-address template)))
      true
    )
    
    ;; Create license
    (var-set last-license-id new-license-id)
    (map-set licenses
      { license-id: new-license-id }
      {
        template-id: template-id,
        licensee: tx-sender,
        purchase-block: stacks-block-height,
        expiry-block: expiry-block,
        uses-remaining: (get max-uses template),
        active: true
      }
    )
    (ok new-license-id)
  )
)

;; Record license usage
(define-public (use-license (license-id uint) (usage-description (string-ascii 128)))
  (let
    (
      (license (unwrap! (map-get? licenses { license-id: license-id }) err-not-found))
      (new-usage-id (+ (var-get last-usage-id) u1))
      (updated-uses (if (> (get uses-remaining license) u0) 
                       (- (get uses-remaining license) u1) 
                       u0))
    )
    ;; Validate license ownership and validity
    (asserts! (is-eq (get licensee license) tx-sender) err-unauthorized)
    (asserts! (unwrap-panic (is-license-valid license-id)) err-license-expired)
    
    ;; Check usage limits
    (if (> (get uses-remaining license) u0)
      (asserts! (> (get uses-remaining license) u0) err-usage-exceeded)
      true
    )
    
    ;; Record usage
    (var-set last-usage-id new-usage-id)
    (map-set license-usage
      { license-id: license-id, usage-id: new-usage-id }
      {
        used-by: tx-sender,
        usage-block: stacks-block-height,
        usage-description: usage-description
      }
    )
    
    ;; Update license usage count
    (map-set licenses
      { license-id: license-id }
      (merge license { uses-remaining: updated-uses })
    )
    
    (ok new-usage-id)
  )
)

;; Transfer license to another user
(define-public (transfer-license (license-id uint) (recipient principal) (transfer-price uint))
  (let
    (
      (license (unwrap! (map-get? licenses { license-id: license-id }) err-not-found))
      (template (unwrap! (map-get? license-templates { template-id: (get template-id license) }) err-not-found))
      (new-transfer-id (+ (var-get last-transfer-id) u1))
    )
    ;; Validate ownership and transferability
    (asserts! (is-eq (get licensee license) tx-sender) err-unauthorized)
    (asserts! (get transferable template) err-not-transferable)
    (asserts! (unwrap-panic (is-license-valid license-id)) err-license-inactive)
    
    ;; Process transfer payment if applicable
    (if (> transfer-price u0)
      (try! (stx-transfer? transfer-price recipient tx-sender))
      true
    )
    
    ;; Record transfer
    (var-set last-transfer-id new-transfer-id)
    (map-set license-transfers
      { transfer-id: new-transfer-id }
      {
        license-id: license-id,
        from-address: tx-sender,
        to-address: recipient,
        transfer-block: stacks-block-height,
        transfer-price: transfer-price
      }
    )
    
    ;; Update license ownership
    (map-set licenses
      { license-id: license-id }
      (merge license { licensee: recipient })
    )
    
    (ok new-transfer-id)
  )
)

;; Deactivate license template
(define-public (deactivate-template (template-id uint))
  (let
    (
      (template (unwrap! (map-get? license-templates { template-id: template-id }) err-not-found))
    )
    ;; Only template creator can deactivate
    (asserts! (is-eq (get artist-address template) tx-sender) err-unauthorized)
    
    (map-set license-templates
      { template-id: template-id }
      (merge template { active: false })
    )
    (ok true)
  )
)

;; Revoke license - only by artist in special circumstances
(define-public (revoke-license (license-id uint))
  (let
    (
      (license (unwrap! (map-get? licenses { license-id: license-id }) err-not-found))
      (template (unwrap! (map-get? license-templates { template-id: (get template-id license) }) err-not-found))
    )
    ;; Only original artist can revoke
    (asserts! (is-eq (get artist-address template) tx-sender) err-unauthorized)
    
    (map-set licenses
      { license-id: license-id }
      (merge license { active: false })
    )
    (ok true)
  )
)

;; Get license statistics for a work
(define-read-only (get-work-license-stats (work-id uint))
  (ok {
    total-templates: (var-get last-template-id),
    total-licenses: (var-get last-license-id),
    total-usage-records: (var-get last-usage-id)
  })
)


