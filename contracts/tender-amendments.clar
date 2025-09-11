;; Tender Amendment System
;; Enables controlled modifications to active tenders with transparency and fairness

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u400))
(define-constant err-amendment-not-found (err u401))
(define-constant err-invalid-tender-status (err u402))
(define-constant err-amendment-window-closed (err u403))
(define-constant err-too-many-amendments (err u404))
(define-constant err-invalid-amendment-type (err u405))
(define-constant err-grace-period-active (err u406))
(define-constant err-already-withdrawn (err u407))

(define-data-var next-amendment-id uint u1)
(define-data-var amendment-window-blocks uint u1440) ;; 10 days in blocks  
(define-data-var grace-period-blocks uint u288) ;; 2 days for bid withdrawal
(define-data-var max-amendments-per-tender uint u5)

;; Amendment tracking for each tender
(define-map tender-amendments
  { tender-id: uint }
  { 
    amendment-count: uint,
    last-amendment-block: uint,
    total-notifications-sent: uint
  }
)

;; Individual amendment records
(define-map amendments
  { amendment-id: uint }
  {
    tender-id: uint,
    amendment-type: (string-ascii 30),
    original-value: (string-utf8 200),
    new-value: (string-utf8 200),
    reason: (string-utf8 300),
    proposed-by: principal,
    proposed-at: uint,
    status: (string-ascii 20),
    approved-at: (optional uint),
    effective-at: (optional uint),
    impact-level: (string-ascii 20)
  }
)

;; Bidder notification tracking
(define-map amendment-notifications
  { amendment-id: uint, bidder: principal }
  {
    notified-at: uint,
    acknowledged: bool,
    acknowledged-at: (optional uint),
    withdrawal-eligible: bool
  }
)

;; Bid withdrawal tracking due to amendments
(define-map amendment-withdrawals
  { tender-id: uint, bidder: principal }
  {
    withdrawal-reason: (string-utf8 200),
    withdrawn-at: uint,
    amendment-id: uint,
    refund-processed: bool
  }
)

;; Amendment approval workflow
(define-map amendment-approvals
  { amendment-id: uint }
  {
    approver-count: uint,
    required-approvers: uint,
    approved-by: (list 10 principal),
    approval-threshold: uint
  }
)

;; Read-only functions
(define-read-only (get-amendment (amendment-id uint))
  (map-get? amendments { amendment-id: amendment-id }))

(define-read-only (get-tender-amendments (tender-id uint))
  (default-to 
    { amendment-count: u0, last-amendment-block: u0, total-notifications-sent: u0 }
    (map-get? tender-amendments { tender-id: tender-id })))

(define-read-only (get-amendment-notification (amendment-id uint) (bidder principal))
  (map-get? amendment-notifications { amendment-id: amendment-id, bidder: bidder }))

(define-read-only (get-withdrawal-info (tender-id uint) (bidder principal))
  (map-get? amendment-withdrawals { tender-id: tender-id, bidder: bidder }))

(define-read-only (is-amendment-window-open (tender-id uint))
  (let ((tender-data (unwrap! (contract-call? .procurement-system get-tender tender-id) false)))
    (and 
      (is-eq (get status tender-data) "open")
      (> (+ (get created-at tender-data) (var-get amendment-window-blocks)) stacks-block-height))))

;; Propose amendment to an active tender
(define-public (propose-amendment 
  (tender-id uint) 
  (amendment-type (string-ascii 30)) 
  (original-value (string-utf8 200)) 
  (new-value (string-utf8 200)) 
  (reason (string-utf8 300))
  (impact-level (string-ascii 20)))
  (let (
    (amendment-id (var-get next-amendment-id))
    (tender-data (unwrap! (contract-call? .procurement-system get-tender tender-id) err-amendment-not-found))
    (current-amendments (get-tender-amendments tender-id))
  )
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (is-eq (get status tender-data) "open") err-invalid-tender-status)
    (asserts! (is-amendment-window-open tender-id) err-amendment-window-closed)
    (asserts! (< (get amendment-count current-amendments) (var-get max-amendments-per-tender)) err-too-many-amendments)
    
    ;; Record amendment proposal
    (map-set amendments
      { amendment-id: amendment-id }
      {
        tender-id: tender-id,
        amendment-type: amendment-type,
        original-value: original-value,
        new-value: new-value,
        reason: reason,
        proposed-by: tx-sender,
        proposed-at: stacks-block-height,
        status: "proposed",
        approved-at: none,
        effective-at: none,
        impact-level: impact-level
      })
    
    ;; Update tender amendment tracking
    (map-set tender-amendments
      { tender-id: tender-id }
      {
        amendment-count: (+ (get amendment-count current-amendments) u1),
        last-amendment-block: stacks-block-height,
        total-notifications-sent: (get total-notifications-sent current-amendments)
      })
    
    (var-set next-amendment-id (+ amendment-id u1))
    (ok amendment-id)))

;; Approve proposed amendment and activate grace period
(define-public (approve-amendment (amendment-id uint))
  (let ((amendment (unwrap! (get-amendment amendment-id) err-amendment-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (is-eq (get status amendment) "proposed") err-invalid-amendment-type)
    
    (let ((grace-end-block (+ stacks-block-height (var-get grace-period-blocks))))
      (map-set amendments
        { amendment-id: amendment-id }
        (merge amendment {
          status: "approved",
          approved-at: (some stacks-block-height),
          effective-at: (some grace-end-block)
        }))
      
      ;; Trigger notification system for all current bidders
      (unwrap! (notify-all-bidders amendment-id (get tender-id amendment)) (err u408))
      (ok grace-end-block))))

;; Notify bidders about tender amendments
(define-private (notify-all-bidders (amendment-id uint) (tender-id uint))
  (let (
    (tender-bids (unwrap! (contract-call? .procurement-system get-tender-bids tender-id) (err u409)))
    (bid-ids (get bid-ids tender-bids))
  )
    (fold notify-bidder-of-amendment bid-ids (ok amendment-id))))

;; Helper function to notify individual bidders
(define-private (notify-bidder-of-amendment (bid-id uint) (result (response uint uint)))
  (match result
    amendment-id 
    (let ((bid-data (unwrap! (contract-call? .procurement-system get-bid bid-id) result)))
      (map-set amendment-notifications
        { amendment-id: amendment-id, bidder: (get bidder bid-data) }
        {
          notified-at: stacks-block-height,
          acknowledged: false,
          acknowledged-at: none,
          withdrawal-eligible: true
        })
      (ok amendment-id))
    error result))

;; Allow bidders to acknowledge amendment notifications
(define-public (acknowledge-amendment (amendment-id uint))
  (let ((notification (unwrap! (get-amendment-notification amendment-id tx-sender) err-amendment-not-found)))
    (asserts! (not (get acknowledged notification)) (err u410))
    
    (map-set amendment-notifications
      { amendment-id: amendment-id, bidder: tx-sender }
      (merge notification {
        acknowledged: true,
        acknowledged-at: (some stacks-block-height)
      }))
    (ok true)))

;; Allow bid withdrawal during grace period due to amendment
(define-public (withdraw-bid-due-amendment (tender-id uint) (amendment-id uint) (withdrawal-reason (string-utf8 200)))
  (let (
    (amendment (unwrap! (get-amendment amendment-id) err-amendment-not-found))
    (notification (unwrap! (get-amendment-notification amendment-id tx-sender) err-amendment-not-found))
    (existing-withdrawal (get-withdrawal-info tender-id tx-sender))
  )
    (asserts! (is-eq (get tender-id amendment) tender-id) err-amendment-not-found)
    (asserts! (is-eq (get status amendment) "approved") err-invalid-amendment-type)
    (asserts! (get withdrawal-eligible notification) err-grace-period-active)
    (asserts! (is-none existing-withdrawal) err-already-withdrawn)
    
    ;; Check if still within grace period
    (let ((effective-at (unwrap! (get effective-at amendment) err-grace-period-active)))
      (asserts! (< stacks-block-height effective-at) err-grace-period-active)
      
      (map-set amendment-withdrawals
        { tender-id: tender-id, bidder: tx-sender }
        {
          withdrawal-reason: withdrawal-reason,
          withdrawn-at: stacks-block-height,
          amendment-id: amendment-id,
          refund-processed: false
        })
      
      (ok true))))

;; Get amendment history for a tender
(define-read-only (get-tender-amendment-history (tender-id uint))
  (let ((amendments-info (get-tender-amendments tender-id)))
    {
      tender-id: tender-id,
      total-amendments: (get amendment-count amendments-info),
      last-amendment: (get last-amendment-block amendments-info),
      notifications-sent: (get total-notifications-sent amendments-info),
      amendment-window-open: (is-amendment-window-open tender-id)
    }))

;; Check if bidder is eligible for withdrawal
(define-read-only (check-withdrawal-eligibility (tender-id uint) (bidder principal))
  (match (get-withdrawal-info tender-id bidder)
    withdrawal-info 
    { eligible: false, reason: "already-withdrawn" }
    { eligible: true, reason: "no-previous-withdrawal" }))

;; Get amendment impact analysis
(define-read-only (get-amendment-impact (amendment-id uint))
  (let ((amendment (unwrap! (get-amendment amendment-id) none)))
    (match amendment
      amendment-data
      (some {
        amendment-id: amendment-id,
        tender-id: (get tender-id amendment-data),
        type: (get amendment-type amendment-data),
        impact-level: (get impact-level amendment-data),
        status: (get status amendment-data),
        grace-period-end: (get effective-at amendment-data),
        bidders-affected: (count-affected-bidders amendment-id)
      })
      none)))

;; Count bidders affected by amendment
(define-read-only (count-affected-bidders (amendment-id uint))
  u0) ;; Simplified implementation

;; Administrative function to extend amendment window
(define-public (extend-amendment-window (additional-blocks uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (<= additional-blocks u2880) (err u411)) ;; Max 20 days extension
    (var-set amendment-window-blocks (+ (var-get amendment-window-blocks) additional-blocks))
    (ok (var-get amendment-window-blocks))))

;; Update amendment system parameters
(define-public (update-amendment-parameters (window-blocks uint) (grace-blocks uint) (max-amendments uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (and (> window-blocks u144) (> grace-blocks u144)) (err u412))
    (asserts! (<= max-amendments u10) (err u413))
    
    (var-set amendment-window-blocks window-blocks)
    (var-set grace-period-blocks grace-blocks)
    (var-set max-amendments-per-tender max-amendments)
    (ok true)))