;; Royalty Withdrawal System
;; Enables artists to track and withdraw accumulated earnings from sales and royalties

;; Error constants
(define-constant err-unauthorized (err u300))
(define-constant err-not-found (err u301))
(define-constant err-insufficient-balance (err u302))
(define-constant err-invalid-amount (err u303))
(define-constant err-zero-withdrawal (err u304))

;; Data variables
(define-data-var last-withdrawal-id uint u0)

;; Artist earnings tracking
(define-map artist-earnings
  { artist-address: principal }
  {
    total-earned: uint,
    total-withdrawn: uint,
    last-update-block: uint
  }
)

;; Withdrawal history
(define-map withdrawal-records
  { withdrawal-id: uint }
  {
    artist-address: principal,
    amount: uint,
    remaining-balance: uint,
    timestamp: uint,
    block-height: uint
  }
)

;; Pending withdrawals (for multi-step withdrawals)
(define-map pending-withdrawals
  { artist-address: principal }
  {
    amount: uint,
    created-block: uint,
    expires-block: uint
  }
)

;; Read-only functions
(define-read-only (get-artist-balance (artist-address principal))
  (let
    (
      (earnings (map-get? artist-earnings { artist-address: artist-address }))
    )
    (match earnings
      some-earnings 
        (ok (- (get total-earned some-earnings) (get total-withdrawn some-earnings)))
      (ok u0)
    )
  )
)

(define-read-only (get-artist-earnings (artist-address principal))
  (map-get? artist-earnings { artist-address: artist-address })
)

(define-read-only (get-withdrawal-record (withdrawal-id uint))
  (map-get? withdrawal-records { withdrawal-id: withdrawal-id })
)

(define-read-only (get-pending-withdrawal (artist-address principal))
  (map-get? pending-withdrawals { artist-address: artist-address })
)

;; Add earnings to artist account (called by main royalty contract)
(define-public (add-earnings (artist-address principal) (amount uint))
  (let
    (
      (current-earnings (default-to 
        { total-earned: u0, total-withdrawn: u0, last-update-block: u0 }
        (map-get? artist-earnings { artist-address: artist-address })
      ))
      (new-total (+ (get total-earned current-earnings) amount))
    )
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set artist-earnings
      { artist-address: artist-address }
      {
        total-earned: new-total,
        total-withdrawn: (get total-withdrawn current-earnings),
        last-update-block: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Immediate withdrawal of available balance
(define-public (withdraw-earnings (amount uint))
  (let
    (
      (current-balance (unwrap! (get-artist-balance tx-sender) err-not-found))
      (current-earnings (unwrap! (map-get? artist-earnings { artist-address: tx-sender }) err-not-found))
      (new-withdrawal-id (+ (var-get last-withdrawal-id) u1))
      (new-total-withdrawn (+ (get total-withdrawn current-earnings) amount))
      (remaining-balance (- current-balance amount))
    )
    (asserts! (> amount u0) err-zero-withdrawal)
    (asserts! (>= current-balance amount) err-insufficient-balance)
    
    ;; Transfer funds to artist
    (try! (as-contract (stx-transfer? amount (as-contract tx-sender) tx-sender)))
    
    ;; Update earnings record
    (map-set artist-earnings
      { artist-address: tx-sender }
      {
        total-earned: (get total-earned current-earnings),
        total-withdrawn: new-total-withdrawn,
        last-update-block: stacks-block-height
      }
    )
    
    ;; Record withdrawal
    (var-set last-withdrawal-id new-withdrawal-id)
    (map-set withdrawal-records
      { withdrawal-id: new-withdrawal-id }
      {
        artist-address: tx-sender,
        amount: amount,
        remaining-balance: remaining-balance,
        timestamp: stacks-block-height,
        block-height: stacks-block-height
      }
    )
    
    (ok new-withdrawal-id)
  )
)

;; Schedule a withdrawal for later (useful for gas optimization)
(define-public (schedule-withdrawal (amount uint) (delay-blocks uint))
  (let
    (
      (current-balance (unwrap! (get-artist-balance tx-sender) err-not-found))
      (expires-block (+ stacks-block-height delay-blocks))
    )
    (asserts! (> amount u0) err-zero-withdrawal)
    (asserts! (>= current-balance amount) err-insufficient-balance)
    
    (map-set pending-withdrawals
      { artist-address: tx-sender }
      {
        amount: amount,
        created-block: stacks-block-height,
        expires-block: expires-block
      }
    )
    (ok true)
  )
)

;; Execute a previously scheduled withdrawal
(define-public (execute-scheduled-withdrawal)
  (let
    (
      (pending (unwrap! (map-get? pending-withdrawals { artist-address: tx-sender }) err-not-found))
      (withdrawal-amount (get amount pending))
      (current-balance (unwrap! (get-artist-balance tx-sender) err-not-found))
    )
    ;; Check if withdrawal hasn't expired
    (asserts! (< stacks-block-height (get expires-block pending)) err-not-found)
    (asserts! (>= current-balance withdrawal-amount) err-insufficient-balance)
    
    ;; Remove pending withdrawal
    (map-delete pending-withdrawals { artist-address: tx-sender })
    
    ;; Execute withdrawal
    (try! (withdraw-earnings withdrawal-amount))
    (ok true)
  )
)

;; Cancel a scheduled withdrawal
(define-public (cancel-scheduled-withdrawal)
  (let
    (
      (pending (unwrap! (map-get? pending-withdrawals { artist-address: tx-sender }) err-not-found))
    )
    (map-delete pending-withdrawals { artist-address: tx-sender })
    (ok true)
  )
)

;; Get artist's earning summary
(define-read-only (get-earnings-summary (artist-address principal))
  (let
    (
      (earnings (default-to 
        { total-earned: u0, total-withdrawn: u0, last-update-block: u0 }
        (map-get? artist-earnings { artist-address: artist-address })))
      (pending (map-get? pending-withdrawals { artist-address: artist-address }))
      (available-balance (unwrap-panic (get-artist-balance artist-address)))
    )
    (ok {
      total-earned: (get total-earned earnings),
      total-withdrawn: (get total-withdrawn earnings),
      available-balance: available-balance,
      pending-withdrawal: (if (is-some pending) (get amount (unwrap! pending err-not-found)) u0),
      last-update-block: (get last-update-block earnings)
    })
  )
)

;; Emergency withdrawal of all available funds
(define-public (emergency-withdraw-all)
  (let
    (
      (current-balance (unwrap! (get-artist-balance tx-sender) err-not-found))
    )
    (asserts! (> current-balance u0) err-zero-withdrawal)
    
    ;; Cancel any pending withdrawal first
    (match (map-get? pending-withdrawals { artist-address: tx-sender })
      some-pending (map-delete pending-withdrawals { artist-address: tx-sender })
      true
    )
    
    ;; Withdraw all available balance
    (try! (withdraw-earnings current-balance))
    (ok current-balance)
  )
)
