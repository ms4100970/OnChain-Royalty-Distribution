(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-percentage (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-unauthorized (err u105))
(define-constant err-no-royalties (err u106))
(define-constant err-zero-amount (err u107))

(define-data-var royalty-percentage uint u10)

(define-map artists 
  { artist-id: uint }
  { 
    address: principal,
    name: (string-ascii 64),
    active: bool
  }
)

(define-map works
  { work-id: uint }
  {
    artist-id: uint,
    title: (string-ascii 128),
    price: uint,
    royalty-percentage: uint,
    total-sales: uint,
    active: bool
  }
)

(define-map collaborators
  { work-id: uint, artist-id: uint }
  { share-percentage: uint }
)

(define-map sales
  { sale-id: uint }
  {
    work-id: uint,
    buyer: principal,
    amount: uint,
    timestamp: uint
  }
)

(define-data-var last-artist-id uint u0)
(define-data-var last-work-id uint u0)
(define-data-var last-sale-id uint u0)

(define-read-only (get-royalty-percentage)
  (var-get royalty-percentage)
)

(define-read-only (get-artist (artist-id uint))
  (map-get? artists { artist-id: artist-id })
)

(define-read-only (get-work (work-id uint))
  (map-get? works { work-id: work-id })
)

(define-read-only (get-collaborator (work-id uint) (artist-id uint))
  (map-get? collaborators { work-id: work-id, artist-id: artist-id })
)

(define-read-only (get-sale (sale-id uint))
  (map-get? sales { sale-id: sale-id })
)



(define-public (set-royalty-percentage (new-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-percentage u100) err-invalid-percentage)
    (ok (var-set royalty-percentage new-percentage))
  )
)

(define-public (register-artist (name (string-ascii 64)))
  (let
    (
      (new-id (+ (var-get last-artist-id) u1))
    )
    (var-set last-artist-id new-id)
    (map-set artists
      { artist-id: new-id }
      {
        address: tx-sender,
        name: name,
        active: true
      }
    )
    (ok new-id)
  )
)

(define-public (update-artist (artist-id uint) (name (string-ascii 64)))
  (let
    (
      (artist (unwrap! (map-get? artists { artist-id: artist-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get address artist)) err-unauthorized)
    (map-set artists
      { artist-id: artist-id }
      (merge artist { name: name })
    )
    (ok true)
  )
)

(define-public (deactivate-artist (artist-id uint))
  (let
    (
      (artist (unwrap! (map-get? artists { artist-id: artist-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get address artist)) err-unauthorized)
    (map-set artists
      { artist-id: artist-id }
      (merge artist { active: false })
    )
    (ok true)
  )
)

(define-public (register-work (artist-id uint) (title (string-ascii 128)) (price uint) (custom-royalty-percentage uint))
  (let
    (
      (artist (unwrap! (map-get? artists { artist-id: artist-id }) err-not-found))
      (new-id (+ (var-get last-work-id) u1))
      (royalty (if (> custom-royalty-percentage u0) custom-royalty-percentage (var-get royalty-percentage)))
    )
    (asserts! (is-eq tx-sender (get address artist)) err-unauthorized)
    (asserts! (<= royalty u100) err-invalid-percentage)
    (var-set last-work-id new-id)
    (map-set works
      { work-id: new-id }
      {
        artist-id: artist-id,
        title: title,
        price: price,
        royalty-percentage: royalty,
        total-sales: u0,
        active: true
      }
    )
    (ok new-id)
  )
)

(define-public (add-collaborator (work-id uint) (artist-id uint) (share-percentage uint))
  (let
    (
      (work (unwrap! (map-get? works { work-id: work-id }) err-not-found))
      (artist (unwrap! (map-get? artists { artist-id: artist-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get address (unwrap! (map-get? artists { artist-id: (get artist-id work) }) err-not-found))) err-unauthorized)
    (asserts! (<= share-percentage u100) err-invalid-percentage)
    (map-set collaborators
      { work-id: work-id, artist-id: artist-id }
      { share-percentage: share-percentage }
    )
    (ok true)
  )
)

(define-public (update-work-price (work-id uint) (new-price uint))
  (let
    (
      (work (unwrap! (map-get? works { work-id: work-id }) err-not-found))
      (artist (unwrap! (map-get? artists { artist-id: (get artist-id work) }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get address artist)) err-unauthorized)
    (map-set works
      { work-id: work-id }
      (merge work { price: new-price })
    )
    (ok true)
  )
)

(define-public (deactivate-work (work-id uint))
  (let
    (
      (work (unwrap! (map-get? works { work-id: work-id }) err-not-found))
      (artist (unwrap! (map-get? artists { artist-id: (get artist-id work) }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get address artist)) err-unauthorized)
    (map-set works
      { work-id: work-id }
      (merge work { active: false })
    )
    (ok true)
  )
)

(define-public (purchase-work (work-id uint))
  (let
    (
      (work (unwrap! (map-get? works { work-id: work-id }) err-not-found))
      (price (get price work))
      (royalty-amount (/ (* price (get royalty-percentage work)) u100))
      (artist-id (get artist-id work))
      (artist (unwrap! (map-get? artists { artist-id: artist-id }) err-not-found))
      (new-sale-id (+ (var-get last-sale-id) u1))
    )
    (asserts! (get active work) err-not-found)
    (asserts! (> price u0) err-zero-amount)
    
    (try! (stx-transfer? price tx-sender (get address artist)))
    
    (var-set last-sale-id new-sale-id)
    (map-set sales
      { sale-id: new-sale-id }
      {
        work-id: work-id,
        buyer: tx-sender,
        amount: price,
        timestamp: stacks-block-height
      }
    )
    
    (map-set works
      { work-id: work-id }
      (merge work { total-sales: (+ (get total-sales work) u1) })
    )
    
    (ok new-sale-id)
  )
)



(define-read-only (get-work-royalties (work-id uint))
  (let
    (
      (work (unwrap! (map-get? works { work-id: work-id }) err-not-found))
      (price (get price work))
      (royalty-percentages (get royalty-percentage work))
    )
    (ok (/ (* price royalty-percentages (get total-sales work)) u100))
  )
)