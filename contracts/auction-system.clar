(define-constant err-auction-not-found (err u200))
(define-constant err-auction-ended (err u201))
(define-constant err-auction-not-ended (err u202))
(define-constant err-bid-too-low (err u203))
(define-constant err-reserve-not-met (err u204))
(define-constant err-invalid-duration (err u205))
(define-constant err-unauthorized (err u206))
(define-constant err-not-found (err u207))
(define-constant err-auction-active (err u208))
(define-constant err-invalid-extension (err u209))

(define-map auctions
  { auction-id: uint }
  {
    work-id: uint,
    seller: principal,
    starting-price: uint,
    reserve-price: uint,
    current-bid: uint,
    highest-bidder: (optional principal),
    end-block: uint,
    active: bool,
    extension-window: uint,
    extension-duration: uint
  }
)

(define-map auction-bids
  { auction-id: uint, bidder: principal }
  { amount: uint, timestamp: uint }
)

(define-data-var last-auction-id uint u0)

(define-read-only (get-auction (auction-id uint))
  (map-get? auctions { auction-id: auction-id })
)

(define-read-only (get-bid (auction-id uint) (bidder principal))
  (map-get? auction-bids { auction-id: auction-id, bidder: bidder })
)

(define-read-only (is-auction-ended (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { auction-id: auction-id }) err-auction-not-found))
    )
    (ok (>= stacks-block-height (get end-block auction)))
  )
)

(define-public (create-auction (work-id uint) (starting-price uint) (reserve-price uint) (duration uint))
  (let
    (
      (new-auction-id (+ (var-get last-auction-id) u1))
      (end-block (+ stacks-block-height duration))
    )
    (asserts! (> duration u0) err-invalid-duration)
    (asserts! (>= reserve-price starting-price) err-bid-too-low)
    
    (var-set last-auction-id new-auction-id)
    (map-set auctions
      { auction-id: new-auction-id }
      {
        work-id: work-id,
        seller: tx-sender,
        starting-price: starting-price,
        reserve-price: reserve-price,
        current-bid: u0,
        highest-bidder: none,
        end-block: end-block,
        active: true,
        extension-window: u10,
        extension-duration: u10
      }
    )
    (ok new-auction-id)
  )
)

(define-public (create-auction-with-extension (work-id uint) (starting-price uint) (reserve-price uint) (duration uint) (extension-window uint) (extension-duration uint))
  (let
    (
      (new-auction-id (+ (var-get last-auction-id) u1))
      (end-block (+ stacks-block-height duration))
    )
    (asserts! (> duration u0) err-invalid-duration)
    (asserts! (>= reserve-price starting-price) err-bid-too-low)
    (asserts! (> extension-window u0) err-invalid-extension)
    (asserts! (> extension-duration u0) err-invalid-extension)
    (asserts! (< extension-window duration) err-invalid-extension)
    
    (var-set last-auction-id new-auction-id)
    (map-set auctions
      { auction-id: new-auction-id }
      {
        work-id: work-id,
        seller: tx-sender,
        starting-price: starting-price,
        reserve-price: reserve-price,
        current-bid: u0,
        highest-bidder: none,
        end-block: end-block,
        active: true,
        extension-window: extension-window,
        extension-duration: extension-duration
      }
    )
    (ok new-auction-id)
  )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { auction-id: auction-id }) err-auction-not-found))
      (current-bid (get current-bid auction))
      (min-bid (if (> current-bid u0) (+ current-bid u1) (get starting-price auction)))
      (end-block (get end-block auction))
      (extension-window (get extension-window auction))
      (extension-duration (get extension-duration auction))
      (blocks-until-end (- end-block stacks-block-height))
      (should-extend (and (<= blocks-until-end extension-window) (> blocks-until-end u0)))
      (new-end-block (if should-extend (+ end-block extension-duration) end-block))
    )
    (asserts! (get active auction) err-auction-not-found)
    (asserts! (< stacks-block-height end-block) err-auction-ended)
    (asserts! (>= bid-amount min-bid) err-bid-too-low)
    
    (if (> current-bid u0)
      (try! (as-contract (stx-transfer? current-bid (as-contract tx-sender) (unwrap! (get highest-bidder auction) err-not-found))))
      true
    )
    
    (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
    
    (map-set auctions
      { auction-id: auction-id }
      (merge auction {
        current-bid: bid-amount,
        highest-bidder: (some tx-sender),
        end-block: new-end-block
      })
    )
    
    (map-set auction-bids
      { auction-id: auction-id, bidder: tx-sender }
      {
        amount: bid-amount,
        timestamp: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (finalize-auction (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { auction-id: auction-id }) err-auction-not-found))
      (current-bid (get current-bid auction))
      (reserve-price (get reserve-price auction))
      (seller (get seller auction))
      (highest-bidder (get highest-bidder auction))
    )
    (asserts! (get active auction) err-auction-not-found)
    (asserts! (>= stacks-block-height (get end-block auction)) err-auction-not-ended)
    
    (if (and (> current-bid u0) (>= current-bid reserve-price))
      (begin
        (try! (as-contract (stx-transfer? current-bid (as-contract tx-sender) seller)))
        (map-set auctions
          { auction-id: auction-id }
          (merge auction { active: false })
        )
        (ok { winner: highest-bidder, amount: current-bid })
      )
      (begin
        (if (> current-bid u0)
          (try! (as-contract (stx-transfer? current-bid (as-contract tx-sender) (unwrap! highest-bidder err-not-found))))
          true
        )
        (map-set auctions
          { auction-id: auction-id }
          (merge auction { active: false })
        )
        err-reserve-not-met
      )
    )
  )
)

(define-public (cancel-auction (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { auction-id: auction-id }) err-auction-not-found))
      (current-bid (get current-bid auction))
      (highest-bidder (get highest-bidder auction))
    )
    (asserts! (is-eq tx-sender (get seller auction)) err-unauthorized)
    (asserts! (get active auction) err-auction-not-found)
    (asserts! (is-eq current-bid u0) err-auction-active)
    
    (map-set auctions
      { auction-id: auction-id }
      (merge auction { active: false })
    )
    (ok true)
  )
)

(define-read-only (get-auction-status (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { auction-id: auction-id }) err-auction-not-found))
      (is-ended (>= stacks-block-height (get end-block auction)))
      (reserve-met (>= (get current-bid auction) (get reserve-price auction)))
    )
    (ok {
      active: (get active auction),
      ended: is-ended,
      reserve-met: reserve-met,
      current-bid: (get current-bid auction),
      highest-bidder: (get highest-bidder auction),
      blocks-remaining: (if is-ended u0 (- (get end-block auction) stacks-block-height))
    })
  )
)