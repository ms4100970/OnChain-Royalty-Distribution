(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROPERTY_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_SHARES (err u102))
(define-constant ERR_PROPERTY_EXISTS (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_NOT_OWNER (err u105))
(define-constant ERR_TRANSFER_FAILED (err u106))
(define-constant ERR_INVALID_SHARES (err u107))
(define-constant ERR_NO_INCOME_AVAILABLE (err u108))
(define-constant ERR_ALREADY_CLAIMED (err u109))
(define-constant ERR_INVALID_PERIOD (err u110))
(define-constant ERR_RENTAL_PERIOD_ACTIVE (err u111))
(define-constant ERR_RENTAL_PERIOD_ENDED (err u112))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u113))
(define-constant ERR_PROPOSAL_EXPIRED (err u114))
(define-constant ERR_PROPOSAL_NOT_ACTIVE (err u115))
(define-constant ERR_ALREADY_VOTED (err u116))
(define-constant ERR_PROPOSAL_NOT_APPROVED (err u117))
(define-constant ERR_INSUFFICIENT_FUNDS (err u118))
(define-constant ERR_PROPOSAL_ALREADY_EXECUTED (err u119))
(define-constant ERR_MARKET_DATA_NOT_FOUND (err u120))
(define-constant ERR_INVALID_PRICE_CHANGE (err u121))
(define-constant ERR_ANALYSIS_PERIOD_TOO_SHORT (err u122))

(define-data-var next-property-id uint u1)
(define-data-var next-rental-period-id uint u1)
(define-data-var next-maintenance-proposal-id uint u1)
(define-data-var next-price-history-id uint u1)
(define-data-var market-analysis-enabled bool true)

(define-map properties
  { property-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    total-shares: uint,
    share-price: uint,
    owner: principal,
    created-at: uint
  }
)

(define-map property-shares
  { property-id: uint, owner: principal }
  { shares: uint }
)

(define-map property-shareholders
  { property-id: uint }
  { shareholders: (list 100 principal) }
)

(define-map user-properties
  { user: principal }
  { properties: (list 50 uint) }
)

(define-map rental-periods
  { rental-period-id: uint }
  {
    property-id: uint,
    total-income: uint,
    start-block: uint,
    end-block: uint,
    claimed-by: (list 100 principal),
    is-active: bool
  }
)

(define-map property-rental-periods
  { property-id: uint }
  { rental-periods: (list 100 uint) }
)

(define-map rental-income-claims
  { rental-period-id: uint, claimant: principal }
  { amount: uint, claimed-at: uint }
)

(define-map property-income-history
  { property-id: uint }
  { total-income: uint, last-distribution: uint }
)

(define-map maintenance-proposals
  { proposal-id: uint }
  {
    property-id: uint,
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    cost: uint,
    contractor: principal,
    created-at: uint,
    voting-end: uint,
    votes-for: uint,
    votes-against: uint,
    total-voting-power: uint,
    is-approved: bool,
    is-executed: bool,
    funds-collected: uint
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, voting-power: uint, voted-at: uint }
)

(define-map property-maintenance-proposals
  { property-id: uint }
  { proposals: (list 50 uint) }
)

(define-map maintenance-fund-contributions
  { proposal-id: uint, contributor: principal }
  { amount: uint, contributed-at: uint }
)

(define-map property-maintenance-history
  { property-id: uint }
  { total-spent: uint, completed-proposals: (list 50 uint) }
)

;; ----- Market Analysis Data Maps -----

;; Price history tracking for market analysis
(define-map property-price-history
  { property-id: uint, entry-id: uint }
  {
    old-price: uint,
    new-price: uint,
    price-change-percentage: int,
    timestamp: uint,
    changed-by: principal
  }
)

;; Property performance metrics
(define-map property-performance
  { property-id: uint }
  {
    initial-price: uint,
    current-price: uint,
    highest-price: uint,
    lowest-price: uint,
    total-price-changes: uint,
    total-appreciation: int,
    last-analysis-update: uint,
    performance-rating: uint
  }
)

;; Market trend analysis data
(define-map market-trends
  { analysis-period: uint }
  {
    average-price-change: int,
    properties-analyzed: uint,
    trending-up: uint,
    trending-down: uint,
    stable-properties: uint,
    market-sentiment: (string-ascii 20)
  }
)

;; Property comparison rankings
(define-map property-rankings
  { property-id: uint }
  {
    performance-rank: uint,
    roi-percentage: int,
    volatility-score: uint,
    investment-grade: (string-ascii 10),
    last-ranking_update: uint
  }
)

;; Track price history entries per property
(define-map property-price-entries
  { property-id: uint }
  { entry-count: uint }
)

;; ----- Core Functions (Shortened for space) -----

(define-public (create-property (name (string-ascii 100)) (description (string-ascii 500)) (total-shares uint) (share-price uint))
  (let
    (
      (property-id (var-get next-property-id))
    )
    (asserts! (> total-shares u0) ERR_INVALID_SHARES)
    (asserts! (> share-price u0) ERR_INVALID_AMOUNT)
    (map-set properties
      { property-id: property-id }
      {
        name: name,
        description: description,
        total-shares: total-shares,
        share-price: share-price,
        owner: tx-sender,
        created-at: stacks-block-height
      }
    )
    (map-set property-shares
      { property-id: property-id, owner: tx-sender }
      { shares: total-shares }
    )
    (map-set property-shareholders
      { property-id: property-id }
      { shareholders: (list tx-sender) }
    )
    (map-set user-properties
      { user: tx-sender }
      { properties: (unwrap-panic (as-max-len? (append (get-user-properties tx-sender) property-id) u50)) }
    )
    (var-set next-property-id (+ property-id u1))
    (ok property-id)
  )
)

(define-public (update-share-price (property-id uint) (new-price uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (old-price (get share-price property))
    )
    (asserts! (is-eq tx-sender (get owner property)) ERR_NOT_AUTHORIZED)
    (asserts! (> new-price u0) ERR_INVALID_AMOUNT)
    
    ;; Update property price
    (map-set properties
      { property-id: property-id }
      (merge property { share-price: new-price })
    )
    
    ;; Record price change for market analysis if enabled
    (if (and (var-get market-analysis-enabled) (not (is-eq old-price new-price)))
      (match (record-price-change property-id old-price new-price)
        success true
        error true)
      true
    )
    
    (ok new-price)
  )
)

;; ----- Market Analysis Functions -----

;; Record price change when share price is updated
(define-public (record-price-change (property-id uint) (old-price uint) (new-price uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (price-entries (default-to { entry-count: u0 } (map-get? property-price-entries { property-id: property-id })))
      (entry-id (get entry-count price-entries))
      (price-change (- (to-int new-price) (to-int old-price)))
      (price-change-percentage (if (> old-price u0) (/ (* price-change 10000) (to-int old-price)) 0))
    )
    (asserts! (is-eq tx-sender (get owner property)) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq old-price new-price)) ERR_INVALID_PRICE_CHANGE)
    (asserts! (var-get market-analysis-enabled) ERR_MARKET_DATA_NOT_FOUND)
    
    (map-set property-price-history
      { property-id: property-id, entry-id: entry-id }
      {
        old-price: old-price,
        new-price: new-price,
        price-change-percentage: price-change-percentage,
        timestamp: stacks-block-height,
        changed-by: tx-sender
      }
    )
    
    (map-set property-price-entries
      { property-id: property-id }
      { entry-count: (+ entry-id u1) }
    )
    
    (try! (update-property-performance property-id new-price price-change-percentage))
    
    (ok entry-id)
  )
)

;; Update property performance metrics
(define-public (update-property-performance (property-id uint) (current-price uint) (price-change-percentage int))
  (let
    (
      (existing-performance (map-get? property-performance { property-id: property-id }))
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (initial-price (get share-price property))
    )
    (match existing-performance
      performance-data
      (let
        (
          (new-highest (if (> current-price (get highest-price performance-data)) current-price (get highest-price performance-data)))
          (new-lowest (if (< current-price (get lowest-price performance-data)) current-price (get lowest-price performance-data)))
          (total-appreciation (+ (get total-appreciation performance-data) price-change-percentage))
          (performance-rating (calculate-performance-rating total-appreciation))
        )
        (map-set property-performance
          { property-id: property-id }
          {
            initial-price: (get initial-price performance-data),
            current-price: current-price,
            highest-price: new-highest,
            lowest-price: new-lowest,
            total-price-changes: (+ (get total-price-changes performance-data) u1),
            total-appreciation: total-appreciation,
            last-analysis-update: stacks-block-height,
            performance-rating: performance-rating
          }
        )
      )
      ;; Create initial performance record
      (map-set property-performance
        { property-id: property-id }
        {
          initial-price: initial-price,
          current-price: current-price,
          highest-price: current-price,
          lowest-price: current-price,
          total-price-changes: u1,
          total-appreciation: price-change-percentage,
          last-analysis-update: stacks-block-height,
          performance-rating: (calculate-performance-rating price-change-percentage)
        }
      )
    )
    
    (ok true)
  )
)

;; Calculate performance rating based on appreciation
(define-private (calculate-performance-rating (total-appreciation int))
  (if (>= total-appreciation 2000) u5
    (if (>= total-appreciation 1000) u4
      (if (>= total-appreciation 500) u3
        (if (>= total-appreciation 0) u2
          u1)))))

;; Generate market trend analysis
(define-public (analyze-market-trends (analysis-blocks uint))
  (let
    (
      (current-block stacks-block-height)
      (analysis-period (/ current-block analysis-blocks))
    )
    (asserts! (> analysis-blocks u144) ERR_ANALYSIS_PERIOD_TOO_SHORT)
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    (map-set market-trends
      { analysis-period: analysis-period }
      {
        average-price-change: 0,
        properties-analyzed: u0,
        trending-up: u0,
        trending-down: u0,
        stable-properties: u0,
        market-sentiment: "neutral"
      }
    )
    
    (ok analysis-period)
  )
)

;; Determine investment grade
(define-private (determine-investment-grade (roi-percentage int) (volatility uint))
  (if (and (> roi-percentage 1500) (<= volatility u3)) "A"
    (if (and (> roi-percentage 1000) (<= volatility u6)) "B"
      (if (> roi-percentage 0) "C"
        "D"))))

;; Read-only functions for market analysis
(define-read-only (get-property-price-history (property-id uint) (entry-id uint))
  (map-get? property-price-history { property-id: property-id, entry-id: entry-id })
)

(define-read-only (get-property-performance (property-id uint))
  (map-get? property-performance { property-id: property-id })
)

(define-read-only (get-market-trends (analysis-period uint))
  (map-get? market-trends { analysis-period: analysis-period })
)

(define-read-only (get-property-ranking (property-id uint))
  (map-get? property-rankings { property-id: property-id })
)

;; ----- Required Read-Only Functions -----

(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-user-shares (property-id uint) (user principal))
  (default-to u0 (get shares (map-get? property-shares { property-id: property-id, owner: user })))
)

(define-read-only (get-user-properties (user principal))
  (default-to (list) (get properties (map-get? user-properties { user: user })))
)

(define-read-only (get-total-properties)
  (- (var-get next-property-id) u1)
)
