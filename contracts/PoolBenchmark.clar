;; Pool Performance Benchmarking & Rewards System
;; Tracks performance metrics and distributes rewards to high-performing pools

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_POOL_NOT_FOUND (err u201))
(define-constant ERR_INSUFFICIENT_FUNDS (err u202))
(define-constant ERR_INVALID_AMOUNT (err u203))
(define-constant ERR_BENCHMARK_NOT_READY (err u204))
(define-constant ERR_REWARDS_ALREADY_CLAIMED (err u205))
(define-constant ERR_NOT_ELIGIBLE (err u206))

;; Benchmark periods and scoring
(define-data-var current-benchmark-period uint u1)
(define-data-var benchmark-period-blocks uint u1008) ;; ~1 week
(define-data-var reward-pool-balance uint u0)
(define-data-var min-benchmark-pools uint u3)
(define-data-var top-performer-threshold uint u800)

;; Performance tracking maps
(define-map benchmark-periods
  { period: uint }
  {
    start-block: uint,
    end-block: uint,
    participating-pools: uint,
    total-reward-distributed: uint,
    network-avg-score: uint,
    completed: bool
  }
)

(define-map pool-performance
  { pool-id: uint, period: uint }
  {
    efficiency-score: uint,
    profitability-ratio: uint,
    claim-resolution-speed: uint,
    participant-satisfaction: uint,
    overall-score: uint,
    benchmark-rank: uint,
    reward-earned: uint,
    data-collected: bool
  }
)

(define-map participant-rewards
  { pool-id: uint, participant: principal, period: uint }
  {
    performance-bonus: uint,
    benchmark-bonus: uint,
    total-reward: uint,
    claimed: bool,
    earned-at: uint
  }
)

(define-map network-benchmarks
  { period: uint }
  {
    avg-efficiency: uint,
    avg-profitability: uint,
    avg-resolution-speed: uint,
    avg-satisfaction: uint,
    top-10-percent-score: uint,
    median-score: uint
  }
)

;; Read-only function to get pool performance data
(define-read-only (get-pool-performance (pool-id uint) (period uint))
  (map-get? pool-performance { pool-id: pool-id, period: period })
)

;; Read-only function to get current benchmark period
(define-read-only (get-current-benchmark-period)
  (var-get current-benchmark-period)
)

;; Read-only function to get network benchmarks
(define-read-only (get-network-benchmarks (period uint))
  (map-get? network-benchmarks { period: period })
)

;; Read-only function to get participant rewards
(define-read-only (get-participant-rewards (pool-id uint) (participant principal) (period uint))
  (map-get? participant-rewards { pool-id: pool-id, participant: participant, period: period })
)

;; Read-only function to get benchmark period info
(define-read-only (get-benchmark-period (period uint))
  (map-get? benchmark-periods { period: period })
)

;; Function to collect pool performance data
(define-public (collect-pool-performance (pool-id uint))
  (let
    (
      (period (var-get current-benchmark-period))
      (pool-data (unwrap! (contract-call? .Repool get-pool pool-id) ERR_POOL_NOT_FOUND))
      (pool-metrics (unwrap! (contract-call? .Repool get-pool-metrics pool-id) ERR_POOL_NOT_FOUND))
      
      ;; Calculate performance metrics
      (total-funds (get total-funds pool-data))
      (total-payouts (get total-payouts pool-data))
      (total-claims (get total-claims pool-data))
      (participant-count (get participant-count pool-data))
      
      ;; Efficiency: funds managed vs payouts (higher is better)
      (efficiency-score (if (> total-funds u0)
                         (min (/ (* (- total-funds total-payouts) u1000) total-funds) u1000)
                         u500))
      
      ;; Profitability: premium income vs claims paid
      (profitability-ratio (if (> total-payouts u0)
                            (min (/ (* total-funds u1000) total-payouts) u1000)
                            u1000))
      
      ;; Claim resolution: based on approval rate and frequency
      (approval-rate (get approval-rate pool-metrics))
      (claim-frequency (get claim-frequency pool-metrics))
      (resolution-speed (min (+ (/ approval-rate u2) (- u1000 claim-frequency)) u1000))
      
      ;; Satisfaction: based on participant count and activity
      (satisfaction-score (min (* participant-count u50) u1000))
      
      ;; Overall weighted score
      (overall-score (/ (+ (* efficiency-score u3)
                          (* profitability-ratio u3)
                          (* resolution-speed u2)
                          (* satisfaction-score u2)) u10))
    )
    (map-set pool-performance
      { pool-id: pool-id, period: period }
      {
        efficiency-score: efficiency-score,
        profitability-ratio: profitability-ratio,
        claim-resolution-speed: resolution-speed,
        participant-satisfaction: satisfaction-score,
        overall-score: overall-score,
        benchmark-rank: u0,
        reward-earned: u0,
        data-collected: true
      }
    )
    (ok overall-score)
  )
)

;; Function to fund the reward pool
(define-public (fund-reward-pool (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set reward-pool-balance (+ (var-get reward-pool-balance) amount))
    (ok (var-get reward-pool-balance))
  )
)

;; Function to start a new benchmark period
(define-public (start-new-benchmark-period)
  (let
    (
      (current-period (var-get current-benchmark-period))
      (period-blocks (var-get benchmark-period-blocks))
      (start-block stacks-block-height)
      (end-block (+ start-block period-blocks))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    ;; Complete current period if it exists
    (match (map-get? benchmark-periods { period: current-period })
      existing-period
      (if (and (> stacks-block-height (get end-block existing-period))
               (not (get completed existing-period)))
        (try! (finalize-benchmark-period current-period))
        (ok true))
      (ok true)
    )
    
    ;; Start new period
    (let ((new-period (+ current-period u1)))
      (map-set benchmark-periods
        { period: new-period }
        {
          start-block: start-block,
          end-block: end-block,
          participating-pools: u0,
          total-reward-distributed: u0,
          network-avg-score: u0,
          completed: false
        }
      )
      (var-set current-benchmark-period new-period)
      (ok new-period)
    )
  )
)

;; Function to finalize benchmark period and distribute rewards
(define-public (finalize-benchmark-period (period uint))
  (let
    (
      (period-data (unwrap! (map-get? benchmark-periods { period: period }) ERR_BENCHMARK_NOT_READY))
      (pool-count (contract-call? .Repool get-pool-count))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> stacks-block-height (get end-block period-data)) ERR_BENCHMARK_NOT_READY)
    (asserts! (not (get completed period-data)) ERR_REWARDS_ALREADY_CLAIMED)
    
    ;; Calculate network benchmarks and distribute rewards
    (try! (calculate-network-benchmarks period))
    (try! (distribute-period-rewards period))
    
    ;; Mark period as completed
    (map-set benchmark-periods
      { period: period }
      (merge period-data { completed: true })
    )
    (ok true)
  )
)

;; Private function to calculate network benchmarks
(define-private (calculate-network-benchmarks (period uint))
  (let
    (
      ;; Simplified calculation - in real implementation would iterate through all pools
      (sample-scores (list u800 u750 u900 u650 u850)) ;; Mock data
      (avg-score (/ (fold + sample-scores u0) (len sample-scores)))
      (top-threshold (var-get top-performer-threshold))
    )
    (map-set network-benchmarks
      { period: period }
      {
        avg-efficiency: u750,
        avg-profitability: u800,
        avg-resolution-speed: u700,
        avg-satisfaction: u650,
        top-10-percent-score: top-threshold,
        median-score: avg-score
      }
    )
    (ok true)
  )
)

;; Private function to distribute period rewards
(define-private (distribute-period-rewards (period uint))
  (let
    (
      (available-rewards (var-get reward-pool-balance))
      (base-reward (/ available-rewards u10)) ;; 10% of pool for this period
    )
    (asserts! (> available-rewards u1000000) ERR_INSUFFICIENT_FUNDS) ;; Minimum 1 STX
    
    ;; Update reward pool balance
    (var-set reward-pool-balance (- available-rewards base-reward))
    
    ;; In a full implementation, this would iterate through all qualifying pools
    ;; For now, we'll just mark the function as successful
    (ok base-reward)
  )
)

;; Function to claim performance rewards for a participant
(define-public (claim-performance-reward (pool-id uint) (period uint))
  (let
    (
      (participant tx-sender)
      (pool-performance-data (unwrap! (map-get? pool-performance { pool-id: pool-id, period: period }) ERR_POOL_NOT_FOUND))
      (existing-rewards (map-get? participant-rewards { pool-id: pool-id, participant: participant, period: period }))
      (participant-data (unwrap! (contract-call? .Repool get-participant pool-id participant) ERR_NOT_ELIGIBLE))
      
      ;; Calculate reward based on pool performance and participant stake
      (pool-score (get overall-score pool-performance-data))
      (participant-stake (get stake participant-data))
      (base-reward (/ (* pool-score participant-stake) u1000000)) ;; Scale down
      (performance-bonus (if (> pool-score (var-get top-performer-threshold)) (/ base-reward u2) u0))
      (total-reward (+ base-reward performance-bonus))
    )
    (asserts! (is-none existing-rewards) ERR_REWARDS_ALREADY_CLAIMED)
    (asserts! (> total-reward u0) ERR_NOT_ELIGIBLE)
    (asserts! (get data-collected pool-performance-data) ERR_BENCHMARK_NOT_READY)
    
    ;; Transfer reward
    (try! (as-contract (stx-transfer? total-reward tx-sender participant)))
    
    ;; Record reward
    (map-set participant-rewards
      { pool-id: pool-id, participant: participant, period: period }
      {
        performance-bonus: performance-bonus,
        benchmark-bonus: base-reward,
        total-reward: total-reward,
        claimed: true,
        earned-at: stacks-block-height
      }
    )
    (ok total-reward)
  )
)

;; Function to get pool ranking for a period
(define-read-only (get-pool-ranking (pool-id uint) (period uint))
  (match (map-get? pool-performance { pool-id: pool-id, period: period })
    performance
    (let
      (
        (score (get overall-score performance))
        (threshold (var-get top-performer-threshold))
      )
      (some {
        overall-score: score,
        rank-tier: (if (>= score threshold) "top-performer" 
                     (if (>= score u600) "above-average" 
                       (if (>= score u400) "average" "below-average"))),
        eligible-for-rewards: (get data-collected performance)
      })
    )
    none
  )
)

;; Admin function to set benchmark parameters
(define-public (set-benchmark-parameters (period-blocks uint) (min-pools uint) (top-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> period-blocks u0) ERR_INVALID_AMOUNT)
    (asserts! (> min-pools u0) ERR_INVALID_AMOUNT)
    (asserts! (and (> top-threshold u0) (<= top-threshold u1000)) ERR_INVALID_AMOUNT)
    
    (var-set benchmark-period-blocks period-blocks)
    (var-set min-benchmark-pools min-pools)
    (var-set top-performer-threshold top-threshold)
    (ok { period-blocks: period-blocks, min-pools: min-pools, threshold: top-threshold })
  )
)

;; Read-only function to get benchmark statistics
(define-read-only (get-benchmark-stats)
  {
    current-period: (var-get current-benchmark-period),
    reward-pool-balance: (var-get reward-pool-balance),
    period-blocks: (var-get benchmark-period-blocks),
    top-performer-threshold: (var-get top-performer-threshold),
    min-pools-required: (var-get min-benchmark-pools)
  }
)

;; Initialize first benchmark period
(map-set benchmark-periods
  { period: u1 }
  {
    start-block: stacks-block-height,
    end-block: (+ stacks-block-height u1008),
    participating-pools: u0,
    total-reward-distributed: u0,
    network-avg-score: u0,
    completed: false
  }
)
