(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_ORACLE_NOT_FOUND (err u101))
(define-constant ERR_INVALID_SCORE (err u102))
(define-constant ERR_ALREADY_REGISTERED (err u103))
(define-constant ERR_INSUFFICIENT_REPORTS (err u104))
(define-constant ERR_INVALID_ACCURACY (err u105))
(define-constant ERR_ORACLE_INACTIVE (err u106))
(define-constant ERR_INVALID_RESPONSE_TIME (err u107))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u108))
(define-constant ERR_SUBSCRIPTION_EXPIRED (err u109))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u110))
(define-constant ERR_SUBSCRIPTION_ALREADY_EXISTS (err u111))
(define-constant ERR_INVALID_DURATION (err u112))
(define-constant ERR_INVALID_PRICE (err u113))
(define-constant ERR_INSUFFICIENT_BOND (err u114))
(define-constant ERR_BOND_NOT_FOUND (err u115))
(define-constant ERR_BOND_LOCKED (err u116))
(define-constant ERR_INVALID_BOND_AMOUNT (err u117))
(define-constant ERR_SLASHING_NOT_ALLOWED (err u118))
(define-constant ERR_WITHDRAWAL_TOO_EARLY (err u119))

(define-constant MIN_SCORE u0)
(define-constant MAX_SCORE u1000)
(define-constant INITIAL_SCORE u500)
(define-constant MIN_REPORTS_FOR_RATING u5)
(define-constant ACCURACY_WEIGHT u60)
(define-constant RESPONSE_TIME_WEIGHT u25)
(define-constant UPTIME_WEIGHT u15)
(define-constant MAX_RESPONSE_TIME u100)
(define-constant MIN_SUBSCRIPTION_DURATION u1440)
(define-constant MAX_SUBSCRIPTION_DURATION u525600)
(define-constant SUBSCRIPTION_BLOCKS_PER_DAY u144)
(define-constant MIN_BOND_AMOUNT u1000000)
(define-constant SLASHING_ACCURACY_THRESHOLD u300)
(define-constant SLASHING_UPTIME_THRESHOLD u300)
(define-constant SLASHING_RATE u100)
(define-constant REWARD_RATE u50)
(define-constant BOND_LOCK_PERIOD u1440)
(define-constant PERFORMANCE_EVALUATION_PERIOD u288)

(define-data-var next-oracle-id uint u1)
(define-data-var total-oracles uint u0)
(define-data-var next-subscription-id uint u1)
(define-data-var total-subscriptions uint u0)
(define-data-var total-bonded-amount uint u0)

(define-map oracles
  { oracle-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    reputation-score: uint,
    total-reports: uint,
    accurate-reports: uint,
    total-response-time: uint,
    uptime-blocks: uint,
    registration-block: uint,
    last-activity-block: uint,
    is-active: bool
  }
)

(define-map oracle-owners
  { owner: principal }
  { oracle-id: uint }
)

(define-map oracle-ratings
  { oracle-id: uint, rater: principal }
  {
    accuracy-rating: uint,
    response-time-rating: uint,
    uptime-rating: uint,
    block-height: uint
  }
)

(define-map oracle-performance-history
  { oracle-id: uint, block-height: uint }
  {
    accuracy: uint,
    response-time: uint,
    uptime: uint,
    score: uint
  }
)

(define-map oracle-subscription-plans
  { oracle-id: uint }
  {
    daily-price: uint,
    weekly-price: uint,
    monthly-price: uint,
    is-subscription-enabled: bool,
    max-concurrent-subscribers: uint,
    current-subscriber-count: uint
  }
)

(define-map subscriptions
  { subscription-id: uint }
  {
    subscriber: principal,
    oracle-id: uint,
    plan-type: (string-ascii 10),
    start-block: uint,
    end-block: uint,
    price-paid: uint,
    is-active: bool,
    auto-renew: bool,
    renewal-count: uint
  }
)

(define-map subscriber-oracle-map
  { subscriber: principal, oracle-id: uint }
  { subscription-id: uint }
)

(define-map oracle-subscribers
  { oracle-id: uint, subscriber: principal }
  {
    subscription-id: uint,
    total-paid: uint,
    subscription-start: uint
  }
)

(define-map oracle-bonds
  { oracle-id: uint }
  {
    bonded-amount: uint,
    lock-end-block: uint,
    total-slashed: uint,
    total-rewarded: uint,
    last-evaluation-block: uint,
    pending-withdrawal: uint,
    is-bond-active: bool
  }
)

(define-map bond-performance-history
  { oracle-id: uint, evaluation-block: uint }
  {
    accuracy-score: uint,
    uptime-score: uint,
    slash-amount: uint,
    reward-amount: uint,
    final-bond-amount: uint
  }
)

(define-public (register-oracle (name (string-ascii 50)))
  (let
    (
      (oracle-id (var-get next-oracle-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-none (map-get? oracle-owners { owner: tx-sender })) ERR_ALREADY_REGISTERED)
    (map-set oracles
      { oracle-id: oracle-id }
      {
        owner: tx-sender,
        name: name,
        reputation-score: INITIAL_SCORE,
        total-reports: u0,
        accurate-reports: u0,
        total-response-time: u0,
        uptime-blocks: u0,
        registration-block: current-block,
        last-activity-block: current-block,
        is-active: true
      }
    )
    (map-set oracle-owners { owner: tx-sender } { oracle-id: oracle-id })
    (var-set next-oracle-id (+ oracle-id u1))
    (var-set total-oracles (+ (var-get total-oracles) u1))
    (map-set oracle-subscription-plans
      { oracle-id: oracle-id }
      {
        daily-price: u0,
        weekly-price: u0,
        monthly-price: u0,
        is-subscription-enabled: false,
        max-concurrent-subscribers: u0,
        current-subscriber-count: u0
      }
    )
    (map-set oracle-bonds
      { oracle-id: oracle-id }
      {
        bonded-amount: u0,
        lock-end-block: u0,
        total-slashed: u0,
        total-rewarded: u0,
        last-evaluation-block: current-block,
        pending-withdrawal: u0,
        is-bond-active: false
      }
    )
    (ok oracle-id)
  )
)

(define-public (submit-oracle-report (oracle-id uint) (response-time uint) (is-accurate bool))
  (let
    (
      (oracle-data (unwrap! (map-get? oracles { oracle-id: oracle-id }) ERR_ORACLE_NOT_FOUND))
      (current-block stacks-block-height)
      (new-total-reports (+ (get total-reports oracle-data) u1))
      (new-accurate-reports (if is-accurate (+ (get accurate-reports oracle-data) u1) (get accurate-reports oracle-data)))
      (new-total-response-time (+ (get total-response-time oracle-data) response-time))
      (blocks-since-last-activity (- current-block (get last-activity-block oracle-data)))
      (new-uptime-blocks (+ (get uptime-blocks oracle-data) blocks-since-last-activity))
    )
    (asserts! (get is-active oracle-data) ERR_ORACLE_INACTIVE)
    (asserts! (<= response-time MAX_RESPONSE_TIME) ERR_INVALID_RESPONSE_TIME)
    (map-set oracles
      { oracle-id: oracle-id }
      (merge oracle-data {
        total-reports: new-total-reports,
        accurate-reports: new-accurate-reports,
        total-response-time: new-total-response-time,
        uptime-blocks: new-uptime-blocks,
        last-activity-block: current-block
      })
    )
    (if (>= new-total-reports MIN_REPORTS_FOR_RATING)
      (update-reputation-score oracle-id)
      (ok true)
    )
  )
)

(define-public (rate-oracle (oracle-id uint) (accuracy-rating uint) (response-time-rating uint) (uptime-rating uint))
  (let
    (
      (oracle-data (unwrap! (map-get? oracles { oracle-id: oracle-id }) ERR_ORACLE_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (get is-active oracle-data) ERR_ORACLE_INACTIVE)
    (asserts! (and (<= accuracy-rating u100) (>= accuracy-rating u0)) ERR_INVALID_SCORE)
    (asserts! (and (<= response-time-rating u100) (>= response-time-rating u0)) ERR_INVALID_SCORE)
    (asserts! (and (<= uptime-rating u100) (>= uptime-rating u0)) ERR_INVALID_SCORE)
    (asserts! (not (is-eq tx-sender (get owner oracle-data))) ERR_UNAUTHORIZED)
    (map-set oracle-ratings
      { oracle-id: oracle-id, rater: tx-sender }
      {
        accuracy-rating: accuracy-rating,
        response-time-rating: response-time-rating,
        uptime-rating: uptime-rating,
        block-height: current-block
      }
    )
    (update-reputation-score oracle-id)
  )
)

(define-public (deactivate-oracle (oracle-id uint))
  (let
    (
      (oracle-data (unwrap! (map-get? oracles { oracle-id: oracle-id }) ERR_ORACLE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner oracle-data)) ERR_UNAUTHORIZED)
    (map-set oracles
      { oracle-id: oracle-id }
      (merge oracle-data { is-active: false })
    )
    (ok true)
  )
)

(define-public (reactivate-oracle (oracle-id uint))
  (let
    (
      (oracle-data (unwrap! (map-get? oracles { oracle-id: oracle-id }) ERR_ORACLE_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get owner oracle-data)) ERR_UNAUTHORIZED)
    (map-set oracles
      { oracle-id: oracle-id }
      (merge oracle-data { 
        is-active: true,
        last-activity-block: current-block
      })
    )
    (ok true)
  )
)

(define-public (set-subscription-plan (oracle-id uint) (daily-price uint) (weekly-price uint) (monthly-price uint) (max-subscribers uint))
  (let
    (
      (oracle-data (unwrap! (map-get? oracles { oracle-id: oracle-id }) ERR_ORACLE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner oracle-data)) ERR_UNAUTHORIZED)
    (asserts! (get is-active oracle-data) ERR_ORACLE_INACTIVE)
    (asserts! (> max-subscribers u0) ERR_INVALID_PRICE)
    (map-set oracle-subscription-plans
      { oracle-id: oracle-id }
      {
        daily-price: daily-price,
        weekly-price: weekly-price,
        monthly-price: monthly-price,
        is-subscription-enabled: true,
        max-concurrent-subscribers: max-subscribers,
        current-subscriber-count: u0
      }
    )
    (ok true)
  )
)

(define-public (subscribe-to-oracle (oracle-id uint) (plan-type (string-ascii 10)) (duration-blocks uint))
  (let
    (
      (oracle-data (unwrap! (map-get? oracles { oracle-id: oracle-id }) ERR_ORACLE_NOT_FOUND))
      (subscription-plan (unwrap! (map-get? oracle-subscription-plans { oracle-id: oracle-id }) ERR_ORACLE_NOT_FOUND))
      (subscription-id (var-get next-subscription-id))
      (current-block stacks-block-height)
      (end-block (+ current-block duration-blocks))
      (price (get-subscription-price plan-type subscription-plan duration-blocks))
      (existing-subscription (map-get? subscriber-oracle-map { subscriber: tx-sender, oracle-id: oracle-id }))
    )
    (asserts! (get is-active oracle-data) ERR_ORACLE_INACTIVE)
    (asserts! (get is-subscription-enabled subscription-plan) ERR_ORACLE_INACTIVE)
    (asserts! (is-none existing-subscription) ERR_SUBSCRIPTION_ALREADY_EXISTS)
    (asserts! (< (get current-subscriber-count subscription-plan) (get max-concurrent-subscribers subscription-plan)) ERR_ORACLE_NOT_FOUND)
    (asserts! (>= duration-blocks MIN_SUBSCRIPTION_DURATION) ERR_INVALID_DURATION)
    (asserts! (<= duration-blocks MAX_SUBSCRIPTION_DURATION) ERR_INVALID_DURATION)
    (asserts! (> price u0) ERR_INVALID_PRICE)
    (asserts! (>= (stx-get-balance tx-sender) price) ERR_INSUFFICIENT_PAYMENT)
    (try! (stx-transfer? price tx-sender (get owner oracle-data)))
    (map-set subscriptions
      { subscription-id: subscription-id }
      {
        subscriber: tx-sender,
        oracle-id: oracle-id,
        plan-type: plan-type,
        start-block: current-block,
        end-block: end-block,
        price-paid: price,
        is-active: true,
        auto-renew: false,
        renewal-count: u0
      }
    )
    (map-set subscriber-oracle-map
      { subscriber: tx-sender, oracle-id: oracle-id }
      { subscription-id: subscription-id }
    )
    (map-set oracle-subscribers
      { oracle-id: oracle-id, subscriber: tx-sender }
      {
        subscription-id: subscription-id,
        total-paid: price,
        subscription-start: current-block
      }
    )
    (map-set oracle-subscription-plans
      { oracle-id: oracle-id }
      (merge subscription-plan { current-subscriber-count: (+ (get current-subscriber-count subscription-plan) u1) })
    )
    (var-set next-subscription-id (+ subscription-id u1))
    (var-set total-subscriptions (+ (var-get total-subscriptions) u1))
    (ok subscription-id)
  )
)

(define-public (renew-subscription (subscription-id uint) (duration-blocks uint))
  (let
    (
      (subscription-data (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND))
      (oracle-data (unwrap! (map-get? oracles { oracle-id: (get oracle-id subscription-data) }) ERR_ORACLE_NOT_FOUND))
      (subscription-plan (unwrap! (map-get? oracle-subscription-plans { oracle-id: (get oracle-id subscription-data) }) ERR_ORACLE_NOT_FOUND))
      (current-block stacks-block-height)
      (new-end-block (+ (get end-block subscription-data) duration-blocks))
      (price (get-subscription-price (get plan-type subscription-data) subscription-plan duration-blocks))
      (current-subscriber-data (unwrap! (map-get? oracle-subscribers { oracle-id: (get oracle-id subscription-data), subscriber: tx-sender }) ERR_SUBSCRIPTION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription-data)) ERR_UNAUTHORIZED)
    (asserts! (get is-active subscription-data) ERR_SUBSCRIPTION_EXPIRED)
    (asserts! (get is-active oracle-data) ERR_ORACLE_INACTIVE)
    (asserts! (>= duration-blocks MIN_SUBSCRIPTION_DURATION) ERR_INVALID_DURATION)
    (asserts! (<= duration-blocks MAX_SUBSCRIPTION_DURATION) ERR_INVALID_DURATION)
    (asserts! (> price u0) ERR_INVALID_PRICE)
    (asserts! (>= (stx-get-balance tx-sender) price) ERR_INSUFFICIENT_PAYMENT)
    (try! (stx-transfer? price tx-sender (get owner oracle-data)))
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription-data {
        end-block: new-end-block,
        renewal-count: (+ (get renewal-count subscription-data) u1)
      })
    )
    (map-set oracle-subscribers
      { oracle-id: (get oracle-id subscription-data), subscriber: tx-sender }
      (merge current-subscriber-data {
        total-paid: (+ (get total-paid current-subscriber-data) price)
      })
    )
    (ok true)
  )
)

(define-public (cancel-subscription (subscription-id uint))
  (let
    (
      (subscription-data (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) ERR_SUBSCRIPTION_NOT_FOUND))
      (subscription-plan (unwrap! (map-get? oracle-subscription-plans { oracle-id: (get oracle-id subscription-data) }) ERR_ORACLE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription-data)) ERR_UNAUTHORIZED)
    (asserts! (get is-active subscription-data) ERR_SUBSCRIPTION_EXPIRED)
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription-data { is-active: false })
    )
    (map-delete subscriber-oracle-map { subscriber: tx-sender, oracle-id: (get oracle-id subscription-data) })
    (map-delete oracle-subscribers { oracle-id: (get oracle-id subscription-data), subscriber: tx-sender })
    (map-set oracle-subscription-plans
      { oracle-id: (get oracle-id subscription-data) }
      (merge subscription-plan { current-subscriber-count: (- (get current-subscriber-count subscription-plan) u1) })
    )
    (ok true)
  )
)

(define-public (deposit-bond (oracle-id uint) (bond-amount uint))
  (let
    (
      (oracle-data (unwrap! (map-get? oracles { oracle-id: oracle-id }) ERR_ORACLE_NOT_FOUND))
      (bond-data (unwrap! (map-get? oracle-bonds { oracle-id: oracle-id }) ERR_BOND_NOT_FOUND))
      (current-block stacks-block-height)
      (new-lock-end (+ current-block BOND_LOCK_PERIOD))
      (new-bonded-amount (+ (get bonded-amount bond-data) bond-amount))
    )
    (asserts! (is-eq tx-sender (get owner oracle-data)) ERR_UNAUTHORIZED)
    (asserts! (get is-active oracle-data) ERR_ORACLE_INACTIVE)
    (asserts! (>= bond-amount MIN_BOND_AMOUNT) ERR_INVALID_BOND_AMOUNT)
    (asserts! (>= (stx-get-balance tx-sender) bond-amount) ERR_INSUFFICIENT_PAYMENT)
    (try! (stx-transfer? bond-amount tx-sender (as-contract tx-sender)))
    (map-set oracle-bonds
      { oracle-id: oracle-id }
      (merge bond-data {
        bonded-amount: new-bonded-amount,
        lock-end-block: new-lock-end,
        is-bond-active: true
      })
    )
    (var-set total-bonded-amount (+ (var-get total-bonded-amount) bond-amount))
    (ok true)
  )
)

(define-public (withdraw-bond (oracle-id uint) (withdraw-amount uint))
  (let
    (
      (oracle-data (unwrap! (map-get? oracles { oracle-id: oracle-id }) ERR_ORACLE_NOT_FOUND))
      (bond-data (unwrap! (map-get? oracle-bonds { oracle-id: oracle-id }) ERR_BOND_NOT_FOUND))
      (current-block stacks-block-height)
      (available-amount (- (get bonded-amount bond-data) (get pending-withdrawal bond-data)))
    )
    (asserts! (is-eq tx-sender (get owner oracle-data)) ERR_UNAUTHORIZED)
    (asserts! (get is-bond-active bond-data) ERR_BOND_NOT_FOUND)
    (asserts! (> current-block (get lock-end-block bond-data)) ERR_WITHDRAWAL_TOO_EARLY)
    (asserts! (<= withdraw-amount available-amount) ERR_INSUFFICIENT_BOND)
    (asserts! (>= available-amount withdraw-amount) ERR_INSUFFICIENT_BOND)
    (try! (as-contract (stx-transfer? withdraw-amount tx-sender (get owner oracle-data))))
    (map-set oracle-bonds
      { oracle-id: oracle-id }
      (merge bond-data {
        bonded-amount: (- (get bonded-amount bond-data) withdraw-amount),
        is-bond-active: (> (- (get bonded-amount bond-data) withdraw-amount) u0)
      })
    )
    (var-set total-bonded-amount (- (var-get total-bonded-amount) withdraw-amount))
    (ok true)
  )
)

(define-public (evaluate-oracle-performance (oracle-id uint))
  (let
    (
      (oracle-data (unwrap! (map-get? oracles { oracle-id: oracle-id }) ERR_ORACLE_NOT_FOUND))
      (bond-data (unwrap! (map-get? oracle-bonds { oracle-id: oracle-id }) ERR_BOND_NOT_FOUND))
      (current-block stacks-block-height)
      (blocks-since-last-eval (- current-block (get last-evaluation-block bond-data)))
      (accuracy-score (calculate-accuracy-score oracle-data))
      (uptime-score (calculate-uptime-score oracle-data))
      (slash-amount (calculate-slash-amount bond-data accuracy-score uptime-score))
      (reward-amount (calculate-reward-amount bond-data accuracy-score uptime-score))
      (net-adjustment (if (> slash-amount reward-amount) (- slash-amount reward-amount) (- reward-amount slash-amount)))
      (is-slash (> slash-amount reward-amount))
      (new-bonded-amount (if is-slash 
        (if (> net-adjustment (get bonded-amount bond-data)) u0 (- (get bonded-amount bond-data) net-adjustment))
        (+ (get bonded-amount bond-data) net-adjustment)
      ))
    )
    (asserts! (get is-bond-active bond-data) ERR_BOND_NOT_FOUND)
    (asserts! (>= blocks-since-last-eval PERFORMANCE_EVALUATION_PERIOD) ERR_SLASHING_NOT_ALLOWED)
    (asserts! (get is-active oracle-data) ERR_ORACLE_INACTIVE)
    (map-set bond-performance-history
      { oracle-id: oracle-id, evaluation-block: current-block }
      {
        accuracy-score: accuracy-score,
        uptime-score: uptime-score,
        slash-amount: (if is-slash net-adjustment u0),
        reward-amount: (if is-slash u0 net-adjustment),
        final-bond-amount: new-bonded-amount
      }
    )
    (map-set oracle-bonds
      { oracle-id: oracle-id }
      (merge bond-data {
        bonded-amount: new-bonded-amount,
        total-slashed: (+ (get total-slashed bond-data) (if is-slash net-adjustment u0)),
        total-rewarded: (+ (get total-rewarded bond-data) (if is-slash u0 net-adjustment)),
        last-evaluation-block: current-block,
        is-bond-active: (> new-bonded-amount u0)
      })
    )
    (if is-slash
      (var-set total-bonded-amount (- (var-get total-bonded-amount) net-adjustment))
      (var-set total-bonded-amount (+ (var-get total-bonded-amount) net-adjustment))
    )
    (ok { slashed: is-slash, amount: net-adjustment })
  )
)

(define-public (force-slash-oracle (oracle-id uint) (slash-amount uint))
  (let
    (
      (oracle-data (unwrap! (map-get? oracles { oracle-id: oracle-id }) ERR_ORACLE_NOT_FOUND))
      (bond-data (unwrap! (map-get? oracle-bonds { oracle-id: oracle-id }) ERR_BOND_NOT_FOUND))
      (current-block stacks-block-height)
      (new-bonded-amount (if (> slash-amount (get bonded-amount bond-data)) u0 (- (get bonded-amount bond-data) slash-amount)))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (get is-bond-active bond-data) ERR_BOND_NOT_FOUND)
    (asserts! (<= slash-amount (get bonded-amount bond-data)) ERR_INSUFFICIENT_BOND)
    (map-set oracle-bonds
      { oracle-id: oracle-id }
      (merge bond-data {
        bonded-amount: new-bonded-amount,
        total-slashed: (+ (get total-slashed bond-data) slash-amount),
        is-bond-active: (> new-bonded-amount u0)
      })
    )
    (var-set total-bonded-amount (- (var-get total-bonded-amount) slash-amount))
    (ok true)
  )
)

(define-private (calculate-slash-amount (bond-data (tuple (bonded-amount uint) (lock-end-block uint) (total-slashed uint) (total-rewarded uint) (last-evaluation-block uint) (pending-withdrawal uint) (is-bond-active bool))) (accuracy uint) (uptime uint))
  (let
    (
      (accuracy-penalty (if (< accuracy SLASHING_ACCURACY_THRESHOLD) (/ (* (get bonded-amount bond-data) SLASHING_RATE) u1000) u0))
      (uptime-penalty (if (< uptime SLASHING_UPTIME_THRESHOLD) (/ (* (get bonded-amount bond-data) SLASHING_RATE) u1000) u0))
    )
    (+ accuracy-penalty uptime-penalty)
  )
)

(define-private (calculate-reward-amount (bond-data (tuple (bonded-amount uint) (lock-end-block uint) (total-slashed uint) (total-rewarded uint) (last-evaluation-block uint) (pending-withdrawal uint) (is-bond-active bool))) (accuracy uint) (uptime uint))
  (if (and (> accuracy u800) (> uptime u800))
    (/ (* (get bonded-amount bond-data) REWARD_RATE) u1000)
    u0
  )
)

(define-private (get-subscription-price (plan-type (string-ascii 10)) (subscription-plan (tuple (daily-price uint) (weekly-price uint) (monthly-price uint) (is-subscription-enabled bool) (max-concurrent-subscribers uint) (current-subscriber-count uint))) (duration-blocks uint))
  (if (is-eq plan-type "daily")
    (/ (* (get daily-price subscription-plan) duration-blocks) SUBSCRIPTION_BLOCKS_PER_DAY)
    (if (is-eq plan-type "weekly")
      (/ (* (get weekly-price subscription-plan) duration-blocks) (* SUBSCRIPTION_BLOCKS_PER_DAY u7))
      (if (is-eq plan-type "monthly")
        (/ (* (get monthly-price subscription-plan) duration-blocks) (* SUBSCRIPTION_BLOCKS_PER_DAY u30))
        u0
      )
    )
  )
)

(define-private (update-reputation-score (oracle-id uint))
  (let
    (
      (oracle-data (unwrap! (map-get? oracles { oracle-id: oracle-id }) ERR_ORACLE_NOT_FOUND))
      (accuracy-score (calculate-accuracy-score oracle-data))
      (response-time-score (calculate-response-time-score oracle-data))
      (uptime-score (calculate-uptime-score oracle-data))
      (weighted-score (calculate-weighted-score accuracy-score response-time-score uptime-score))
      (current-block stacks-block-height)
    )
    (map-set oracle-performance-history
      { oracle-id: oracle-id, block-height: current-block }
      {
        accuracy: accuracy-score,
        response-time: response-time-score,
        uptime: uptime-score,
        score: weighted-score
      }
    )
    (map-set oracles
      { oracle-id: oracle-id }
      (merge oracle-data { reputation-score: weighted-score })
    )
    (ok true)
  )
)

(define-private (calculate-accuracy-score (oracle-data (tuple (owner principal) (name (string-ascii 50)) (reputation-score uint) (total-reports uint) (accurate-reports uint) (total-response-time uint) (uptime-blocks uint) (registration-block uint) (last-activity-block uint) (is-active bool))))
  (if (> (get total-reports oracle-data) u0)
    (/ (* (get accurate-reports oracle-data) u1000) (get total-reports oracle-data))
    u500
  )
)

(define-private (calculate-response-time-score (oracle-data (tuple (owner principal) (name (string-ascii 50)) (reputation-score uint) (total-reports uint) (accurate-reports uint) (total-response-time uint) (uptime-blocks uint) (registration-block uint) (last-activity-block uint) (is-active bool))))
  (if (> (get total-reports oracle-data) u0)
    (let
      (
        (avg-response-time (/ (get total-response-time oracle-data) (get total-reports oracle-data)))
      )
      (if (<= avg-response-time MAX_RESPONSE_TIME)
        (- u1000 (* avg-response-time u10))
        u0
      )
    )
    u500
  )
)

(define-private (calculate-uptime-score (oracle-data (tuple (owner principal) (name (string-ascii 50)) (reputation-score uint) (total-reports uint) (accurate-reports uint) (total-response-time uint) (uptime-blocks uint) (registration-block uint) (last-activity-block uint) (is-active bool))))
  (let
    (
      (total-blocks-since-registration (- stacks-block-height (get registration-block oracle-data)))
    )
    (if (> total-blocks-since-registration u0)
      (/ (* (get uptime-blocks oracle-data) u1000) total-blocks-since-registration)
      u1000
    )
  )
)

(define-private (calculate-weighted-score (accuracy uint) (response-time uint) (uptime uint))
  (let
    (
      (weighted-accuracy (/ (* accuracy ACCURACY_WEIGHT) u100))
      (weighted-response-time (/ (* response-time RESPONSE_TIME_WEIGHT) u100))
      (weighted-uptime (/ (* uptime UPTIME_WEIGHT) u100))
      (total-score (+ weighted-accuracy (+ weighted-response-time weighted-uptime)))
    )
    (if (> total-score MAX_SCORE) MAX_SCORE total-score)
  )
)

(define-read-only (get-oracle-info (oracle-id uint))
  (map-get? oracles { oracle-id: oracle-id })
)

(define-read-only (get-oracle-by-owner (owner principal))
  (match (map-get? oracle-owners { owner: owner })
    oracle-record (map-get? oracles { oracle-id: (get oracle-id oracle-record) })
    none
  )
)

(define-read-only (get-oracle-rating (oracle-id uint) (rater principal))
  (map-get? oracle-ratings { oracle-id: oracle-id, rater: rater })
)

(define-read-only (get-oracle-performance (oracle-id uint) (block-block uint))
  (map-get? oracle-performance-history { oracle-id: oracle-id, block-height: stacks-block-height })
)

(define-read-only (get-total-oracles)
  (var-get total-oracles)
)

(define-read-only (get-next-oracle-id)
  (var-get next-oracle-id)
)

(define-read-only (is-oracle-active (oracle-id uint))
  (match (map-get? oracles { oracle-id: oracle-id })
    oracle-data (get is-active oracle-data)
    false
  )
)

(define-read-only (get-oracle-reputation-score (oracle-id uint))
  (match (map-get? oracles { oracle-id: oracle-id })
    oracle-data (get reputation-score oracle-data)
    u0
  )
)

(define-read-only (get-subscription-plan (oracle-id uint))
  (map-get? oracle-subscription-plans { oracle-id: oracle-id })
)

(define-read-only (get-subscription-info (subscription-id uint))
  (map-get? subscriptions { subscription-id: subscription-id })
)

(define-read-only (get-subscriber-subscription (subscriber principal) (oracle-id uint))
  (match (map-get? subscriber-oracle-map { subscriber: subscriber, oracle-id: oracle-id })
    subscription-record (map-get? subscriptions { subscription-id: (get subscription-id subscription-record) })
    none
  )
)

(define-read-only (is-subscription-active (subscription-id uint))
  (match (map-get? subscriptions { subscription-id: subscription-id })
    subscription-data (and (get is-active subscription-data) (> (get end-block subscription-data) stacks-block-height))
    false
  )
)

(define-read-only (get-oracle-subscriber-info (oracle-id uint) (subscriber principal))
  (map-get? oracle-subscribers { oracle-id: oracle-id, subscriber: subscriber })
)

(define-read-only (get-total-subscriptions)
  (var-get total-subscriptions)
)

(define-read-only (get-next-subscription-id)
  (var-get next-subscription-id)
)

(define-read-only (calculate-subscription-cost (oracle-id uint) (plan-type (string-ascii 10)) (duration-blocks uint))
  (match (map-get? oracle-subscription-plans { oracle-id: oracle-id })
    subscription-plan (get-subscription-price plan-type subscription-plan duration-blocks)
    u0
  )
)

(define-read-only (is-subscription-available (oracle-id uint))
  (match (map-get? oracle-subscription-plans { oracle-id: oracle-id })
    subscription-plan (and 
      (get is-subscription-enabled subscription-plan)
      (< (get current-subscriber-count subscription-plan) (get max-concurrent-subscribers subscription-plan))
    )
    false
  )
)

(define-read-only (get-oracle-bond-info (oracle-id uint))
  (map-get? oracle-bonds { oracle-id: oracle-id })
)

(define-read-only (get-bond-performance-history (oracle-id uint) (evaluation-block uint))
  (map-get? bond-performance-history { oracle-id: oracle-id, evaluation-block: evaluation-block })
)

(define-read-only (calculate-oracle-risk-score (oracle-id uint))
  (match (map-get? oracle-bonds { oracle-id: oracle-id })
    bond-data (let
      (
        (bonded-amount (get bonded-amount bond-data))
        (total-slashed (get total-slashed bond-data))
        (total-rewarded (get total-rewarded bond-data))
        (slash-rate (if (> bonded-amount u0) (/ (* total-slashed u1000) bonded-amount) u0))
      )
      (if (> slash-rate u500) u0 (- u1000 slash-rate))
    )
    u0
  )
)

(define-read-only (get-total-bonded-amount)
  (var-get total-bonded-amount)
)

(define-read-only (is-bond-locked (oracle-id uint))
  (match (map-get? oracle-bonds { oracle-id: oracle-id })
    bond-data (> (get lock-end-block bond-data) stacks-block-height)
    false
  )
)

(define-read-only (get-bond-withdrawal-time (oracle-id uint))
  (match (map-get? oracle-bonds { oracle-id: oracle-id })
    bond-data (get lock-end-block bond-data)
    u0
  )
)

(define-read-only (calculate-potential-slash (oracle-id uint))
  (match (map-get? oracles { oracle-id: oracle-id })
    oracle-data (match (map-get? oracle-bonds { oracle-id: oracle-id })
      bond-data (let
        (
          (accuracy-score (calculate-accuracy-score oracle-data))
          (uptime-score (calculate-uptime-score oracle-data))
        )
        (calculate-slash-amount bond-data accuracy-score uptime-score)
      )
      u0
    )
    u0
  )
)

(define-read-only (get-oracle-bond-status (oracle-id uint))
  (match (map-get? oracle-bonds { oracle-id: oracle-id })
    bond-data (some {
      bonded: (get bonded-amount bond-data),
      locked: (> (get lock-end-block bond-data) stacks-block-height),
      active: (get is-bond-active bond-data),
      slashed: (get total-slashed bond-data),
      rewarded: (get total-rewarded bond-data)
    })
    none
  )
)

;; =====================================
;; ORACLE DATA FEED AGGREGATION SYSTEM
;; =====================================

;; Data Feed Aggregation Constants
(define-constant ERR_FEED_NOT_FOUND (err u200))
(define-constant ERR_FEED_ALREADY_EXISTS (err u201))
(define-constant ERR_ORACLE_NOT_IN_FEED (err u202))
(define-constant ERR_INVALID_DATA_VALUE (err u203))
(define-constant ERR_FEED_INACTIVE (err u204))
(define-constant ERR_INSUFFICIENT_ORACLES (err u205))
(define-constant ERR_CONSENSUS_NOT_REACHED (err u206))
(define-constant ERR_SUBMISSION_WINDOW_CLOSED (err u207))

;; Feed system variables  
(define-data-var next-feed-id uint u1)
(define-data-var min-oracles-per-feed uint u3)
(define-data-var consensus-threshold uint u67) ;; 67% agreement required
(define-data-var submission-window-blocks uint u10) ;; 10 blocks for submissions

;; Data feed definitions
(define-map data-feeds
    { feed-id: uint }
    {
        name: (string-ascii 50),
        description: (string-ascii 200), 
        creator: principal,
        min-oracles: uint,
        max-oracles: uint,
        oracle-count: uint,
        is-active: bool,
        creation-block: uint,
        last-update: uint
    }
)

;; Track which oracles participate in which feeds
(define-map feed-oracles
    { feed-id: uint, oracle-id: uint }
    {
        weight: uint, ;; Oracle weight in consensus (1-100)
        join-block: uint,
        total-submissions: uint,
        accurate-submissions: uint,
        is-active: bool
    }
)

;; Store data submissions for aggregation
(define-map data-submissions
    { feed-id: uint, round-id: uint, oracle-id: uint }
    {
        value: uint,
        confidence: uint, ;; Oracle's confidence in their data (1-100)
        submission-block: uint,
        weight: uint
    }
)

;; Aggregated feed data
(define-map aggregated-data
    { feed-id: uint, round-id: uint }
    {
        consensus-value: uint,
        confidence-score: uint,
        participating-oracles: uint,
        submission-count: uint,
        finalized-at: uint,
        variance: uint
    }
)

;; Round management
(define-map feed-rounds
    { feed-id: uint }
    {
        current-round: uint,
        submission-deadline: uint,
        finalization-block: uint
    }
)

;; Create new data feed
(define-public (create-data-feed (name (string-ascii 50)) (description (string-ascii 200)) (min-oracles uint) (max-oracles uint))
    (let (
        (feed-id (var-get next-feed-id))
        (current-block stacks-block-height)
    )
        (asserts! (and (>= min-oracles (var-get min-oracles-per-feed)) (<= min-oracles max-oracles)) ERR_INSUFFICIENT_ORACLES)
        (asserts! (<= max-oracles u20) ERR_INVALID_DATA_VALUE) ;; Max 20 oracles per feed
        
        (map-set data-feeds
            { feed-id: feed-id }
            {
                name: name,
                description: description,
                creator: tx-sender,
                min-oracles: min-oracles,
                max-oracles: max-oracles,
                oracle-count: u0,
                is-active: true,
                creation-block: current-block,
                last-update: current-block
            }
        )
        
        (map-set feed-rounds
            { feed-id: feed-id }
            {
                current-round: u1,
                submission-deadline: (+ current-block (var-get submission-window-blocks)),
                finalization-block: u0
            }
        )
        
        (var-set next-feed-id (+ feed-id u1))
        (ok feed-id)
    )
)

;; Add oracle to data feed
(define-public (add-oracle-to-feed (feed-id uint) (oracle-id uint) (weight uint))
    (let (
        (feed-data (unwrap! (map-get? data-feeds { feed-id: feed-id }) ERR_FEED_NOT_FOUND))
        (oracle-data (unwrap! (map-get? oracles { oracle-id: oracle-id }) ERR_ORACLE_NOT_FOUND))
        (current-block stacks-block-height)
    )
        (asserts! (is-eq tx-sender (get creator feed-data)) ERR_UNAUTHORIZED)
        (asserts! (get is-active feed-data) ERR_FEED_INACTIVE)
        (asserts! (get is-active oracle-data) ERR_ORACLE_INACTIVE)
        (asserts! (< (get oracle-count feed-data) (get max-oracles feed-data)) ERR_INSUFFICIENT_ORACLES)
        (asserts! (and (>= weight u1) (<= weight u100)) ERR_INVALID_DATA_VALUE)
        (asserts! (is-none (map-get? feed-oracles { feed-id: feed-id, oracle-id: oracle-id })) ERR_ALREADY_REGISTERED)
        
        (map-set feed-oracles
            { feed-id: feed-id, oracle-id: oracle-id }
            {
                weight: weight,
                join-block: current-block,
                total-submissions: u0,
                accurate-submissions: u0,
                is-active: true
            }
        )
        
        (map-set data-feeds
            { feed-id: feed-id }
            (merge feed-data { oracle-count: (+ (get oracle-count feed-data) u1) })
        )
        
        (ok true)
    )
)

;; Submit data to feed
(define-public (submit-feed-data (feed-id uint) (value uint) (confidence uint))
    (let (
        (feed-data (unwrap! (map-get? data-feeds { feed-id: feed-id }) ERR_FEED_NOT_FOUND))
        (oracle-owner (unwrap! (map-get? oracle-owners { owner: tx-sender }) ERR_ORACLE_NOT_FOUND))
        (oracle-id (get oracle-id oracle-owner))
        (oracle-feed (unwrap! (map-get? feed-oracles { feed-id: feed-id, oracle-id: oracle-id }) ERR_ORACLE_NOT_IN_FEED))
        (round-data (unwrap! (map-get? feed-rounds { feed-id: feed-id }) ERR_FEED_NOT_FOUND))
        (current-round (get current-round round-data))
        (current-block stacks-block-height)
    )
        (asserts! (get is-active feed-data) ERR_FEED_INACTIVE)
        (asserts! (get is-active oracle-feed) ERR_ORACLE_INACTIVE)
        (asserts! (and (>= confidence u1) (<= confidence u100)) ERR_INVALID_DATA_VALUE)
        (asserts! (< current-block (get submission-deadline round-data)) ERR_SUBMISSION_WINDOW_CLOSED)
        (asserts! (is-none (map-get? data-submissions { feed-id: feed-id, round-id: current-round, oracle-id: oracle-id })) ERR_ALREADY_REGISTERED)
        
        (map-set data-submissions
            { feed-id: feed-id, round-id: current-round, oracle-id: oracle-id }
            {
                value: value,
                confidence: confidence,
                submission-block: current-block,
                weight: (get weight oracle-feed)
            }
        )
        
        (map-set feed-oracles
            { feed-id: feed-id, oracle-id: oracle-id }
            (merge oracle-feed { total-submissions: (+ (get total-submissions oracle-feed) u1) })
        )
        
        (ok true)
    )
)

;; Finalize feed round and calculate aggregated value
(define-public (finalize-feed-round (feed-id uint))
    (let (
        (feed-data (unwrap! (map-get? data-feeds { feed-id: feed-id }) ERR_FEED_NOT_FOUND))
        (round-data (unwrap! (map-get? feed-rounds { feed-id: feed-id }) ERR_FEED_NOT_FOUND))
        (current-round (get current-round round-data))
        (current-block stacks-block-height)
        (submission-count (count-round-submissions feed-id current-round))
    )
        (asserts! (get is-active feed-data) ERR_FEED_INACTIVE)
        (asserts! (> current-block (get submission-deadline round-data)) ERR_SUBMISSION_WINDOW_CLOSED)
        (asserts! (>= submission-count (get min-oracles feed-data)) ERR_INSUFFICIENT_ORACLES)
        (asserts! (is-eq (get finalization-block round-data) u0) ERR_ALREADY_REGISTERED) ;; Not already finalized
        
        (let (
            (consensus-result (calculate-consensus feed-id current-round))
            (consensus-value (get consensus-value consensus-result))
            (confidence-score (get confidence-score consensus-result))
            (variance (get variance consensus-result))
        )
            (map-set aggregated-data
                { feed-id: feed-id, round-id: current-round }
                {
                    consensus-value: consensus-value,
                    confidence-score: confidence-score,
                    participating-oracles: submission-count,
                    submission-count: submission-count,
                    finalized-at: current-block,
                    variance: variance
                }
            )
            
            (map-set feed-rounds
                { feed-id: feed-id }
                (merge round-data {
                    finalization-block: current-block
                })
            )
            
            (map-set data-feeds
                { feed-id: feed-id }
                (merge feed-data { last-update: current-block })
            )
            
            (ok { consensus-value: consensus-value, confidence: confidence-score })
        )
    )
)

;; Start new feed round
(define-public (start-new-round (feed-id uint))
    (let (
        (feed-data (unwrap! (map-get? data-feeds { feed-id: feed-id }) ERR_FEED_NOT_FOUND))
        (round-data (unwrap! (map-get? feed-rounds { feed-id: feed-id }) ERR_FEED_NOT_FOUND))
        (current-block stacks-block-height)
        (new-round (+ (get current-round round-data) u1))
    )
        (asserts! (or (is-eq tx-sender (get creator feed-data)) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
        (asserts! (get is-active feed-data) ERR_FEED_INACTIVE)
        (asserts! (> (get finalization-block round-data) u0) ERR_CONSENSUS_NOT_REACHED) ;; Previous round must be finalized
        
        (map-set feed-rounds
            { feed-id: feed-id }
            {
                current-round: new-round,
                submission-deadline: (+ current-block (var-get submission-window-blocks)),
                finalization-block: u0
            }
        )
        
        (ok new-round)
    )
)

;; Private helper functions
(define-private (count-round-submissions (feed-id uint) (round-id uint))
    ;; In a real implementation, this would iterate through submissions
    ;; For simplicity, we'll estimate based on oracle count
    (match (map-get? data-feeds { feed-id: feed-id })
        feed-data (get oracle-count feed-data)
        u0
    )
)

(define-private (calculate-consensus (feed-id uint) (round-id uint))
    ;; Simplified consensus calculation - in practice would need more complex aggregation
    { 
        consensus-value: u1000, ;; Placeholder - would calculate weighted median/average
        confidence-score: u85,  ;; Based on agreement level and oracle weights
        variance: u50           ;; Measure of data spread
    }
)

;; Read-only functions for feed system
(define-read-only (get-data-feed (feed-id uint))
    (map-get? data-feeds { feed-id: feed-id })
)

(define-read-only (get-feed-oracle-info (feed-id uint) (oracle-id uint))
    (map-get? feed-oracles { feed-id: feed-id, oracle-id: oracle-id })
)

(define-read-only (get-feed-round-data (feed-id uint))
    (map-get? feed-rounds { feed-id: feed-id })
)

(define-read-only (get-aggregated-value (feed-id uint) (round-id uint))
    (map-get? aggregated-data { feed-id: feed-id, round-id: round-id })
)

(define-read-only (get-oracle-submission (feed-id uint) (round-id uint) (oracle-id uint))
    (map-get? data-submissions { feed-id: feed-id, round-id: round-id, oracle-id: oracle-id })
)

;; Get latest aggregated value for a feed
(define-read-only (get-latest-feed-value (feed-id uint))
    (match (map-get? feed-rounds { feed-id: feed-id })
        round-data
        (if (> (get finalization-block round-data) u0)
            (map-get? aggregated-data { feed-id: feed-id, round-id: (get current-round round-data) })
            none
        )
        none
    )
)

;; Check if oracle can submit to feed in current round
(define-read-only (can-submit-to-feed (feed-id uint) (oracle-id uint))
    (match (map-get? feed-oracles { feed-id: feed-id, oracle-id: oracle-id })
        oracle-feed
        (match (map-get? feed-rounds { feed-id: feed-id })
            round-data
            (and 
                (get is-active oracle-feed)
                (< stacks-block-height (get submission-deadline round-data))
                (is-none (map-get? data-submissions { feed-id: feed-id, round-id: (get current-round round-data), oracle-id: oracle-id }))
            )
            false
        )
        false
    )
)

;; Get feed statistics
(define-read-only (get-feed-statistics (feed-id uint))
    (match (map-get? data-feeds { feed-id: feed-id })
        feed-data
        (some {
            total-oracles: (get oracle-count feed-data),
            is-active: (get is-active feed-data),
            blocks-since-update: (- stacks-block-height (get last-update feed-data)),
            creation-age: (- stacks-block-height (get creation-block feed-data))
        })
        none
    )
)


