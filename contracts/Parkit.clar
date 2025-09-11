(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-space-unavailable (err u104))
(define-constant err-reservation-expired (err u105))
(define-constant err-invalid-duration (err u106))
(define-constant err-unauthorized (err u107))
(define-constant err-already-reviewed (err u108))
(define-constant err-invalid-rating (err u109))
(define-constant err-review-not-allowed (err u110))
(define-constant err-subscription-not-found (err u111))
(define-constant err-subscription-expired (err u112))
(define-constant err-subscription-already-exists (err u113))
(define-constant err-invalid-subscription-type (err u114))
(define-constant err-insufficient-subscription-time (err u115))
(define-constant err-loyalty-not-found (err u116))
(define-constant err-loyalty-already-exists (err u117))
(define-constant err-insufficient-loyalty-points (err u118))
(define-constant err-invalid-tier (err u119))
(define-constant err-reward-already-claimed (err u120))

(define-fungible-token parkit-token)

(define-data-var total-spaces uint u0)
(define-data-var reservation-counter uint u0)
(define-data-var token-price uint u1000000)
(define-data-var platform-fee uint u50000)
(define-data-var review-counter uint u0)
(define-data-var subscription-counter uint u0)
(define-data-var loyalty-counter uint u0)

(define-map parking-spaces
  { space-id: uint }
  {
    owner: principal,
    location: (string-ascii 100),
    hourly-rate: uint,
    is-available: bool,
    created-at: uint,
    average-rating: uint,
    total-reviews: uint
  }
)

(define-map reservations
  { reservation-id: uint }
  {
    space-id: uint,
    renter: principal,
    start-time: uint,
    end-time: uint,
    total-cost: uint,
    is-active: bool,
    tokens-earned: uint
  }
)

(define-map user-stats
  { user: principal }
  {
    total-reservations: uint,
    total-spent: uint,
    tokens-earned: uint,
    spaces-owned: uint
  }
)

(define-map space-earnings
  { space-id: uint }
  { total-earned: uint, total-hours: uint }
)

(define-map reviews
  { review-id: uint }
  {
    space-id: uint,
    reviewer: principal,
    reservation-id: uint,
    rating: uint,
    comment: (string-ascii 500),
    created-at: uint,
    is-verified: bool
  }
)

(define-map user-reviews
  { user: principal, space-id: uint }
  { review-id: uint, has-reviewed: bool }
)

(define-map space-ratings
  { space-id: uint }
  {
    total-rating-points: uint,
    total-reviews: uint,
    five-star: uint,
    four-star: uint,
    three-star: uint,
    two-star: uint,
    one-star: uint
  }
)

(define-map subscriptions
  { subscription-id: uint }
  {
    subscriber: principal,
    space-id: uint,
    subscription-type: (string-ascii 20),
    start-time: uint,
    end-time: uint,
    monthly-cost: uint,
    total-paid: uint,
    hours-remaining: uint,
    max-daily-hours: uint,
    is-active: bool,
    auto-renew: bool,
    transferable: bool
  }
)

(define-map user-subscriptions
  { user: principal, space-id: uint }
  { subscription-id: uint, is-subscribed: bool }
)

(define-map subscription-usage
  { subscription-id: uint, usage-date: uint }
  { hours-used: uint, reservations-count: uint }
)

(define-map subscription-plans
  { plan-type: (string-ascii 20) }
  {
    duration-blocks: uint,
    discount-percentage: uint,
    max-daily-hours: uint,
    transferable: bool,
    priority-booking: bool
  }
)

(define-map loyalty-members
  { user: principal }
  {
    loyalty-id: uint,
    tier: (string-ascii 15),
    total-points: uint,
    points-earned-this-month: uint,
    tier-upgrade-block: uint,
    consecutive-months: uint,
    bonus-multiplier: uint,
    total-bonus-tokens: uint,
    last-activity-block: uint,
    monthly-reset-block: uint
  }
)

(define-map loyalty-tier-rewards
  { tier: (string-ascii 15), month: uint }
  {
    reward-tokens: uint,
    bonus-multiplier: uint,
    free-reservation-hours: uint,
    priority-access: bool
  }
)

(define-map monthly-loyalty-claims
  { user: principal, month: uint }
  {
    claimed: bool,
    claim-block: uint,
    reward-amount: uint
  }
)

(define-read-only (get-parking-space (space-id uint))
  (map-get? parking-spaces { space-id: space-id })
)

(define-read-only (get-reservation (reservation-id uint))
  (map-get? reservations { reservation-id: reservation-id })
)

(define-read-only (get-user-stats (user principal))
  (default-to
    { total-reservations: u0, total-spent: u0, tokens-earned: u0, spaces-owned: u0 }
    (map-get? user-stats { user: user })
  )
)

(define-read-only (get-space-earnings (space-id uint))
  (default-to
    { total-earned: u0, total-hours: u0 }
    (map-get? space-earnings { space-id: space-id })
  )
)

(define-read-only (get-review (review-id uint))
  (map-get? reviews { review-id: review-id })
)

(define-read-only (get-space-ratings (space-id uint))
  (default-to
    { total-rating-points: u0, total-reviews: u0, five-star: u0, four-star: u0, three-star: u0, two-star: u0, one-star: u0 }
    (map-get? space-ratings { space-id: space-id })
  )
)

(define-read-only (get-user-review-status (user principal) (space-id uint))
  (default-to
    { review-id: u0, has-reviewed: false }
    (map-get? user-reviews { user: user, space-id: space-id })
  )
)

(define-read-only (get-average-rating (space-id uint))
  (let ((ratings (get-space-ratings space-id)))
    (if (> (get total-reviews ratings) u0)
      (/ (get total-rating-points ratings) (get total-reviews ratings))
      u0
    )
  )
)

(define-read-only (get-total-spaces)
  (var-get total-spaces)
)

(define-read-only (get-token-balance (user principal))
  (ft-get-balance parkit-token user)
)

(define-read-only (calculate-cost (hourly-rate uint) (duration-hours uint))
  (* hourly-rate duration-hours)
)

(define-read-only (calculate-tokens-earned (cost uint))
  (/ cost (var-get token-price))
)

(define-read-only (is-space-available (space-id uint) (start-time uint) (end-time uint))
  (let ((space (unwrap! (get-parking-space space-id) false)))
    (and
      (get is-available space)
    ;;   (not ((true) space-id start-time end-time))
    )
  )
)


(define-private (check-reservation-conflict (params (list 4 uint)) (has-conflict bool))
  (let ((space-id (unwrap-panic (element-at params u0)))
        (start-time (unwrap-panic (element-at params u1)))
        (end-time (unwrap-panic (element-at params u2))))
    has-conflict
  )
)

(define-public (add-parking-space (location (string-ascii 100)) (hourly-rate uint))
  (let ((space-id (+ (var-get total-spaces) u1)))
    (asserts! (> hourly-rate u0) err-invalid-duration)
    (map-set parking-spaces
      { space-id: space-id }
      {
        owner: tx-sender,
        location: location,
        hourly-rate: hourly-rate,
        is-available: true,
        created-at: stacks-block-height,
        average-rating: u0,
        total-reviews: u0
      }
    )
    (var-set total-spaces space-id)
    (let ((current-stats (get-user-stats tx-sender)))
      (map-set user-stats
        { user: tx-sender }
        (merge current-stats { spaces-owned: (+ (get spaces-owned current-stats) u1) })
      )
    )
    (ok space-id)
  )
)

(define-public (update-space-availability (space-id uint) (available bool))
  (let ((space (unwrap! (get-parking-space space-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner space)) err-unauthorized)
    (map-set parking-spaces
      { space-id: space-id }
      (merge space { is-available: available })
    )
    (ok true)
  )
)

(define-public (reserve-space (space-id uint) (duration-hours uint))
  (let (
    (space (unwrap! (get-parking-space space-id) err-not-found))
    (reservation-id (+ (var-get reservation-counter) u1))
    (start-time stacks-block-height)
    (end-time (+ stacks-block-height (* duration-hours u144)))
    (total-cost (calculate-cost (get hourly-rate space) duration-hours))
    (platform-cost (+ total-cost (var-get platform-fee)))
    (tokens-earned (calculate-tokens-earned total-cost))
  )
    (asserts! (> duration-hours u0) err-invalid-duration)
    (asserts! (get is-available space) err-space-unavailable)
    (asserts! (is-space-available space-id start-time end-time) err-space-unavailable)
    
    (try! (stx-transfer? platform-cost tx-sender (get owner space)))
    
    (map-set reservations
      { reservation-id: reservation-id }
      {
        space-id: space-id,
        renter: tx-sender,
        start-time: start-time,
        end-time: end-time,
        total-cost: total-cost,
        is-active: true,
        tokens-earned: tokens-earned
      }
    )
    
    (var-set reservation-counter reservation-id)
    
    (try! (ft-mint? parkit-token tokens-earned tx-sender))
    
    (let ((current-stats (get-user-stats tx-sender)))
      (map-set user-stats
        { user: tx-sender }
        (merge current-stats {
          total-reservations: (+ (get total-reservations current-stats) u1),
          total-spent: (+ (get total-spent current-stats) platform-cost),
          tokens-earned: (+ (get tokens-earned current-stats) tokens-earned)
        })
      )
    )
    
    (let ((current-earnings (get-space-earnings space-id)))
      (map-set space-earnings
        { space-id: space-id }
        {
          total-earned: (+ (get total-earned current-earnings) total-cost),
          total-hours: (+ (get total-hours current-earnings) duration-hours)
        }
      )
    )
    
    (ok reservation-id)
  )
)

(define-public (end-reservation (reservation-id uint))
  (let ((reservation (unwrap! (get-reservation reservation-id) err-not-found)))
    (asserts! (or 
      (is-eq tx-sender (get renter reservation))
      (> stacks-block-height (get end-time reservation))
    ) err-unauthorized)
    
    (map-set reservations
      { reservation-id: reservation-id }
      (merge reservation { is-active: false })
    )
    (ok true)
  )
)

(define-public (extend-reservation (reservation-id uint) (additional-hours uint))
  (let (
    (reservation (unwrap! (get-reservation reservation-id) err-not-found))
    (space (unwrap! (get-parking-space (get space-id reservation)) err-not-found))
    (additional-cost (calculate-cost (get hourly-rate space) additional-hours))
    (platform-cost (+ additional-cost (var-get platform-fee)))
    (additional-tokens (calculate-tokens-earned additional-cost))
    (new-end-time (+ (get end-time reservation) (* additional-hours u144)))
  )
    (asserts! (is-eq tx-sender (get renter reservation)) err-unauthorized)
    (asserts! (get is-active reservation) err-reservation-expired)
    (asserts! (> additional-hours u0) err-invalid-duration)
    
    (try! (stx-transfer? platform-cost tx-sender (get owner space)))
    
    (map-set reservations
      { reservation-id: reservation-id }
      (merge reservation {
        end-time: new-end-time,
        total-cost: (+ (get total-cost reservation) additional-cost),
        tokens-earned: (+ (get tokens-earned reservation) additional-tokens)
      })
    )
    
    (try! (ft-mint? parkit-token additional-tokens tx-sender))
    
    (let ((current-stats (get-user-stats tx-sender)))
      (map-set user-stats
        { user: tx-sender }
        (merge current-stats {
          total-spent: (+ (get total-spent current-stats) platform-cost),
          tokens-earned: (+ (get tokens-earned current-stats) additional-tokens)
        })
      )
    )
    
    (ok true)
  )
)

(define-public (update-token-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-price u0) err-invalid-duration)
    (var-set token-price new-price)
    (ok true)
  )
)

(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set platform-fee new-fee)
    (ok true)
  )
)

(define-public (withdraw-platform-fees)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let ((balance (stx-get-balance (as-contract tx-sender))))
      (and (> balance u0)
        (try! (as-contract (stx-transfer? balance tx-sender contract-owner)))
      )
      (ok balance)
    )
  )
)

(define-public (burn-tokens (amount uint))
  (ft-burn? parkit-token amount tx-sender)
)

(define-private (update-star-count (rating uint) (current-ratings (tuple (total-rating-points uint) (total-reviews uint) (five-star uint) (four-star uint) (three-star uint) (two-star uint) (one-star uint))))
  (if (is-eq rating u5)
    (merge current-ratings { five-star: (+ (get five-star current-ratings) u1) })
    (if (is-eq rating u4)
      (merge current-ratings { four-star: (+ (get four-star current-ratings) u1) })
      (if (is-eq rating u3)
        (merge current-ratings { three-star: (+ (get three-star current-ratings) u1) })
        (if (is-eq rating u2)
          (merge current-ratings { two-star: (+ (get two-star current-ratings) u1) })
          (merge current-ratings { one-star: (+ (get one-star current-ratings) u1) })
        )
      )
    )
  )
)

(define-private (can-review-space (user principal) (space-id uint) (reservation-id uint))
  (let ((reservation (unwrap! (get-reservation reservation-id) false)))
    (and
      (is-eq (get renter reservation) user)
      (is-eq (get space-id reservation) space-id)
      (not (get is-active reservation))
      (not (get has-reviewed (get-user-review-status user space-id)))
    )
  )
)

(define-public (submit-review (space-id uint) (reservation-id uint) (rating uint) (comment (string-ascii 500)))
  (let (
    (review-id (+ (var-get review-counter) u1))
    (space (unwrap! (get-parking-space space-id) err-not-found))
    (reservation (unwrap! (get-reservation reservation-id) err-not-found))
    (current-ratings (get-space-ratings space-id))
    (review-status (get-user-review-status tx-sender space-id))
  )
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
    (asserts! (not (get has-reviewed review-status)) err-already-reviewed)
    (asserts! (can-review-space tx-sender space-id reservation-id) err-review-not-allowed)
    
    (map-set reviews
      { review-id: review-id }
      {
        space-id: space-id,
        reviewer: tx-sender,
        reservation-id: reservation-id,
        rating: rating,
        comment: comment,
        created-at: stacks-block-height,
        is-verified: true
      }
    )
    
    (map-set user-reviews
      { user: tx-sender, space-id: space-id }
      { review-id: review-id, has-reviewed: true }
    )
    
    (let ((updated-ratings (update-star-count rating current-ratings)))
      (map-set space-ratings
        { space-id: space-id }
        (merge updated-ratings {
          total-rating-points: (+ (get total-rating-points updated-ratings) rating),
          total-reviews: (+ (get total-reviews updated-ratings) u1)
        })
      )
    )
    
    (let ((new-average (get-average-rating space-id)))
      (map-set parking-spaces
        { space-id: space-id }
        (merge space {
          average-rating: new-average,
          total-reviews: (+ (get total-reviews space) u1)
        })
      )
    )
    
    (var-set review-counter review-id)
    (ok review-id)
  )
)

(define-public (get-space-reviews (space-id uint) (offset uint) (limit uint))
  (let ((max-reviews (if (> limit u20) u20 limit)))
    (ok {
      space-id: space-id,
      total-reviews: (get total-reviews (get-space-ratings space-id)),
      average-rating: (get-average-rating space-id),
      rating-breakdown: (get-space-ratings space-id)
    })
  )
)

(define-public (flag-review (review-id uint) (reason (string-ascii 200)))
  (let ((review (unwrap! (get-review review-id) err-not-found)))
    (asserts! (not (is-eq tx-sender (get reviewer review))) err-unauthorized)
    (ok true)
  )
)

(define-public (get-top-rated-spaces (limit uint))
  (ok {
    message: "top-rated-spaces-query",
    limit: (if (> limit u50) u50 limit)
  })
)

(define-public (moderate-review (review-id uint) (action (string-ascii 20)))
  (let ((review (unwrap! (get-review review-id) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (or (is-eq action "approve") (is-eq action "reject")) err-invalid-rating)
    
    (map-set reviews
      { review-id: review-id }
      (merge review { is-verified: (is-eq action "approve") })
    )
    (ok true)
  )
)

(define-read-only (get-review-summary (space-id uint))
  (let ((ratings (get-space-ratings space-id)))
    {
      space-id: space-id,
      average-rating: (get-average-rating space-id),
      total-reviews: (get total-reviews ratings),
      rating-distribution: {
        five-star: (get five-star ratings),
        four-star: (get four-star ratings),
        three-star: (get three-star ratings),
        two-star: (get two-star ratings),
        one-star: (get one-star ratings)
      }
    }
  )
)

(define-read-only (get-subscription (subscription-id uint))
  (map-get? subscriptions { subscription-id: subscription-id })
)

(define-read-only (get-user-subscription (user principal) (space-id uint))
  (default-to
    { subscription-id: u0, is-subscribed: false }
    (map-get? user-subscriptions { user: user, space-id: space-id })
  )
)

(define-read-only (get-subscription-plan (plan-type (string-ascii 20)))
  (map-get? subscription-plans { plan-type: plan-type })
)

(define-read-only (get-subscription-usage (subscription-id uint) (usage-date uint))
  (default-to
    { hours-used: u0, reservations-count: u0 }
    (map-get? subscription-usage { subscription-id: subscription-id, usage-date: usage-date })
  )
)

(define-read-only (is-subscription-active (subscription-id uint))
  (let ((subscription (unwrap! (get-subscription subscription-id) false)))
    (and
      (get is-active subscription)
      (< stacks-block-height (get end-time subscription))
      (> (get hours-remaining subscription) u0)
    )
  )
)

(define-read-only (calculate-subscription-cost (space-id uint) (plan-type (string-ascii 20)))
  (let (
    (space (unwrap! (get-parking-space space-id) u0))
    (plan (unwrap! (get-subscription-plan plan-type) u0))
    (base-cost (* (get hourly-rate space) (get max-daily-hours plan) u30))
    (discount (/ (* base-cost (get discount-percentage plan)) u100))
  )
    (- base-cost discount)
  )
)

(define-read-only (get-daily-usage-limit (subscription-id uint) (current-date uint))
  (let (
    (subscription (unwrap! (get-subscription subscription-id) u0))
    (usage (get-subscription-usage subscription-id current-date))
  )
    (- (get max-daily-hours subscription) (get hours-used usage))
  )
)

(define-public (initialize-subscription-plans)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set subscription-plans { plan-type: "monthly" } 
      { duration-blocks: u4320, discount-percentage: u10, max-daily-hours: u8, transferable: false, priority-booking: true })
    (map-set subscription-plans { plan-type: "quarterly" } 
      { duration-blocks: u12960, discount-percentage: u20, max-daily-hours: u10, transferable: true, priority-booking: true })
    (map-set subscription-plans { plan-type: "yearly" } 
      { duration-blocks: u51840, discount-percentage: u35, max-daily-hours: u12, transferable: true, priority-booking: true })
    (ok true)
  )
)

(define-public (purchase-subscription (space-id uint) (plan-type (string-ascii 20)))
  (let (
    (space (unwrap! (get-parking-space space-id) err-not-found))
    (plan (unwrap! (get-subscription-plan plan-type) err-invalid-subscription-type))
    (subscription-id (+ (var-get subscription-counter) u1))
    (current-subscription (get-user-subscription tx-sender space-id))
    (cost (calculate-subscription-cost space-id plan-type))
    (start-time stacks-block-height)
    (end-time (+ start-time (get duration-blocks plan)))
    (monthly-hours (* (get max-daily-hours plan) u30))
  )
    (asserts! (not (get is-subscribed current-subscription)) err-subscription-already-exists)
    (asserts! (> cost u0) err-invalid-subscription-type)
    
    (try! (stx-transfer? cost tx-sender (get owner space)))
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      {
        subscriber: tx-sender,
        space-id: space-id,
        subscription-type: plan-type,
        start-time: start-time,
        end-time: end-time,
        monthly-cost: cost,
        total-paid: cost,
        hours-remaining: monthly-hours,
        max-daily-hours: (get max-daily-hours plan),
        is-active: true,
        auto-renew: false,
        transferable: (get transferable plan)
      }
    )
    
    (map-set user-subscriptions
      { user: tx-sender, space-id: space-id }
      { subscription-id: subscription-id, is-subscribed: true }
    )
    
    (var-set subscription-counter subscription-id)
    (ok subscription-id)
  )
)

(define-public (reserve-with-subscription (space-id uint) (duration-hours uint))
  (let (
    (user-sub (get-user-subscription tx-sender space-id))
    (subscription-id (get subscription-id user-sub))
    (subscription (unwrap! (get-subscription subscription-id) err-subscription-not-found))
    (current-date (/ stacks-block-height u144))
    (usage (get-subscription-usage subscription-id current-date))
    (daily-limit (get-daily-usage-limit subscription-id current-date))
    (reservation-id (+ (var-get reservation-counter) u1))
    (start-time stacks-block-height)
    (end-time (+ stacks-block-height (* duration-hours u144)))
  )
    (asserts! (get is-subscribed user-sub) err-subscription-not-found)
    (asserts! (is-subscription-active subscription-id) err-subscription-expired)
    (asserts! (>= daily-limit duration-hours) err-insufficient-subscription-time)
    (asserts! (>= (get hours-remaining subscription) duration-hours) err-insufficient-subscription-time)
    
    (map-set reservations
      { reservation-id: reservation-id }
      {
        space-id: space-id,
        renter: tx-sender,
        start-time: start-time,
        end-time: end-time,
        total-cost: u0,
        is-active: true,
        tokens-earned: u0
      }
    )
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription { hours-remaining: (- (get hours-remaining subscription) duration-hours) })
    )
    
    (map-set subscription-usage
      { subscription-id: subscription-id, usage-date: current-date }
      {
        hours-used: (+ (get hours-used usage) duration-hours),
        reservations-count: (+ (get reservations-count usage) u1)
      }
    )
    
    (var-set reservation-counter reservation-id)
    (ok reservation-id)
  )
)

(define-public (renew-subscription (subscription-id uint))
  (let (
    (subscription (unwrap! (get-subscription subscription-id) err-subscription-not-found))
    (space-id (get space-id subscription))
    (plan-type (get subscription-type subscription))
    (plan (unwrap! (get-subscription-plan plan-type) err-invalid-subscription-type))
    (cost (calculate-subscription-cost space-id plan-type))
    (space (unwrap! (get-parking-space space-id) err-not-found))
    (new-end-time (+ (get end-time subscription) (get duration-blocks plan)))
    (additional-hours (* (get max-daily-hours plan) u30))
  )
    (asserts! (is-eq tx-sender (get subscriber subscription)) err-unauthorized)
    (asserts! (< stacks-block-height (get end-time subscription)) err-subscription-expired)
    
    (try! (stx-transfer? cost tx-sender (get owner space)))
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription {
        end-time: new-end-time,
        total-paid: (+ (get total-paid subscription) cost),
        hours-remaining: (+ (get hours-remaining subscription) additional-hours)
      })
    )
    
    (ok true)
  )
)

(define-public (transfer-subscription (subscription-id uint) (new-owner principal))
  (let ((subscription (unwrap! (get-subscription subscription-id) err-subscription-not-found)))
    (asserts! (is-eq tx-sender (get subscriber subscription)) err-unauthorized)
    (asserts! (get transferable subscription) err-unauthorized)
    (asserts! (is-subscription-active subscription-id) err-subscription-expired)
    
    (map-set user-subscriptions
      { user: tx-sender, space-id: (get space-id subscription) }
      { subscription-id: u0, is-subscribed: false }
    )
    
    (map-set user-subscriptions
      { user: new-owner, space-id: (get space-id subscription) }
      { subscription-id: subscription-id, is-subscribed: true }
    )
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription { subscriber: new-owner })
    )
    
    (ok true)
  )
)

(define-public (cancel-subscription (subscription-id uint))
  (let (
    (subscription (unwrap! (get-subscription subscription-id) err-subscription-not-found))
    (space (unwrap! (get-parking-space (get space-id subscription)) err-not-found))
    (remaining-blocks (- (get end-time subscription) stacks-block-height))
    (total-blocks (- (get end-time subscription) (get start-time subscription)))
    (refund-amount (/ (* (get total-paid subscription) remaining-blocks) total-blocks))
  )
    (asserts! (is-eq tx-sender (get subscriber subscription)) err-unauthorized)
    (asserts! (is-subscription-active subscription-id) err-subscription-expired)
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription { is-active: false })
    )
    
    (map-set user-subscriptions
      { user: tx-sender, space-id: (get space-id subscription) }
      { subscription-id: subscription-id, is-subscribed: false }
    )
    
    (and (> refund-amount u0)
      (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
    )
    
    (ok refund-amount)
  )
)

(define-read-only (get-contract-info)
  {
    total-spaces: (var-get total-spaces),
    total-reservations: (var-get reservation-counter),
    total-reviews: (var-get review-counter),
    total-subscriptions: (var-get subscription-counter),
    token-price: (var-get token-price),
    platform-fee: (var-get platform-fee),
    contract-owner: contract-owner,
    total-loyalty-members: (var-get loyalty-counter)
  }
)

;; Loyalty Program Functions

(define-read-only (get-loyalty-member (user principal))
  (map-get? loyalty-members { user: user })
)

(define-read-only (get-loyalty-tier-reward (tier (string-ascii 15)) (month uint))
  (map-get? loyalty-tier-rewards { tier: tier, month: month })
)

(define-read-only (get-monthly-claim-status (user principal) (month uint))
  (default-to
    { claimed: false, claim-block: u0, reward-amount: u0 }
    (map-get? monthly-loyalty-claims { user: user, month: month })
  )
)

(define-read-only (calculate-loyalty-points (total-spent uint) (reservation-count uint))
  (+ (/ total-spent u100000) (* reservation-count u10))
)

(define-read-only (determine-loyalty-tier (total-points uint))
  (if (>= total-points u1000)
    "platinum"
    (if (>= total-points u500)
      "gold"
      (if (>= total-points u200)
        "silver"
        "bronze"
      )
    )
  )
)

(define-read-only (get-tier-multiplier (tier (string-ascii 15)))
  (if (is-eq tier "platinum")
    u150
    (if (is-eq tier "gold")
      u130
      (if (is-eq tier "silver")
        u120
        u110
      )
    )
  )
)

(define-read-only (calculate-bonus-tokens (base-tokens uint) (tier (string-ascii 15)))
  (let ((multiplier (get-tier-multiplier tier)))
    (/ (* base-tokens multiplier) u100)
  )
)

(define-private (should-reset-monthly-points (last-reset uint))
  (let ((current-month (/ stacks-block-height u4320))
        (reset-month (/ last-reset u4320)))
    (> current-month reset-month)
  )
)

(define-private (update-loyalty-activity (user principal) (points-earned uint) (bonus-tokens uint))
  (let ((member (unwrap! (get-loyalty-member user) err-loyalty-not-found))
        (current-month (/ stacks-block-height u4320))
        (should-reset (should-reset-monthly-points (get monthly-reset-block member)))
        (new-monthly-points (if should-reset points-earned (+ (get points-earned-this-month member) points-earned)))
        (new-total-points (+ (get total-points member) points-earned))
        (new-tier (determine-loyalty-tier new-total-points))
        (tier-changed (not (is-eq (get tier member) new-tier)))
        (consecutive-months (if should-reset u1 (get consecutive-months member))))
    (map-set loyalty-members
      { user: user }
      (merge member {
        total-points: new-total-points,
        points-earned-this-month: new-monthly-points,
        tier: new-tier,
        tier-upgrade-block: (if tier-changed stacks-block-height (get tier-upgrade-block member)),
        consecutive-months: consecutive-months,
        bonus-multiplier: (get-tier-multiplier new-tier),
        total-bonus-tokens: (+ (get total-bonus-tokens member) bonus-tokens),
        last-activity-block: stacks-block-height,
        monthly-reset-block: (if should-reset stacks-block-height (get monthly-reset-block member))
      })
    )
    (ok true)
  )
)

(define-public (join-loyalty-program)
  (let ((loyalty-id (+ (var-get loyalty-counter) u1))
        (current-user-stats (get-user-stats tx-sender))
        (initial-points (calculate-loyalty-points (get total-spent current-user-stats) (get total-reservations current-user-stats)))
        (initial-tier (determine-loyalty-tier initial-points)))
    (asserts! (is-none (get-loyalty-member tx-sender)) err-loyalty-already-exists)
    (map-set loyalty-members
      { user: tx-sender }
      {
        loyalty-id: loyalty-id,
        tier: initial-tier,
        total-points: initial-points,
        points-earned-this-month: u0,
        tier-upgrade-block: stacks-block-height,
        consecutive-months: u1,
        bonus-multiplier: (get-tier-multiplier initial-tier),
        total-bonus-tokens: u0,
        last-activity-block: stacks-block-height,
        monthly-reset-block: stacks-block-height
      }
    )
    (var-set loyalty-counter loyalty-id)
    (ok loyalty-id)
  )
)

(define-public (process-loyalty-reservation (reservation-cost uint) (tokens-earned uint))
  (let ((member (unwrap! (get-loyalty-member tx-sender) err-loyalty-not-found))
        (points-earned (calculate-loyalty-points reservation-cost u1))
        (bonus-tokens (- (calculate-bonus-tokens tokens-earned (get tier member)) tokens-earned)))
    (try! (update-loyalty-activity tx-sender points-earned bonus-tokens))
    (and (> bonus-tokens u0)
         (try! (ft-mint? parkit-token bonus-tokens tx-sender)))
    (ok {
      points-earned: points-earned,
      bonus-tokens: bonus-tokens,
      new-tier: (get tier (unwrap-panic (get-loyalty-member tx-sender)))
    })
  )
)

(define-public (claim-monthly-loyalty-reward)
  (let ((member (unwrap! (get-loyalty-member tx-sender) err-loyalty-not-found))
        (current-month (/ stacks-block-height u4320))
        (claim-status (get-monthly-claim-status tx-sender current-month))
        (tier (get tier member))
        (reward-tokens (if (is-eq tier "platinum") u100
                         (if (is-eq tier "gold") u75
                           (if (is-eq tier "silver") u50 u25))))
        (consecutive-bonus (if (> (get consecutive-months member) u3) u25 u0))
        (total-reward (+ reward-tokens consecutive-bonus)))
    (asserts! (not (get claimed claim-status)) err-reward-already-claimed)
    (asserts! (> (get points-earned-this-month member) u0) err-insufficient-loyalty-points)
    (try! (ft-mint? parkit-token total-reward tx-sender))
    (map-set monthly-loyalty-claims
      { user: tx-sender, month: current-month }
      {
        claimed: true,
        claim-block: stacks-block-height,
        reward-amount: total-reward
      }
    )
    (map-set loyalty-members
      { user: tx-sender }
      (merge member {
        consecutive-months: (+ (get consecutive-months member) u1),
        total-bonus-tokens: (+ (get total-bonus-tokens member) total-reward)
      })
    )
    (ok total-reward)
  )
)

(define-public (initialize-loyalty-tier-rewards)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    ;; Bronze tier rewards
    (map-set loyalty-tier-rewards { tier: "bronze", month: u1 }
      { reward-tokens: u25, bonus-multiplier: u110, free-reservation-hours: u0, priority-access: false })
    ;; Silver tier rewards
    (map-set loyalty-tier-rewards { tier: "silver", month: u1 }
      { reward-tokens: u50, bonus-multiplier: u120, free-reservation-hours: u1, priority-access: false })
    ;; Gold tier rewards
    (map-set loyalty-tier-rewards { tier: "gold", month: u1 }
      { reward-tokens: u75, bonus-multiplier: u130, free-reservation-hours: u2, priority-access: true })
    ;; Platinum tier rewards
    (map-set loyalty-tier-rewards { tier: "platinum", month: u1 }
      { reward-tokens: u100, bonus-multiplier: u150, free-reservation-hours: u3, priority-access: true })
    (ok true)
  )
)

(define-read-only (get-user-loyalty-summary (user principal))
  (let ((member (unwrap! (get-loyalty-member user) (err err-loyalty-not-found)))
        (current-month (/ stacks-block-height u4320))
        (claim-status (get-monthly-claim-status user current-month)))
    (ok {
      loyalty-id: (get loyalty-id member),
      tier: (get tier member),
      total-points: (get total-points member),
      points-this-month: (get points-earned-this-month member),
      bonus-multiplier: (get bonus-multiplier member),
      consecutive-months: (get consecutive-months member),
      total-bonus-tokens: (get total-bonus-tokens member),
      can-claim-monthly-reward: (and (> (get points-earned-this-month member) u0) (not (get claimed claim-status))),
      next-tier-points: (if (is-eq (get tier member) "bronze") u200
                          (if (is-eq (get tier member) "silver") u500
                            (if (is-eq (get tier member) "gold") u1000 u0))),
      points-to-next-tier: (if (is-eq (get tier member) "bronze") (- u200 (get total-points member))
                             (if (is-eq (get tier member) "silver") (- u500 (get total-points member))
                               (if (is-eq (get tier member) "gold") (- u1000 (get total-points member)) u0)))
    })
  )
)

(define-read-only (get-loyalty-program-stats)
  (ok {
    total-members: (var-get loyalty-counter),
    bronze-tier-multiplier: u110,
    silver-tier-multiplier: u120,
    gold-tier-multiplier: u130,
    platinum-tier-multiplier: u150,
    points-per-reservation: u10,
    points-per-stx-spent: u1
  })
)



