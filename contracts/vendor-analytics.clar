;; Vendor Performance Analytics Dashboard
;; Provides comprehensive analytics and insights for vendor performance

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_VENDOR_NOT_FOUND (err u201))
(define-constant ERR_INVALID_PERIOD (err u202))
(define-constant ERR_NO_DATA (err u203))

;; Data variables
(define-data-var analytics-start-block uint u0)

;; Performance metrics by time period
(define-map vendor-daily-metrics
  { vendor-id: uint, day: uint } ;; day = blocks / 144 (~24 hours)
  {
    reviews-received: uint,
    total-rating-points: uint,
    product-views: uint,
    new-subscribers: uint,
    revenue-estimate: uint,
    engagement-score: uint,
    updated-at: uint
  }
)

;; Weekly aggregated metrics
(define-map vendor-weekly-summary
  { vendor-id: uint, week: uint } ;; week = blocks / 1008 (~7 days)
  {
    total-reviews: uint,
    avg-rating: uint,
    total-views: uint,
    subscriber-growth: uint,
    engagement-trend: int, ;; positive/negative trend
    performance-score: uint,
    peak-day: uint,
    summary-generated: uint
  }
)

;; Growth tracking over time
(define-map vendor-growth-metrics
  { vendor-id: uint }
  {
    baseline-reviews: uint,
    baseline-rating: uint,
    baseline-subscribers: uint,
    growth-rate-reviews: int, ;; percentage change
    growth-rate-rating: int,
    growth-rate-subscribers: int,
    last-calculated: uint,
    trend-direction: (string-ascii 10) ;; "up", "down", "stable"
  }
)

;; Comparative analytics
(define-map category-benchmarks
  { category: (string-ascii 30), period: uint }
  {
    avg-rating: uint,
    avg-reviews: uint,
    avg-engagement: uint,
    vendor-count: uint,
    updated-at: uint
  }
)

;; Performance insights cache
(define-map vendor-insights
  { vendor-id: uint }
  {
    performance-rank: uint, ;; 1-100 percentile
    top-strength: (string-ascii 20), ;; "rating", "engagement", "growth"
    improvement-area: (string-ascii 20),
    recommendation: (string-ascii 100),
    last-updated: uint
  }
)

;; Read-only functions
(define-read-only (get-vendor-daily-metrics (vendor-id uint) (day uint))
  (map-get? vendor-daily-metrics { vendor-id: vendor-id, day: day })
)

(define-read-only (get-vendor-weekly-summary (vendor-id uint) (week uint))
  (map-get? vendor-weekly-summary { vendor-id: vendor-id, week: week })
)

(define-read-only (get-vendor-growth-metrics (vendor-id uint))
  (map-get? vendor-growth-metrics { vendor-id: vendor-id })
)

(define-read-only (get-vendor-insights (vendor-id uint))
  (map-get? vendor-insights { vendor-id: vendor-id })
)

(define-read-only (get-category-benchmarks (category (string-ascii 30)) (period uint))
  (map-get? category-benchmarks { category: category, period: period })
)

(define-read-only (calculate-current-performance-score (vendor-id uint))
  (let
    (
      (current-day (/ stacks-block-height u144))
      (daily-metrics (map-get? vendor-daily-metrics { vendor-id: vendor-id, day: current-day }))
    )
    (match daily-metrics
      metrics (let
        (
          (engagement (get engagement-score metrics))
          (reviews (get reviews-received metrics))
          (views (get product-views metrics))
          (subscribers (get new-subscribers metrics))
          (base-score (+ (* engagement u40) (* reviews u30) (* views u20) (* subscribers u10)))
        )
        (some (/ base-score u100))
      )
      none
    )
  )
)

;; Public functions for updating analytics
(define-public (update-daily-metrics (vendor-id uint) (reviews uint) (rating-points uint) (views uint) (new-subs uint) (revenue uint))
  (let
    (
      (current-day (/ stacks-block-height u144))
      (engagement-score (calculate-engagement-score reviews views new-subs))
      (existing-metrics (map-get? vendor-daily-metrics { vendor-id: vendor-id, day: current-day }))
    )
    ;; Simple authorization check - in real implementation, would verify caller
    (asserts! (> vendor-id u0) ERR_VENDOR_NOT_FOUND)
    
    (match existing-metrics
      metrics (map-set vendor-daily-metrics
        { vendor-id: vendor-id, day: current-day }
        {
          reviews-received: (+ (get reviews-received metrics) reviews),
          total-rating-points: (+ (get total-rating-points metrics) rating-points),
          product-views: (+ (get product-views metrics) views),
          new-subscribers: (+ (get new-subscribers metrics) new-subs),
          revenue-estimate: (+ (get revenue-estimate metrics) revenue),
          engagement-score: engagement-score,
          updated-at: stacks-block-height
        }
      )
      (map-set vendor-daily-metrics
        { vendor-id: vendor-id, day: current-day }
        {
          reviews-received: reviews,
          total-rating-points: rating-points,
          product-views: views,
          new-subscribers: new-subs,
          revenue-estimate: revenue,
          engagement-score: engagement-score,
          updated-at: stacks-block-height
        }
      )
    )
    (ok true)
  )
)

(define-public (generate-weekly-summary (vendor-id uint))
  (let
    (
      (current-week (/ stacks-block-height u1008))
      (week-start-day (- (/ stacks-block-height u144) u7))
      (summary-data (aggregate-weekly-data vendor-id week-start-day))
    )
    (asserts! (> vendor-id u0) ERR_VENDOR_NOT_FOUND)
    
    (map-set vendor-weekly-summary
      { vendor-id: vendor-id, week: current-week }
      summary-data
    )
    (ok summary-data)
  )
)

(define-public (update-growth-metrics (vendor-id uint))
  (let
    (
      (current-growth (default-to
        {
          baseline-reviews: u0,
          baseline-rating: u0,
          baseline-subscribers: u0,
          growth-rate-reviews: 0,
          growth-rate-rating: 0,
          growth-rate-subscribers: 0,
          last-calculated: u0,
          trend-direction: "stable"
        }
        (map-get? vendor-growth-metrics { vendor-id: vendor-id })
      ))
      (current-day (/ stacks-block-height u144))
      (today-metrics (map-get? vendor-daily-metrics { vendor-id: vendor-id, day: current-day }))
      (week-ago-metrics (map-get? vendor-daily-metrics { vendor-id: vendor-id, day: (- current-day u7) }))
    )
    (asserts! (> vendor-id u0) ERR_VENDOR_NOT_FOUND)
    
    (match today-metrics
      today (match week-ago-metrics
        week-ago (let
          (
            (review-growth (calculate-growth-rate (get reviews-received week-ago) (get reviews-received today)))
            (view-growth (calculate-growth-rate (get product-views week-ago) (get product-views today)))
            (sub-growth (calculate-growth-rate (get new-subscribers week-ago) (get new-subscribers today)))
            (trend (determine-trend review-growth view-growth sub-growth))
          )
          (map-set vendor-growth-metrics
            { vendor-id: vendor-id }
            (merge current-growth {
              growth-rate-reviews: review-growth,
              growth-rate-subscribers: sub-growth,
              last-calculated: stacks-block-height,
              trend-direction: trend
            })
          )
          (ok trend)
        )
        (ok "insufficient-data")
      )
      (ok "no-current-data")
    )
  )
)

(define-public (generate-vendor-insights (vendor-id uint))
  (let
    (
      (growth-metrics (map-get? vendor-growth-metrics { vendor-id: vendor-id }))
      (performance-score (calculate-current-performance-score vendor-id))
    )
    (asserts! (> vendor-id u0) ERR_VENDOR_NOT_FOUND)
    
    (match growth-metrics
      metrics (let
        (
          (rank (default-to u50 performance-score)) ;; Simplified ranking
          (strength (determine-top-strength metrics))
          (weakness (determine-improvement-area metrics))
          (recommendation (generate-recommendation strength weakness))
        )
        (map-set vendor-insights
          { vendor-id: vendor-id }
          {
            performance-rank: rank,
            top-strength: strength,
            improvement-area: weakness,
            recommendation: recommendation,
            last-updated: stacks-block-height
          }
        )
        (ok true)
      )
      (ok false)
    )
  )
)

;; Private helper functions
(define-private (calculate-engagement-score (reviews uint) (views uint) (subscribers uint))
  (let
    (
      (review-score (* reviews u30))
      (view-score (* views u5))
      (sub-score (* subscribers u20))
      (total-score (+ review-score view-score sub-score))
    )
    (if (> total-score u100) u100 total-score)
  )
)

(define-private (aggregate-weekly-data (vendor-id uint) (start-day uint))
  (let
    (
      ;; Simplified - would aggregate across 7 days in full implementation
      (day-1 (default-to { reviews-received: u0, total-rating-points: u0, product-views: u0, new-subscribers: u0, revenue-estimate: u0, engagement-score: u0, updated-at: u0 } 
              (map-get? vendor-daily-metrics { vendor-id: vendor-id, day: start-day })))
      (day-7 (default-to { reviews-received: u0, total-rating-points: u0, product-views: u0, new-subscribers: u0, revenue-estimate: u0, engagement-score: u0, updated-at: u0 }
              (map-get? vendor-daily-metrics { vendor-id: vendor-id, day: (+ start-day u6) })))
    )
    {
      total-reviews: (+ (get reviews-received day-1) (get reviews-received day-7)),
      avg-rating: (if (> (+ (get reviews-received day-1) (get reviews-received day-7)) u0)
                    (/ (+ (get total-rating-points day-1) (get total-rating-points day-7))
                       (+ (get reviews-received day-1) (get reviews-received day-7)))
                    u0),
      total-views: (+ (get product-views day-1) (get product-views day-7)),
      subscriber-growth: (+ (get new-subscribers day-1) (get new-subscribers day-7)),
      engagement-trend: 0, ;; Simplified
      performance-score: (/ (+ (get engagement-score day-1) (get engagement-score day-7)) u2),
      peak-day: start-day,
      summary-generated: stacks-block-height
    }
  )
)

(define-private (calculate-growth-rate (old-value uint) (new-value uint))
  (if (is-eq old-value u0)
    (if (> new-value u0) 100 0)
    (let ((diff (if (> new-value old-value) (- new-value old-value) (- old-value new-value))))
      (if (> new-value old-value)
        (to-int (/ (* diff u100) old-value))
        (- (to-int (/ (* diff u100) old-value)))
      )
    )
  )
)

(define-private (determine-trend (review-growth int) (view-growth int) (sub-growth int))
  (let ((avg-growth (/ (+ review-growth view-growth sub-growth) 3)))
    (if (> avg-growth 10)
      "up"
      (if (< avg-growth -10)
        "down"
        "stable"
      )
    )
  )
)

(define-private (determine-top-strength (metrics {baseline-reviews: uint, baseline-rating: uint, baseline-subscribers: uint, growth-rate-reviews: int, growth-rate-rating: int, growth-rate-subscribers: int, last-calculated: uint, trend-direction: (string-ascii 10)}))
  (let
    (
      (review-growth (get growth-rate-reviews metrics))
      (sub-growth (get growth-rate-subscribers metrics))
    )
    (if (> review-growth sub-growth)
      "reviews"
      (if (> sub-growth 5)
        "growth"
        "engagement"
      )
    )
  )
)

(define-private (determine-improvement-area (metrics {baseline-reviews: uint, baseline-rating: uint, baseline-subscribers: uint, growth-rate-reviews: int, growth-rate-rating: int, growth-rate-subscribers: int, last-calculated: uint, trend-direction: (string-ascii 10)}))
  (let
    (
      (review-growth (get growth-rate-reviews metrics))
      (sub-growth (get growth-rate-subscribers metrics))
    )
    (if (< review-growth -5)
      "reviews"
      (if (< sub-growth -5)
        "subscribers"
        "engagement"
      )
    )
  )
)

(define-private (generate-recommendation (strength (string-ascii 20)) (weakness (string-ascii 20)))
  (if (is-eq strength "reviews")
    "Focus on subscriber growth and product visibility"
    (if (is-eq strength "growth")
      "Maintain growth momentum with quality service"
      "Encourage more customer reviews and feedback"
    )
  )
)
