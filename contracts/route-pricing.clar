;; Dynamic Route Pricing System
;; Manages route-specific pricing multipliers for transport passes

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_ROUTE_NOT_FOUND (err u201))
(define-constant ERR_INVALID_MULTIPLIER (err u202))
(define-constant ERR_ROUTE_EXISTS (err u203))

;; Data variables
(define-data-var next-route-id uint u1)
(define-data-var total-routes uint u0)

;; Route data structure
(define-map route-info
  (string-ascii 20) ;; route-id
  {
    route-name: (string-ascii 50),
    base-multiplier: uint,
    distance-km: uint,
    zone: (string-ascii 20),
    is-premium: bool,
    is-active: bool,
    created-at: uint
  }
)

;; Zone pricing multipliers
(define-map zone-multipliers
  (string-ascii 20) ;; zone-name
  uint ;; multiplier (100 = 1.0x, 150 = 1.5x, 200 = 2.0x)
)

;; Peak hour multipliers
(define-map peak-hour-pricing
  uint ;; hour (0-23)
  uint ;; multiplier
)

;; Route usage statistics for dynamic pricing
(define-map route-stats
  (string-ascii 20) ;; route-id
  {
    total-rides: uint,
    daily-rides: uint,
    last-updated: uint
  }
)

;; Read-only functions
(define-read-only (get-route-info (route-id (string-ascii 20)))
  (map-get? route-info route-id)
)

(define-read-only (get-zone-multiplier (zone (string-ascii 20)))
  (default-to u100 (map-get? zone-multipliers zone))
)

(define-read-only (get-peak-multiplier (hour uint))
  (default-to u100 (map-get? peak-hour-pricing hour))
)

(define-read-only (get-route-stats (route-id (string-ascii 20)))
  (map-get? route-stats route-id)
)

(define-read-only (calculate-route-price (route-id (string-ascii 20)) (base-price uint))
  (let
    (
      (route-data (unwrap! (map-get? route-info route-id) ERR_ROUTE_NOT_FOUND))
      (zone-mult (get-zone-multiplier (get zone route-data)))
      (base-mult (get base-multiplier route-data))
      (final-multiplier (/ (* base-mult zone-mult) u100))
    )
    (ok (/ (* base-price final-multiplier) u100))
  )
)

(define-read-only (is-route-active (route-id (string-ascii 20)))
  (match (map-get? route-info route-id)
    route-data (get is-active route-data)
    false
  )
)

(define-read-only (get-contract-stats)
  {
    total-routes: (var-get total-routes),
    next-route-id: (var-get next-route-id)
  }
)

;; Public functions - Owner only
(define-public (register-route 
    (route-id (string-ascii 20)) 
    (route-name (string-ascii 50))
    (base-multiplier uint)
    (distance-km uint)
    (zone (string-ascii 20))
    (is-premium bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? route-info route-id)) ERR_ROUTE_EXISTS)
    (asserts! (and (>= base-multiplier u50) (<= base-multiplier u300)) ERR_INVALID_MULTIPLIER)
    
    (map-set route-info route-id
      {
        route-name: route-name,
        base-multiplier: base-multiplier,
        distance-km: distance-km,
        zone: zone,
        is-premium: is-premium,
        is-active: true,
        created-at: stacks-block-height
      }
    )
    
    (map-set route-stats route-id
      {
        total-rides: u0,
        daily-rides: u0,
        last-updated: stacks-block-height
      }
    )
    
    (var-set next-route-id (+ (var-get next-route-id) u1))
    (var-set total-routes (+ (var-get total-routes) u1))
    
    (ok route-id)
  )
)

(define-public (update-route-multiplier (route-id (string-ascii 20)) (new-multiplier uint))
  (let
    (
      (route-data (unwrap! (map-get? route-info route-id) ERR_ROUTE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (and (>= new-multiplier u50) (<= new-multiplier u300)) ERR_INVALID_MULTIPLIER)
    
    (map-set route-info route-id
      (merge route-data {base-multiplier: new-multiplier})
    )
    
    (ok true)
  )
)

(define-public (set-zone-multiplier (zone (string-ascii 20)) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (and (>= multiplier u50) (<= multiplier u300)) ERR_INVALID_MULTIPLIER)
    
    (map-set zone-multipliers zone multiplier)
    (ok true)
  )
)

(define-public (set-peak-hour-multiplier (hour uint) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (< hour u24) ERR_INVALID_MULTIPLIER)
    (asserts! (and (>= multiplier u100) (<= multiplier u200)) ERR_INVALID_MULTIPLIER)
    
    (map-set peak-hour-pricing hour multiplier)
    (ok true)
  )
)

(define-public (toggle-route-status (route-id (string-ascii 20)))
  (let
    (
      (route-data (unwrap! (map-get? route-info route-id) ERR_ROUTE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set route-info route-id
      (merge route-data {is-active: (not (get is-active route-data))})
    )
    
    (ok true)
  )
)

;; Public function to update route usage (called by main transport contract)
(define-public (update-route-usage (route-id (string-ascii 20)))
  (let
    (
      (current-stats (default-to
        {total-rides: u0, daily-rides: u0, last-updated: u0}
        (map-get? route-stats route-id)
      ))
      (current-day (/ stacks-block-height u144))
      (last-day (/ (get last-updated current-stats) u144))
      (daily-count (if (is-eq current-day last-day) 
                     (+ (get daily-rides current-stats) u1) 
                     u1))
    )
    
    (map-set route-stats route-id
      {
        total-rides: (+ (get total-rides current-stats) u1),
        daily-rides: daily-count,
        last-updated: stacks-block-height
      }
    )
    
    (ok true)
  )
)
