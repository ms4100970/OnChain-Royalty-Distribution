;; Project Risk Assessment & Early Warning System
;; Monitors project health and provides early warnings for budget and schedule risks
;; Integrates with Trackworks contract for comprehensive project oversight

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_PROJECT_NOT_FOUND (err u201))
(define-constant ERR_RISK_ASSESSMENT_NOT_FOUND (err u202))
(define-constant ERR_INVALID_THRESHOLD (err u203))
(define-constant ERR_INVALID_RISK_LEVEL (err u204))

;; Risk level constants
(define-constant RISK_LOW u1)
(define-constant RISK_MEDIUM u2)
(define-constant RISK_HIGH u3)
(define-constant RISK_CRITICAL u4)

;; Risk factor weights (out of 100)
(define-constant BUDGET_WEIGHT u40)
(define-constant SCHEDULE_WEIGHT u30)
(define-constant MILESTONE_WEIGHT u20)
(define-constant PAYMENT_WEIGHT u10)

(define-data-var next-assessment-id uint u1)
(define-data-var next-alert-id uint u1)

;; Risk assessment data for each project
(define-map project-risk-assessments
  { project-id: uint }
  {
    overall-risk-score: uint,
    budget-risk-score: uint,
    schedule-risk-score: uint,
    milestone-risk-score: uint,
    payment-risk-score: uint,
    risk-level: uint,
    last-updated: uint,
    assessment-count: uint
  }
)

;; Risk thresholds configuration
(define-map risk-thresholds
  { threshold-type: (string-ascii 20) }
  {
    low-threshold: uint,
    medium-threshold: uint,
    high-threshold: uint,
    enabled: bool
  }
)

;; Risk alerts for high-risk conditions
(define-map risk-alerts
  { alert-id: uint }
  {
    project-id: uint,
    alert-type: (string-ascii 30),
    risk-level: uint,
    message: (string-ascii 200),
    created-at: uint,
    acknowledged: bool,
    acknowledged-by: (optional principal),
    resolved: bool
  }
)

;; Risk assessment history
(define-map risk-history
  { assessment-id: uint }
  {
    project-id: uint,
    risk-score: uint,
    risk-level: uint,
    assessment-date: uint,
    triggered-alerts: (list 5 uint)
  }
)

;; Initialize default risk thresholds
(map-set risk-thresholds { threshold-type: "overall" } { low-threshold: u25, medium-threshold: u50, high-threshold: u75, enabled: true })
(map-set risk-thresholds { threshold-type: "budget" } { low-threshold: u30, medium-threshold: u60, high-threshold: u80, enabled: true })
(map-set risk-thresholds { threshold-type: "schedule" } { low-threshold: u20, medium-threshold: u40, high-threshold: u70, enabled: true })

;; Calculate comprehensive project risk score
(define-public (calculate-project-risk-score (project-id uint))
  (let
    (
      (project-data (unwrap! (contract-call? .Trackworks get-project project-id) ERR_PROJECT_NOT_FOUND))
      (budget-status (unwrap! (contract-call? .Trackworks get-project-budget-status project-id) ERR_PROJECT_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      
      ;; Calculate individual risk scores
      (budget-risk (calculate-budget-risk budget-status))
      (schedule-risk (calculate-schedule-risk project-data current-time))
      (milestone-risk (calculate-milestone-risk project-id))
      (payment-risk (calculate-payment-risk project-id))
      
      ;; Calculate weighted overall risk score
      (overall-risk (+ 
        (/ (* budget-risk BUDGET_WEIGHT) u100)
        (/ (* schedule-risk SCHEDULE_WEIGHT) u100)
        (/ (* milestone-risk MILESTONE_WEIGHT) u100)
        (/ (* payment-risk PAYMENT_WEIGHT) u100)
      ))
      
      (risk-level (determine-risk-level overall-risk))
      (assessment-id (var-get next-assessment-id))
    )
    
    ;; Store risk assessment
    (map-set project-risk-assessments
      { project-id: project-id }
      {
        overall-risk-score: overall-risk,
        budget-risk-score: budget-risk,
        schedule-risk-score: schedule-risk,
        milestone-risk-score: milestone-risk,
        payment-risk-score: payment-risk,
        risk-level: risk-level,
        last-updated: current-time,
        assessment-count: (+ (default-to u0 (get assessment-count (map-get? project-risk-assessments { project-id: project-id }))) u1)
      }
    )
    
    ;; Record in history
    (map-set risk-history
      { assessment-id: assessment-id }
      {
        project-id: project-id,
        risk-score: overall-risk,
        risk-level: risk-level,
        assessment-date: current-time,
        triggered-alerts: (list)
      }
    )
    
    (var-set next-assessment-id (+ assessment-id u1))
    
    ;; Generate alerts if risk is high
    (if (>= risk-level RISK_HIGH)
      (begin
        (unwrap! (generate-risk-alert project-id "high-risk-detected" risk-level "Project risk level exceeds safe thresholds") (ok overall-risk))
        (ok overall-risk)
      )
      (ok overall-risk)
    )
  )
)

;; Generate risk alert
(define-public (generate-risk-alert 
  (project-id uint) 
  (alert-type (string-ascii 30)) 
  (risk-level uint) 
  (message (string-ascii 200))
)
  (let
    (
      (alert-id (var-get next-alert-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set risk-alerts
      { alert-id: alert-id }
      {
        project-id: project-id,
        alert-type: alert-type,
        risk-level: risk-level,
        message: message,
        created-at: current-time,
        acknowledged: false,
        acknowledged-by: none,
        resolved: false
      }
    )
    (var-set next-alert-id (+ alert-id u1))
    (ok alert-id)
  )
)

;; Acknowledge risk alert
(define-public (acknowledge-alert (alert-id uint))
  (let
    (
      (alert (unwrap! (map-get? risk-alerts { alert-id: alert-id }) ERR_RISK_ASSESSMENT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (get acknowledged alert)) ERR_INVALID_RISK_LEVEL)
    (map-set risk-alerts
      { alert-id: alert-id }
      (merge alert {
        acknowledged: true,
        acknowledged-by: (some tx-sender)
      })
    )
    (ok true)
  )
)

;; Set custom risk thresholds
(define-public (set-risk-thresholds 
  (threshold-type (string-ascii 20)) 
  (low uint) 
  (medium uint) 
  (high uint)
)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (and (< low medium) (< medium high) (<= high u100)) ERR_INVALID_THRESHOLD)
    (map-set risk-thresholds
      { threshold-type: threshold-type }
      {
        low-threshold: low,
        medium-threshold: medium,
        high-threshold: high,
        enabled: true
      }
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-project-risk-assessment (project-id uint))
  (map-get? project-risk-assessments { project-id: project-id })
)

(define-read-only (get-risk-alert (alert-id uint))
  (map-get? risk-alerts { alert-id: alert-id })
)

(define-read-only (get-risk-thresholds (threshold-type (string-ascii 20)))
  (map-get? risk-thresholds { threshold-type: threshold-type })
)

(define-read-only (get-project-active-alerts (project-id uint))
  (ok {
    project-id: project-id,
    total-alerts: (get-project-alert-count project-id),
    unacknowledged-alerts: (get-unacknowledged-alert-count project-id),
    critical-alerts: (get-critical-alert-count project-id)
  })
)

;; Private helper functions
(define-private (calculate-budget-risk (budget-status (tuple (completion-percentage uint) (paid-amount uint) (remaining-budget uint) (total-budget uint))))
  (let
    (
      (completion-pct (get completion-percentage budget-status))
      (budget-used-pct (/ (* (get paid-amount budget-status) u100) (get total-budget budget-status)))
    )
    ;; Risk increases if budget is consumed faster than progress
    (if (> budget-used-pct completion-pct)
      (min u100 (* (- budget-used-pct completion-pct) u2))
      u0
    )
  )
)

(define-private (calculate-schedule-risk (project-data (tuple (contractor principal) (created-at uint) (name (string-ascii 100)) (paid-amount uint) (status (string-ascii 20)) (total-budget uint) (updated-at uint))) (current-time uint))
  (let
    (
      (project-age (- current-time (get created-at project-data)))
      (days-since-creation (/ project-age u86400))
    )
    ;; Simple risk calculation based on project age and status
    (if (is-eq (get status project-data) "active")
      (if (> days-since-creation u90) ;; Project over 90 days
        u60
        (min u50 (/ days-since-creation u2))
      )
      u10
    )
  )
)

(define-private (calculate-milestone-risk (project-id uint))
  ;; Simplified milestone risk calculation
  u20
)

(define-private (calculate-payment-risk (project-id uint))
  ;; Simplified payment risk calculation  
  u15
)

(define-private (determine-risk-level (risk-score uint))
  (let
    (
      (thresholds (unwrap-panic (map-get? risk-thresholds { threshold-type: "overall" })))
    )
    (if (>= risk-score (get high-threshold thresholds))
      RISK_CRITICAL
      (if (>= risk-score (get medium-threshold thresholds))
        RISK_HIGH
        (if (>= risk-score (get low-threshold thresholds))
          RISK_MEDIUM
          RISK_LOW
        )
      )
    )
  )
)

;; Simplified analytics helpers
(define-private (get-project-alert-count (project-id uint))
  u0
)

(define-private (get-unacknowledged-alert-count (project-id uint))
  u0
)

(define-private (get-critical-alert-count (project-id uint))
  u0
)
