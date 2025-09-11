;; Content Moderation System Contract
;; Enables community reporting and moderation of advertisements

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-unauthorized (err u302))
(define-constant err-already-reported (err u303))
(define-constant err-invalid-action (err u304))
(define-constant err-report-resolved (err u305))

;; Data variables
(define-data-var total-reports uint u0)
(define-data-var total-moderators uint u0)
(define-data-var community-vote-threshold uint u3)

;; Report categories for content violations
(define-map ReportCategories
  uint
  {
    category-name: (string-ascii 30),
    severity-level: uint,
    auto-moderate: bool
  }
)

;; Advertisement reports tracking
(define-map AdReports
  uint ;; report-id
  {
    ad-id: uint,
    reporter: principal,
    category-id: uint,
    description: (string-ascii 300),
    reported-at: uint,
    status: (string-ascii 20),
    moderator: (optional principal),
    action-taken: (optional (string-ascii 50)),
    resolved-at: (optional uint)
  }
)

;; Community voting on reports
(define-map ReportVotes
  {report-id: uint, voter: principal}
  {
    vote: bool, ;; true for uphold, false for dismiss
    voted-at: uint
  }
)

;; Moderator reputation and performance
(define-map ModeratorStats
  principal
  {
    total-reviews: uint,
    correct-decisions: uint,
    accuracy-score: uint,
    is-active: bool,
    appointed-at: uint
  }
)

;; Track user report history
(define-map UserReportStats
  principal
  {
    reports-made: uint,
    valid-reports: uint,
    credibility-score: uint
  }
)

;; Initialize default report categories
(define-public (init-report-categories)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set ReportCategories u1 {category-name: "spam", severity-level: u2, auto-moderate: false})
    (map-set ReportCategories u2 {category-name: "inappropriate-content", severity-level: u4, auto-moderate: true})
    (map-set ReportCategories u3 {category-name: "misleading", severity-level: u3, auto-moderate: false})
    (map-set ReportCategories u4 {category-name: "scam", severity-level: u5, auto-moderate: true})
    (map-set ReportCategories u5 {category-name: "copyright", severity-level: u3, auto-moderate: false})
    (ok true)
  )
)

;; Report an advertisement for content violations
(define-public (report-advertisement 
  (ad-id uint) 
  (category-id uint) 
  (description (string-ascii 300)))
  (let (
    (report-id (+ (var-get total-reports) u1))
    (category (unwrap! (map-get? ReportCategories category-id) err-not-found))
    (user-stats (default-to {reports-made: u0, valid-reports: u0, credibility-score: u100} 
                            (map-get? UserReportStats tx-sender)))
  )
    (asserts! (is-none (map-get? AdReports report-id)) err-already-reported)
    
    (map-set AdReports report-id
      {
        ad-id: ad-id,
        reporter: tx-sender,
        category-id: category-id,
        description: description,
        reported-at: stacks-block-height,
        status: "pending",
        moderator: none,
        action-taken: none,
        resolved-at: none
      }
    )
    
    (map-set UserReportStats tx-sender
      {
        reports-made: (+ (get reports-made user-stats) u1),
        valid-reports: (get valid-reports user-stats),
        credibility-score: (get credibility-score user-stats)
      }
    )
    
    (var-set total-reports report-id)
    (ok report-id)
  )
)

;; Community voting on reports
(define-public (vote-on-report (report-id uint) (uphold bool))
  (let (
    (report (unwrap! (map-get? AdReports report-id) err-not-found))
    (vote-key {report-id: report-id, voter: tx-sender})
  )
    (asserts! (is-eq (get status report) "pending") err-report-resolved)
    (asserts! (is-none (map-get? ReportVotes vote-key)) err-already-reported)
    
    (map-set ReportVotes vote-key
      {
        vote: uphold,
        voted-at: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Appoint community moderators
(define-public (appoint-moderator (moderator principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set ModeratorStats moderator
      {
        total-reviews: u0,
        correct-decisions: u0,
        accuracy-score: u100,
        is-active: true,
        appointed-at: stacks-block-height
      }
    )
    (var-set total-moderators (+ (var-get total-moderators) u1))
    (ok true)
  )
)

;; Moderate advertisement reports
(define-public (moderate-report 
  (report-id uint) 
  (action (string-ascii 50)))
  (let (
    (report (unwrap! (map-get? AdReports report-id) err-not-found))
    (moderator-stats (unwrap! (map-get? ModeratorStats tx-sender) err-unauthorized))
  )
    (asserts! (get is-active moderator-stats) err-unauthorized)
    (asserts! (is-eq (get status report) "pending") err-report-resolved)
    
    (map-set AdReports report-id
      (merge report {
        status: "resolved",
        moderator: (some tx-sender),
        action-taken: (some action),
        resolved-at: (some stacks-block-height)
      })
    )
    
    (map-set ModeratorStats tx-sender
      (merge moderator-stats {
        total-reviews: (+ (get total-reviews moderator-stats) u1)
      })
    )
    
    (if (not (is-eq action "dismiss"))
      (let (
        (reporter-stats (default-to {reports-made: u0, valid-reports: u0, credibility-score: u100} 
                                  (map-get? UserReportStats (get reporter report))))
      )
        (map-set UserReportStats (get reporter report)
          (merge reporter-stats {
            valid-reports: (+ (get valid-reports reporter-stats) u1),
            credibility-score: (+ (get credibility-score reporter-stats) u10)
          })
        )
      )
      true
    )
    
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-report (report-id uint))
  (map-get? AdReports report-id)
)

(define-read-only (get-moderator-stats (moderator principal))
  (map-get? ModeratorStats moderator)
)

(define-read-only (get-user-report-stats (user principal))
  (map-get? UserReportStats user)
)

(define-read-only (get-report-category (category-id uint))
  (map-get? ReportCategories category-id)
)

(define-read-only (get-platform-moderation-stats)
  (ok {
    total-reports: (var-get total-reports),
    total-moderators: (var-get total-moderators),
    vote-threshold: (var-get community-vote-threshold)
  })
)

(define-read-only (has-pending-reports (ad-id uint))
  (let (
    (report-list (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10))
    (pending-reports (filter (lambda (report-id)
      (match (map-get? AdReports report-id)
        report (and 
          (is-eq (get ad-id report) ad-id)
          (is-eq (get status report) "pending")
        )
        false
      )
    ) report-list))
  )
    (ok (> (len pending-reports) u0))
  )
)
