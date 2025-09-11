;; title: Scholarship Progress Tracker
;; version: 1.0.0
;; summary: Academic progress tracking system for scholarship recipients
;; description: Allows scholarship recipients to report academic progress and maintain accountability

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u300))
(define-constant ERR_INVALID_PROGRESS_REPORT (err u301))
(define-constant ERR_SCHOLARSHIP_NOT_FOUND (err u302))
(define-constant ERR_INVALID_GPA (err u303))
(define-constant ERR_MILESTONE_NOT_FOUND (err u304))
(define-constant ERR_MILESTONE_ALREADY_COMPLETED (err u305))
(define-constant ERR_INVALID_MILESTONE_TYPE (err u306))

;; milestone types
(define-constant MILESTONE_SEMESTER_COMPLETE u1)
(define-constant MILESTONE_GPA_TARGET u2)
(define-constant MILESTONE_COURSE_COMPLETE u3)
(define-constant MILESTONE_PROJECT_COMPLETE u4)

;; data vars
(define-data-var progress-report-counter uint u0)
(define-data-var milestone-counter uint u0)

;; data maps
(define-map scholarship-recipients
  { student: principal, proposal-id: uint }
  {
    awarded-amount: uint,
    awarded-at: uint,
    recipient-name: (string-ascii 100),
    expected-completion-date: uint,
    program-duration-months: uint,
    active: bool
  }
)

(define-map progress-reports
  uint
  {
    student: principal,
    proposal-id: uint,
    semester-term: (string-ascii 50),
    current-gpa: uint,
    courses-completed: uint,
    courses-enrolled: uint,
    progress-description: (string-ascii 500),
    challenges-faced: (string-ascii 300),
    goals-next-term: (string-ascii 300),
    submitted-at: uint,
    verified: bool,
    verifier: (optional principal)
  }
)

(define-map academic-milestones
  uint
  {
    student: principal,
    proposal-id: uint,
    milestone-type: uint,
    milestone-description: (string-ascii 200),
    target-value: uint,
    target-date: uint,
    completed: bool,
    completed-at: (optional uint),
    completion-proof: (optional (string-ascii 200))
  }
)

(define-map student-progress-summary
  { student: principal, proposal-id: uint }
  {
    total-reports: uint,
    average-gpa: uint,
    total-courses-completed: uint,
    milestones-completed: uint,
    milestones-total: uint,
    last-report-date: uint,
    progress-score: uint,
    at-risk-status: bool
  }
)

(define-map progress-verification-requests
  uint
  {
    report-id: uint,
    student: principal,
    requested-at: uint,
    evidence-description: (string-ascii 300),
    status: (string-ascii 20)
  }
)

;; public functions
(define-public (register-scholarship-recipient 
  (student principal) 
  (proposal-id uint) 
  (awarded-amount uint)
  (recipient-name (string-ascii 100))
  (program-duration-months uint))
  (let ((current-block stacks-block-height)
        (expected-completion (+ current-block (* program-duration-months u720)))) ;; ~30 days per month in blocks
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> awarded-amount u0) ERR_INVALID_PROGRESS_REPORT)
    (asserts! (> program-duration-months u0) ERR_INVALID_PROGRESS_REPORT)
    
    (map-set scholarship-recipients
      { student: student, proposal-id: proposal-id }
      {
        awarded-amount: awarded-amount,
        awarded-at: current-block,
        recipient-name: recipient-name,
        expected-completion-date: expected-completion,
        program-duration-months: program-duration-months,
        active: true
      })
    
    (map-set student-progress-summary
      { student: student, proposal-id: proposal-id }
      {
        total-reports: u0,
        average-gpa: u0,
        total-courses-completed: u0,
        milestones-completed: u0,
        milestones-total: u0,
        last-report-date: u0,
        progress-score: u100,
        at-risk-status: false
      })
    
    (ok true)))

(define-public (submit-progress-report
  (proposal-id uint)
  (semester-term (string-ascii 50))
  (current-gpa uint)
  (courses-completed uint)
  (courses-enrolled uint)
  (progress-description (string-ascii 500))
  (challenges-faced (string-ascii 300))
  (goals-next-term (string-ascii 300)))
  (let ((report-id (+ (var-get progress-report-counter) u1))
        (recipient-info (unwrap! (map-get? scholarship-recipients { student: tx-sender, proposal-id: proposal-id }) ERR_SCHOLARSHIP_NOT_FOUND))
        (current-summary (default-to 
          { total-reports: u0, average-gpa: u0, total-courses-completed: u0, milestones-completed: u0, milestones-total: u0, last-report-date: u0, progress-score: u100, at-risk-status: false }
          (map-get? student-progress-summary { student: tx-sender, proposal-id: proposal-id }))))
    
    (asserts! (get active recipient-info) ERR_NOT_AUTHORIZED)
    (asserts! (<= current-gpa u400) ERR_INVALID_GPA) ;; Max 4.0 GPA (400 = 4.00)
    (asserts! (> courses-enrolled u0) ERR_INVALID_PROGRESS_REPORT)
    
    (map-set progress-reports
      report-id
      {
        student: tx-sender,
        proposal-id: proposal-id,
        semester-term: semester-term,
        current-gpa: current-gpa,
        courses-completed: courses-completed,
        courses-enrolled: courses-enrolled,
        progress-description: progress-description,
        challenges-faced: challenges-faced,
        goals-next-term: goals-next-term,
        submitted-at: stacks-block-height,
        verified: false,
        verifier: none
      })
    
    (let ((new-total-reports (+ (get total-reports current-summary) u1))
          (new-total-courses (+ (get total-courses-completed current-summary) courses-completed))
          (new-average-gpa (calculate-new-average-gpa (get average-gpa current-summary) (get total-reports current-summary) current-gpa)))
      
      (map-set student-progress-summary
        { student: tx-sender, proposal-id: proposal-id }
        (merge current-summary {
          total-reports: new-total-reports,
          average-gpa: new-average-gpa,
          total-courses-completed: new-total-courses,
          last-report-date: stacks-block-height,
          progress-score: (calculate-progress-score new-average-gpa new-total-courses),
          at-risk-status: (< current-gpa u250) ;; Below 2.5 GPA considered at-risk
        })))
    
    (var-set progress-report-counter report-id)
    (ok report-id)))

(define-public (set-academic-milestone
  (proposal-id uint)
  (milestone-type uint)
  (milestone-description (string-ascii 200))
  (target-value uint)
  (target-date uint))
  (let ((milestone-id (+ (var-get milestone-counter) u1))
        (recipient-info (unwrap! (map-get? scholarship-recipients { student: tx-sender, proposal-id: proposal-id }) ERR_SCHOLARSHIP_NOT_FOUND))
        (current-summary (unwrap! (map-get? student-progress-summary { student: tx-sender, proposal-id: proposal-id }) ERR_SCHOLARSHIP_NOT_FOUND)))
    
    (asserts! (get active recipient-info) ERR_NOT_AUTHORIZED)
    (asserts! (<= milestone-type MILESTONE_PROJECT_COMPLETE) ERR_INVALID_MILESTONE_TYPE)
    (asserts! (>= milestone-type MILESTONE_SEMESTER_COMPLETE) ERR_INVALID_MILESTONE_TYPE)
    (asserts! (> target-date stacks-block-height) ERR_INVALID_PROGRESS_REPORT)
    
    (map-set academic-milestones
      milestone-id
      {
        student: tx-sender,
        proposal-id: proposal-id,
        milestone-type: milestone-type,
        milestone-description: milestone-description,
        target-value: target-value,
        target-date: target-date,
        completed: false,
        completed-at: none,
        completion-proof: none
      })
    
    (map-set student-progress-summary
      { student: tx-sender, proposal-id: proposal-id }
      (merge current-summary { milestones-total: (+ (get milestones-total current-summary) u1) }))
    
    (var-set milestone-counter milestone-id)
    (ok milestone-id)))

(define-public (complete-milestone (milestone-id uint) (completion-proof (string-ascii 200)))
  (let ((milestone (unwrap! (map-get? academic-milestones milestone-id) ERR_MILESTONE_NOT_FOUND))
        (current-summary (unwrap! (map-get? student-progress-summary { student: tx-sender, proposal-id: (get proposal-id milestone) }) ERR_SCHOLARSHIP_NOT_FOUND)))
    
    (asserts! (is-eq (get student milestone) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (not (get completed milestone)) ERR_MILESTONE_ALREADY_COMPLETED)
    
    (map-set academic-milestones
      milestone-id
      (merge milestone {
        completed: true,
        completed-at: (some stacks-block-height),
        completion-proof: (some completion-proof)
      }))
    
    (map-set student-progress-summary
      { student: tx-sender, proposal-id: (get proposal-id milestone) }
      (merge current-summary { milestones-completed: (+ (get milestones-completed current-summary) u1) }))
    
    (ok true)))

(define-public (verify-progress-report (report-id uint))
  (let ((report (unwrap! (map-get? progress-reports report-id) ERR_INVALID_PROGRESS_REPORT))
        (verifier-contribution (default-to u0 (contract-call? .Scholara get-member-contribution tx-sender))))
    
    (asserts! (> verifier-contribution u500000) ERR_NOT_AUTHORIZED) ;; Must have contributed at least 0.5 STX
    (asserts! (not (get verified report)) ERR_INVALID_PROGRESS_REPORT)
    
    (map-set progress-reports
      report-id
      (merge report {
        verified: true,
        verifier: (some tx-sender)
      }))
    
    (ok true)))

;; read-only functions
(define-read-only (get-scholarship-recipient (student principal) (proposal-id uint))
  (map-get? scholarship-recipients { student: student, proposal-id: proposal-id }))

(define-read-only (get-progress-report (report-id uint))
  (map-get? progress-reports report-id))

(define-read-only (get-academic-milestone (milestone-id uint))
  (map-get? academic-milestones milestone-id))

(define-read-only (get-student-progress-summary (student principal) (proposal-id uint))
  (map-get? student-progress-summary { student: student, proposal-id: proposal-id }))

(define-read-only (get-progress-report-counter)
  (var-get progress-report-counter))

(define-read-only (get-milestone-counter)
  (var-get milestone-counter))

;; private functions
(define-private (calculate-new-average-gpa (current-avg uint) (report-count uint) (new-gpa uint))
  (if (is-eq report-count u0)
    new-gpa
    (/ (+ (* current-avg report-count) new-gpa) (+ report-count u1))))

(define-private (calculate-progress-score (avg-gpa uint) (courses-completed uint))
  (let ((gpa-score (* avg-gpa u20))
        (course-score (* courses-completed u5)))
    (if (> (+ gpa-score course-score) u100)
      u100
      (+ gpa-score course-score))))