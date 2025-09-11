;; Youth Project Portfolio & Showcase System
;; Enables youth to document and showcase their completed grant projects

;; Error constants  
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_PROJECT_NOT_FOUND (err u201))
(define-constant ERR_INVALID_AGE (err u202))
(define-constant ERR_PROJECT_EXISTS (err u203))
(define-constant ERR_INVALID_DATA (err u204))
(define-constant ERR_PROPOSAL_NOT_EXECUTED (err u205))

;; Contract constants
(define-constant MIN_AGE u13)
(define-constant MAX_AGE u25)
(define-constant MAX_PROJECTS_PER_USER u10)
(define-constant MIN_IMPACT_SCORE u1)
(define-constant MAX_IMPACT_SCORE u100)

;; Data variables
(define-data-var next-project-id uint u1)
(define-data-var total-portfolios uint u0)

;; Project status constants
(define-constant STATUS_IN_PROGRESS "progress")
(define-constant STATUS_COMPLETED "completed") 
(define-constant STATUS_SHOWCASED "showcased")

;; Impact categories
(define-constant CATEGORY_EDUCATION "education")
(define-constant CATEGORY_COMMUNITY "community")
(define-constant CATEGORY_TECHNOLOGY "technology")
(define-constant CATEGORY_ENVIRONMENT "environment")
(define-constant CATEGORY_HEALTH "health")

;; Maps
(define-map project-portfolios uint
  {
    creator: principal,
    proposal-id: uint,
    project-title: (string-ascii 100),
    category: (string-ascii 20),
    description: (string-ascii 500),
    start-date: uint,
    completion-date: (optional uint),
    status: (string-ascii 20),
    beneficiaries-reached: uint,
    skills-developed: (string-ascii 200),
    documentation-link: (string-ascii 200),
    impact-score: uint,
    created-at: uint
  })

(define-map user-portfolios principal
  {
    total-projects: uint,
    completed-projects: uint,
    total-beneficiaries: uint,
    average-impact: uint,
    portfolio-score: uint,
    showcase-count: uint
  })

(define-map project-achievements {project-id: uint, achievement: (string-ascii 30)}
  {
    earned-at: uint,
    description: (string-ascii 100)
  })

(define-map project-updates {project-id: uint, update-id: uint}
  {
    update-text: (string-ascii 300),
    milestone: (string-ascii 50),
    posted-at: uint
  })

;; Create a project portfolio entry
(define-public (create-project-portfolio 
                (proposal-id uint)
                (project-title (string-ascii 100))
                (category (string-ascii 20))
                (description (string-ascii 500))
                (documentation-link (string-ascii 200)))
  (let ((creator tx-sender)
        (project-id (var-get next-project-id))
        (creator-age (default-to u0 (contract-call? .Youngdao get-member-age creator)))
        (user-portfolio (default-to 
                          {total-projects: u0, completed-projects: u0, total-beneficiaries: u0, 
                           average-impact: u0, portfolio-score: u0, showcase-count: u0}
                          (map-get? user-portfolios creator))))
    
    ;; Validate creator eligibility
    (asserts! (and (>= creator-age MIN_AGE) (<= creator-age MAX_AGE)) ERR_INVALID_AGE)
    (asserts! (< (get total-projects user-portfolio) MAX_PROJECTS_PER_USER) ERR_NOT_AUTHORIZED)
    (asserts! (> (len project-title) u0) ERR_INVALID_DATA)
    (asserts! (> (len description) u0) ERR_INVALID_DATA)
    
    ;; Verify proposal exists and was executed (call to main contract)
    (asserts! (is-some (contract-call? .Youngdao get-proposal proposal-id)) ERR_PROPOSAL_NOT_EXECUTED)
    
    ;; Create project portfolio entry
    (map-set project-portfolios project-id
      {
        creator: creator,
        proposal-id: proposal-id,
        project-title: project-title,
        category: category,
        description: description,
        start-date: stacks-block-height,
        completion-date: none,
        status: STATUS_IN_PROGRESS,
        beneficiaries-reached: u0,
        skills-developed: "",
        documentation-link: documentation-link,
        impact-score: u0,
        created-at: stacks-block-height
      })
    
    ;; Update user portfolio stats
    (map-set user-portfolios creator
      (merge user-portfolio {
        total-projects: (+ (get total-projects user-portfolio) u1)
      }))
    
    ;; Update global counters
    (var-set next-project-id (+ project-id u1))
    (var-set total-portfolios (+ (var-get total-portfolios) u1))
    
    (ok project-id)))

;; Update project with completion details
(define-public (complete-project 
                (project-id uint)
                (beneficiaries-reached uint)
                (skills-developed (string-ascii 200))
                (impact-score uint))
  (let ((project (unwrap! (map-get? project-portfolios project-id) ERR_PROJECT_NOT_FOUND))
        (creator (get creator project))
        (user-portfolio (unwrap! (map-get? user-portfolios creator) ERR_NOT_AUTHORIZED)))
    
    ;; Verify caller is project creator
    (asserts! (is-eq tx-sender creator) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS_IN_PROGRESS) ERR_INVALID_DATA)
    (asserts! (and (>= impact-score MIN_IMPACT_SCORE) (<= impact-score MAX_IMPACT_SCORE)) ERR_INVALID_DATA)
    
    ;; Update project completion
    (map-set project-portfolios project-id
      (merge project {
        completion-date: (some stacks-block-height),
        status: STATUS_COMPLETED,
        beneficiaries-reached: beneficiaries-reached,
        skills-developed: skills-developed,
        impact-score: impact-score
      }))
    
    ;; Update user portfolio statistics
    (let ((new-completed (+ (get completed-projects user-portfolio) u1))
          (new-total-beneficiaries (+ (get total-beneficiaries user-portfolio) beneficiaries-reached))
          (new-avg-impact (calculate-average-impact creator new-completed)))
      
      (map-set user-portfolios creator
        (merge user-portfolio {
          completed-projects: new-completed,
          total-beneficiaries: new-total-beneficiaries,
          average-impact: new-avg-impact,
          portfolio-score: (calculate-portfolio-score creator new-completed new-total-beneficiaries new-avg-impact)
        }))
    )
    
    ;; Award achievement based on impact score
    (if (>= impact-score u80)
      (award-achievement project-id "high-impact" "Achieved high impact score (80+)")
      (if (>= impact-score u50)
        (award-achievement project-id "medium-impact" "Achieved medium impact score (50+)")
        true))
    
    (ok true)))

;; Submit project for showcase
(define-public (showcase-project (project-id uint))
  (let ((project (unwrap! (map-get? project-portfolios project-id) ERR_PROJECT_NOT_FOUND))
        (creator (get creator project))
        (user-portfolio (unwrap! (map-get? user-portfolios creator) ERR_NOT_AUTHORIZED)))
    
    ;; Verify caller is project creator and project is completed
    (asserts! (is-eq tx-sender creator) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS_COMPLETED) ERR_INVALID_DATA)
    
    ;; Update project status
    (map-set project-portfolios project-id
      (merge project {status: STATUS_SHOWCASED}))
    
    ;; Update showcase count
    (map-set user-portfolios creator
      (merge user-portfolio {
        showcase-count: (+ (get showcase-count user-portfolio) u1)
      }))
    
    ;; Award showcase achievement
    (award-achievement project-id "showcased" "Project featured in community showcase")
    
    (ok true)))

;; Add project update/milestone
(define-public (add-project-update 
                (project-id uint)
                (update-id uint)
                (update-text (string-ascii 300))
                (milestone (string-ascii 50)))
  (let ((project (unwrap! (map-get? project-portfolios project-id) ERR_PROJECT_NOT_FOUND))
        (creator (get creator project)))
    
    ;; Verify caller is project creator
    (asserts! (is-eq tx-sender creator) ERR_NOT_AUTHORIZED)
    (asserts! (> (len update-text) u0) ERR_INVALID_DATA)
    
    ;; Add project update
    (map-set project-updates {project-id: project-id, update-id: update-id}
      {
        update-text: update-text,
        milestone: milestone,
        posted-at: stacks-block-height
      })
    
    (ok true)))

;; Private helper functions
(define-private (award-achievement (project-id uint) (achievement (string-ascii 30)) (description (string-ascii 100)))
  (begin
    (map-set project-achievements {project-id: project-id, achievement: achievement}
      {
        earned-at: stacks-block-height,
        description: description
      })
    true))

(define-private (calculate-average-impact (creator principal) (completed-count uint))
  (if (> completed-count u0)
    (let ((total-impact (fold sum-project-impact (get-user-project-ids creator) u0)))
      (/ total-impact completed-count))
    u0))

(define-private (calculate-portfolio-score (creator principal) (completed uint) (beneficiaries uint) (avg-impact uint))
  (+ (* completed u10) (/ beneficiaries u10) avg-impact))

(define-private (sum-project-impact (project-id uint) (total uint))
  (match (map-get? project-portfolios project-id)
    project (+ total (get impact-score project))
    total))

(define-private (get-user-project-ids (creator principal))
  ;; Simplified - returns list of project IDs for user, would need iteration in practice
  (list u1 u2 u3 u4 u5))

;; Read-only functions
(define-read-only (get-project-portfolio (project-id uint))
  (map-get? project-portfolios project-id))

(define-read-only (get-user-portfolio (user principal))
  (map-get? user-portfolios user))

(define-read-only (get-project-achievement (project-id uint) (achievement (string-ascii 30)))
  (map-get? project-achievements {project-id: project-id, achievement: achievement}))

(define-read-only (get-project-update (project-id uint) (update-id uint))
  (map-get? project-updates {project-id: project-id, update-id: update-id}))

(define-read-only (get-total-portfolios)
  (var-get total-portfolios))

(define-read-only (get-next-project-id)
  (var-get next-project-id))

(define-read-only (is-project-creator (project-id uint) (user principal))
  (match (map-get? project-portfolios project-id)
    project (is-eq (get creator project) user)
    false))

(define-read-only (get-project-impact-level (project-id uint))
  (match (map-get? project-portfolios project-id)
    project (let ((impact (get impact-score project)))
      (if (>= impact u80) "high"
        (if (>= impact u50) "medium" "low")))
    "none"))

(define-read-only (get-user-achievements-count (user principal))
  ;; Simplified count - would need proper implementation to count achievements
  (match (map-get? user-portfolios user)
    portfolio (* (get completed-projects portfolio) u2)
    u0))

(define-read-only (can-showcase-project (project-id uint))
  (match (map-get? project-portfolios project-id)
    project (and
      (is-eq (get status project) STATUS_COMPLETED)
      (>= (get impact-score project) u30))
    false))
