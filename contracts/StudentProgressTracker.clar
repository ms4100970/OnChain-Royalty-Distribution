;; title: Student Progress Tracker
;; version: 1.0.0
;; summary: Detailed progress tracking for students within courses
;; description: Tracks lesson completion, study time, streaks, and learning milestones for enhanced educational experience

(define-constant CONTRACT_OWNER tx-sender)

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_NOT_FOUND (err u201))
(define-constant ERR_INVALID_LESSON (err u202))
(define-constant ERR_ALREADY_COMPLETED (err u203))
(define-constant ERR_INVALID_TIME (err u204))
(define-constant ERR_NOT_ENROLLED (err u205))

;; Data variables
(define-data-var next-milestone-id uint u1)

;; Maps for tracking progress
(define-map lesson-progress
    {student: principal, course-id: uint, lesson-id: uint}
    {
        completed: bool,
        completion-date: uint,
        time-spent: uint,
        attempts: uint,
        score: uint
    }
)

(define-map student-course-analytics
    {student: principal, course-id: uint}
    {
        total-lessons: uint,
        completed-lessons: uint,
        total-time-spent: uint,
        current-streak: uint,
        longest-streak: uint,
        last-activity: uint,
        start-date: uint
    }
)

(define-map learning-milestones
    uint
    {
        student: principal,
        course-id: uint,
        milestone-type: (string-ascii 30),
        achievement-date: uint,
        description: (string-ascii 100)
    }
)

(define-map daily-study-sessions
    {student: principal, date: uint}
    {
        sessions-count: uint,
        total-time: uint,
        courses-studied: (list 10 uint)
    }
)

;; Read-only functions to check enrollment status
(define-read-only (is-student-enrolled (student principal) (course-id uint))
    (let ((enrollment-check (contract-call? .Skillhub get-enrollment u1)))
        (match enrollment-check
            enrollment-result (match enrollment-result
                enrollment-data (is-eq (get student enrollment-data) student)
                false
            )
            false
        )
    )
)

;; Public functions
(define-public (complete-lesson (course-id uint) (lesson-id uint) (time-spent uint) (score uint))
    (let ((current-date (/ stacks-block-height u144)))  ;; Approximate daily blocks
        (asserts! (> lesson-id u0) ERR_INVALID_LESSON)
        (asserts! (> time-spent u0) ERR_INVALID_TIME)
        (asserts! (<= score u100) ERR_INVALID_TIME)
        
        ;; Check if lesson already completed
        (let ((existing-progress (map-get? lesson-progress {student: tx-sender, course-id: course-id, lesson-id: lesson-id})))
            (match existing-progress
                progress (asserts! (not (get completed progress)) ERR_ALREADY_COMPLETED)
                true
            )
        )
        
        ;; Update lesson progress
        (map-set lesson-progress {student: tx-sender, course-id: course-id, lesson-id: lesson-id} {
            completed: true,
            completion-date: stacks-block-height,
            time-spent: time-spent,
            attempts: (match (map-get? lesson-progress {student: tx-sender, course-id: course-id, lesson-id: lesson-id})
                existing (+ (get attempts existing) u1)
                u1
            ),
            score: score
        })
        
        ;; Update course analytics
        (let ((analytics (default-to {
                total-lessons: u0,
                completed-lessons: u0,
                total-time-spent: u0,
                current-streak: u0,
                longest-streak: u0,
                last-activity: u0,
                start-date: stacks-block-height
            } (map-get? student-course-analytics {student: tx-sender, course-id: course-id}))))
            
            (let ((new-completed (+ (get completed-lessons analytics) u1))
                  (new-total-time (+ (get total-time-spent analytics) time-spent))
                  (days-since-last (if (> (get last-activity analytics) u0)
                      (- current-date (/ (get last-activity analytics) u144))
                      u0
                  ))
                  (new-streak (if (<= days-since-last u1)
                      (+ (get current-streak analytics) u1)
                      u1
                  )))
                
                (map-set student-course-analytics {student: tx-sender, course-id: course-id} (merge analytics {
                    completed-lessons: new-completed,
                    total-time-spent: new-total-time,
                    current-streak: new-streak,
                    longest-streak: (if (> new-streak (get longest-streak analytics)) new-streak (get longest-streak analytics)),
                    last-activity: stacks-block-height
                }))
                
                ;; Update daily study session
                (update-daily-session course-id time-spent current-date)
                
                ;; Check for milestones
                (try! (check-and-award-milestones course-id new-completed new-streak new-total-time))
                
                (ok true)
            )
        )
    )
)

(define-private (update-daily-session (course-id uint) (time-spent uint) (current-date uint))
    (let ((session (default-to {
            sessions-count: u0,
            total-time: u0,
            courses-studied: (list)
        } (map-get? daily-study-sessions {student: tx-sender, date: current-date}))))
        
        (map-set daily-study-sessions {student: tx-sender, date: current-date} {
            sessions-count: (+ (get sessions-count session) u1),
            total-time: (+ (get total-time session) time-spent),
            courses-studied: (unwrap-panic (as-max-len? (append (get courses-studied session) course-id) u10))
        })
        (ok true)
    )
)

(define-private (check-and-award-milestones (course-id uint) (completed-lessons uint) (current-streak uint) (total-time uint))
    (begin
        ;; First lesson milestone
        (if (is-eq completed-lessons u1)
            (try! (award-milestone course-id "first-lesson" "Completed your first lesson!"))
            true
        )
        
        ;; Streak milestones
        (if (is-eq current-streak u7)
            (try! (award-milestone course-id "week-streak" "7-day learning streak achieved!"))
            true
        )
        
        ;; Time-based milestones
        (if (>= total-time u3600)  ;; 1 hour in seconds
            (try! (award-milestone course-id "dedicated-learner" "Spent over 1 hour studying!"))
            true
        )
        
        (ok true)
    )
)

(define-private (award-milestone (course-id uint) (milestone-type (string-ascii 30)) (description (string-ascii 100)))
    (let ((milestone-id (var-get next-milestone-id)))
        (map-set learning-milestones milestone-id {
            student: tx-sender,
            course-id: course-id,
            milestone-type: milestone-type,
            achievement-date: stacks-block-height,
            description: description
        })
        (var-set next-milestone-id (+ milestone-id u1))
        (ok milestone-id)
    )
)

(define-public (update-course-structure (course-id uint) (total-lessons uint))
    (let ((analytics (default-to {
            total-lessons: u0,
            completed-lessons: u0,
            total-time-spent: u0,
            current-streak: u0,
            longest-streak: u0,
            last-activity: u0,
            start-date: stacks-block-height
        } (map-get? student-course-analytics {student: tx-sender, course-id: course-id}))))
        
        (map-set student-course-analytics {student: tx-sender, course-id: course-id} (merge analytics {
            total-lessons: total-lessons
        }))
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-lesson-progress (student principal) (course-id uint) (lesson-id uint))
    (ok (map-get? lesson-progress {student: student, course-id: course-id, lesson-id: lesson-id}))
)

(define-read-only (get-student-analytics (student principal) (course-id uint))
    (ok (map-get? student-course-analytics {student: student, course-id: course-id}))
)

(define-read-only (get-learning-milestone (milestone-id uint))
    (ok (map-get? learning-milestones milestone-id))
)

(define-read-only (get-daily-study-session (student principal) (date uint))
    (ok (map-get? daily-study-sessions {student: student, date: date}))
)

(define-read-only (calculate-progress-percentage (student principal) (course-id uint))
    (match (map-get? student-course-analytics {student: student, course-id: course-id})
        analytics (if (> (get total-lessons analytics) u0)
            (ok (/ (* (get completed-lessons analytics) u100) (get total-lessons analytics)))
            (ok u0)
        )
        (ok u0)
    )
)

(define-read-only (get-study-streak (student principal) (course-id uint))
    (match (map-get? student-course-analytics {student: student, course-id: course-id})
        analytics (ok {
            current-streak: (get current-streak analytics),
            longest-streak: (get longest-streak analytics)
        })
        (ok {current-streak: u0, longest-streak: u0})
    )
)
