;; Session Scheduling & Calendar Management System
;; Allows tutors to set availability and students to schedule sessions

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_TUTOR_NOT_FOUND (err u201))
(define-constant ERR_INVALID_TIME_SLOT (err u202))
(define-constant ERR_SLOT_NOT_AVAILABLE (err u203))
(define-constant ERR_SLOT_ALREADY_BOOKED (err u204))
(define-constant ERR_INVALID_DATE (err u205))
(define-constant ERR_SCHEDULE_NOT_FOUND (err u206))
(define-constant ERR_CANCELLATION_TOO_LATE (err u207))

;; Data variables
(define-data-var next-schedule-id uint u1)
(define-data-var min-advance-booking uint u144) ;; 24 hours in blocks

;; Tutor availability slots (weekly recurring)
(define-map tutor-availability
  { tutor-id: uint, day-of-week: uint, hour: uint } ;; day: 0-6 (Sun-Sat), hour: 0-23
  {
    available: bool,
    max-sessions: uint,
    created-at: uint,
    updated-at: uint
  }
)

;; Scheduled sessions with specific date/time
(define-map scheduled-sessions
  { schedule-id: uint }
  {
    tutor-id: uint,
    student: principal,
    date-blocks: uint, ;; stacks block height for the date
    time-hour: uint, ;; hour of the day (0-23)
    duration-hours: uint,
    status: (string-ascii 20), ;; "scheduled", "confirmed", "completed", "cancelled"
    booking-fee: uint,
    created-at: uint,
    confirmed-at: (optional uint),
    cancelled-at: (optional uint),
    cancellation-reason: (optional (string-ascii 100)),
    reminder-sent: bool
  }
)

;; Daily session count per tutor slot
(define-map daily-slot-bookings
  { tutor-id: uint, date-blocks: uint, hour: uint }
  { sessions-count: uint }
)

;; Student scheduling preferences
(define-map student-preferences
  { student: principal }
  {
    preferred-hours: (list 12 uint), ;; preferred hours of day
    advance-booking-days: uint, ;; how many days ahead to book
    reminder-preference: bool,
    timezone-offset: int, ;; hours offset from UTC
    created-at: uint
  }
)

;; Read-only functions
(define-read-only (get-tutor-availability (tutor-id uint) (day-of-week uint) (hour uint))
  (map-get? tutor-availability { tutor-id: tutor-id, day-of-week: day-of-week, hour: hour })
)

(define-read-only (get-scheduled-session (schedule-id uint))
  (map-get? scheduled-sessions { schedule-id: schedule-id })
)

(define-read-only (get-daily-bookings (tutor-id uint) (date-blocks uint) (hour uint))
  (default-to { sessions-count: u0 } 
    (map-get? daily-slot-bookings { tutor-id: tutor-id, date-blocks: date-blocks, hour: hour }))
)

(define-read-only (get-student-preferences (student principal))
  (map-get? student-preferences { student: student })
)

(define-read-only (is-slot-available (tutor-id uint) (date-blocks uint) (hour uint))
  (let
    (
      (day-of-week (mod (/ date-blocks u144) u7)) ;; Calculate day of week
      (availability (map-get? tutor-availability { tutor-id: tutor-id, day-of-week: day-of-week, hour: hour }))
      (daily-bookings (get-daily-bookings tutor-id date-blocks hour))
    )
    (match availability
      slot-info (and 
        (get available slot-info)
        (< (get sessions-count daily-bookings) (get max-sessions slot-info))
      )
      false
    )
  )
)

(define-read-only (get-contract-stats)
  {
    next-schedule-id: (var-get next-schedule-id),
    min-advance-booking: (var-get min-advance-booking)
  }
)

;; Public functions for tutors
(define-public (set-availability (tutor-id uint) (day-of-week uint) (hour uint) (available bool) (max-sessions uint))
  (begin
    ;; Verify tutor exists and caller is the tutor (simplified check)
    (asserts! (and (>= day-of-week u0) (<= day-of-week u6)) ERR_INVALID_TIME_SLOT)
    (asserts! (and (>= hour u0) (<= hour u23)) ERR_INVALID_TIME_SLOT)
    (asserts! (and (>= max-sessions u1) (<= max-sessions u10)) ERR_INVALID_TIME_SLOT)
    
    (map-set tutor-availability
      { tutor-id: tutor-id, day-of-week: day-of-week, hour: hour }
      {
        available: available,
        max-sessions: max-sessions,
        created-at: stacks-block-height,
        updated-at: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (bulk-set-availability (tutor-id uint) (day-of-week uint) (hours (list 12 uint)) (available bool) (max-sessions uint))
  (begin
    (asserts! (and (>= day-of-week u0) (<= day-of-week u6)) ERR_INVALID_TIME_SLOT)
    (asserts! (and (>= max-sessions u1) (<= max-sessions u10)) ERR_INVALID_TIME_SLOT)
    
    (fold set-single-hour-availability hours (ok { tutor-id: tutor-id, available: available, max-sessions: max-sessions, day: day-of-week }))
  )
)

;; Public functions for students
(define-public (schedule-session (tutor-id uint) (date-blocks uint) (time-hour uint) (duration-hours uint))
  (let
    (
      (schedule-id (var-get next-schedule-id))
      (advance-blocks (- date-blocks stacks-block-height))
      (booking-fee u1000000) ;; 1 STX booking fee
    )
    (asserts! (> advance-blocks (var-get min-advance-booking)) ERR_INVALID_DATE)
    (asserts! (and (>= time-hour u0) (<= time-hour u23)) ERR_INVALID_TIME_SLOT)
    (asserts! (and (>= duration-hours u1) (<= duration-hours u4)) ERR_INVALID_TIME_SLOT)
    (asserts! (is-slot-available tutor-id date-blocks time-hour) ERR_SLOT_NOT_AVAILABLE)
    
    ;; Pay booking fee
    (try! (stx-transfer? booking-fee tx-sender (as-contract tx-sender)))
    
    ;; Create scheduled session
    (map-set scheduled-sessions
      { schedule-id: schedule-id }
      {
        tutor-id: tutor-id,
        student: tx-sender,
        date-blocks: date-blocks,
        time-hour: time-hour,
        duration-hours: duration-hours,
        status: "scheduled",
        booking-fee: booking-fee,
        created-at: stacks-block-height,
        confirmed-at: none,
        cancelled-at: none,
        cancellation-reason: none,
        reminder-sent: false
      }
    )
    
    ;; Update daily bookings count
    (let
      (
        (current-bookings (get-daily-bookings tutor-id date-blocks time-hour))
      )
      (map-set daily-slot-bookings
        { tutor-id: tutor-id, date-blocks: date-blocks, hour: time-hour }
        { sessions-count: (+ (get sessions-count current-bookings) u1) }
      )
    )
    
    (var-set next-schedule-id (+ schedule-id u1))
    (ok schedule-id)
  )
)

(define-public (confirm-scheduled-session (schedule-id uint))
  (let
    (
      (session (unwrap! (map-get? scheduled-sessions { schedule-id: schedule-id }) ERR_SCHEDULE_NOT_FOUND))
    )
    ;; Only tutor can confirm (simplified check)
    (asserts! (is-eq (get status session) "scheduled") ERR_INVALID_TIME_SLOT)
    
    (map-set scheduled-sessions
      { schedule-id: schedule-id }
      (merge session {
        status: "confirmed",
        confirmed-at: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-public (cancel-scheduled-session (schedule-id uint) (reason (string-ascii 100)))
  (let
    (
      (session (unwrap! (map-get? scheduled-sessions { schedule-id: schedule-id }) ERR_SCHEDULE_NOT_FOUND))
      (time-until-session (- (get date-blocks session) stacks-block-height))
    )
    ;; Allow cancellation if more than 24 hours in advance
    (asserts! (> time-until-session u144) ERR_CANCELLATION_TOO_LATE)
    (asserts! (or (is-eq tx-sender (get student session)) (is-eq tx-sender CONTRACT_OWNER)) ERR_NOT_AUTHORIZED)
    
    ;; Refund booking fee for cancellations with sufficient notice
    (if (> time-until-session u288) ;; 48 hours
      (try! (as-contract (stx-transfer? (get booking-fee session) tx-sender (get student session))))
      true
    )
    
    ;; Update session status
    (map-set scheduled-sessions
      { schedule-id: schedule-id }
      (merge session {
        status: "cancelled",
        cancelled-at: (some stacks-block-height),
        cancellation-reason: (some reason)
      })
    )
    
    ;; Decrease daily booking count
    (let
      (
        (current-bookings (get-daily-bookings (get tutor-id session) (get date-blocks session) (get time-hour session)))
      )
      (map-set daily-slot-bookings
        { tutor-id: (get tutor-id session), date-blocks: (get date-blocks session), hour: (get time-hour session) }
        { sessions-count: (if (> (get sessions-count current-bookings) u0) 
                           (- (get sessions-count current-bookings) u1) 
                           u0) }
      )
    )
    
    (ok true)
  )
)

(define-public (set-student-preferences (preferred-hours (list 12 uint)) (advance-days uint) (reminder-preference bool) (timezone-offset int))
  (begin
    (asserts! (and (>= advance-days u1) (<= advance-days u30)) ERR_INVALID_DATE)
    (asserts! (and (>= timezone-offset -12) (<= timezone-offset 12)) ERR_INVALID_TIME_SLOT)
    
    (map-set student-preferences
      { student: tx-sender }
      {
        preferred-hours: preferred-hours,
        advance-booking-days: advance-days,
        reminder-preference: reminder-preference,
        timezone-offset: timezone-offset,
        created-at: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Admin functions
(define-public (set-min-advance-booking (blocks uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (and (>= blocks u24) (<= blocks u1440)) ERR_INVALID_TIME_SLOT) ;; 1 hour to 10 days
    (var-set min-advance-booking blocks)
    (ok true)
  )
)

;; Private helper functions
(define-private (set-single-hour-availability (hour uint) (data (response {tutor-id: uint, available: bool, max-sessions: uint, day: uint} uint)))
  (match data
    success-data (if (and (>= hour u0) (<= hour u23))
      (begin
        (map-set tutor-availability
          { tutor-id: (get tutor-id success-data), day-of-week: (get day success-data), hour: hour }
          {
            available: (get available success-data),
            max-sessions: (get max-sessions success-data),
            created-at: stacks-block-height,
            updated-at: stacks-block-height
          }
        )
        (ok success-data)
      )
      (err ERR_INVALID_TIME_SLOT)
    )
    error-val (err error-val)
  )
)
