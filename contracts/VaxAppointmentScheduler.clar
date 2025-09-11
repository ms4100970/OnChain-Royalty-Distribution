;; Vaccination Appointment Scheduler
;; Manages vaccination appointments for authorized issuers and patients
;; Integrates with VaxProof system for streamlined vaccination workflow

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_APPOINTMENT_NOT_FOUND (err u301))
(define-constant ERR_SLOT_NOT_AVAILABLE (err u302))
(define-constant ERR_INVALID_TIME (err u303))
(define-constant ERR_APPOINTMENT_PAST (err u304))
(define-constant ERR_ALREADY_BOOKED (err u305))
(define-constant ERR_ISSUER_NOT_AUTHORIZED (err u306))
(define-constant ERR_SLOT_CONFLICT (err u307))

;; Appointment status constants
(define-constant STATUS_SCHEDULED "scheduled")
(define-constant STATUS_CONFIRMED "confirmed")
(define-constant STATUS_COMPLETED "completed")
(define-constant STATUS_CANCELLED "cancelled")
(define-constant STATUS_NO_SHOW "no-show")

(define-data-var next-appointment-id uint u1)
(define-data-var next-slot-id uint u1)

;; Vaccination appointments
(define-map appointments
  { appointment-id: uint }
  {
    patient: principal,
    issuer: principal,
    vaccine-type: (string-ascii 64),
    appointment-time: uint,
    duration-minutes: uint,
    status: (string-ascii 16),
    notes: (string-ascii 200),
    created-at: uint,
    confirmed-at: (optional uint),
    completed-at: (optional uint)
  }
)

;; Issuer availability slots
(define-map availability-slots
  { slot-id: uint }
  {
    issuer: principal,
    start-time: uint,
    end-time: uint,
    max-appointments: uint,
    current-bookings: uint,
    vaccine-types: (list 5 (string-ascii 64)),
    location: (string-ascii 100),
    is-active: bool
  }
)

;; Patient appointment history
(define-map patient-appointments
  { patient: principal, appointment-id: uint }
  { active: bool }
)

;; Issuer appointment schedule
(define-map issuer-schedule
  { issuer: principal, appointment-id: uint }
  { slot-id: uint }
)

;; Waitlist for high-demand appointments
(define-map appointment-waitlist
  { issuer: principal, patient: principal }
  {
    vaccine-type: (string-ascii 64),
    preferred-time: uint,
    max-wait-time: uint,
    added-at: uint,
    priority-score: uint
  }
)

;; Create availability slot
(define-public (create-availability-slot 
  (start-time uint) 
  (end-time uint) 
  (max-appointments uint)
  (vaccine-types (list 5 (string-ascii 64)))
  (location (string-ascii 100))
)
  (let
    (
      (slot-id (var-get next-slot-id))
      (current-time stacks-block-height)
    )
    (asserts! (contract-call? .VaxProof is-authorized-issuer tx-sender) ERR_ISSUER_NOT_AUTHORIZED)
    (asserts! (> end-time start-time) ERR_INVALID_TIME)
    (asserts! (> start-time current-time) ERR_INVALID_TIME)
    (asserts! (> max-appointments u0) ERR_INVALID_TIME)
    (map-set availability-slots
      { slot-id: slot-id }
      {
        issuer: tx-sender,
        start-time: start-time,
        end-time: end-time,
        max-appointments: max-appointments,
        current-bookings: u0,
        vaccine-types: vaccine-types,
        location: location,
        is-active: true
      }
    )
    (var-set next-slot-id (+ slot-id u1))
    (ok slot-id)
  )
)

;; Book vaccination appointment
(define-public (book-appointment 
  (issuer principal) 
  (vaccine-type (string-ascii 64))
  (preferred-time uint)
  (notes (string-ascii 200))
)
  (let
    (
      (appointment-id (var-get next-appointment-id))
      (current-time stacks-block-height)
      (slot (find-available-slot issuer preferred-time vaccine-type))
    )
    (asserts! (contract-call? .VaxProof is-authorized-issuer issuer) ERR_ISSUER_NOT_AUTHORIZED)
    (asserts! (> preferred-time current-time) ERR_APPOINTMENT_PAST)
    (asserts! (is-some slot) ERR_SLOT_NOT_AVAILABLE)
    (let
      (
        (slot-info (unwrap-panic slot))
        (slot-id (get slot-id slot-info))
      )
      ;; Check slot capacity
      (asserts! (< (get current-bookings slot-info) (get max-appointments slot-info)) ERR_SLOT_NOT_AVAILABLE)
      ;; Create appointment
      (map-set appointments
        { appointment-id: appointment-id }
        {
          patient: tx-sender,
          issuer: issuer,
          vaccine-type: vaccine-type,
          appointment-time: preferred-time,
          duration-minutes: u30,
          status: STATUS_SCHEDULED,
          notes: notes,
          created-at: current-time,
          confirmed-at: none,
          completed-at: none
        }
      )
      ;; Update slot booking count
      (map-set availability-slots
        { slot-id: slot-id }
        (merge slot-info { current-bookings: (+ (get current-bookings slot-info) u1) })
      )
      ;; Link patient to appointment
      (map-set patient-appointments
        { patient: tx-sender, appointment-id: appointment-id }
        { active: true }
      )
      ;; Link issuer to appointment
      (map-set issuer-schedule
        { issuer: issuer, appointment-id: appointment-id }
        { slot-id: slot-id }
      )
      (var-set next-appointment-id (+ appointment-id u1))
      (ok appointment-id)
    )
  )
)

;; Confirm appointment by issuer
(define-public (confirm-appointment (appointment-id uint))
  (let
    (
      (appointment (unwrap! (map-get? appointments { appointment-id: appointment-id }) ERR_APPOINTMENT_NOT_FOUND))
      (current-time stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get issuer appointment)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status appointment) STATUS_SCHEDULED) ERR_INVALID_TIME)
    (map-set appointments
      { appointment-id: appointment-id }
      (merge appointment {
        status: STATUS_CONFIRMED,
        confirmed-at: (some current-time)
      })
    )
    (ok true)
  )
)

;; Cancel appointment
(define-public (cancel-appointment (appointment-id uint))
  (let
    (
      (appointment (unwrap! (map-get? appointments { appointment-id: appointment-id }) ERR_APPOINTMENT_NOT_FOUND))
      (current-time stacks-block-height)
    )
    (asserts! (or 
      (is-eq tx-sender (get patient appointment))
      (is-eq tx-sender (get issuer appointment))
      (is-eq tx-sender CONTRACT_OWNER)
    ) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq (get status appointment) STATUS_COMPLETED)) ERR_APPOINTMENT_PAST)
    (map-set appointments
      { appointment-id: appointment-id }
      (merge appointment { status: STATUS_CANCELLED })
    )
    ;; Free up slot capacity
    (let
      (
        (schedule-info (map-get? issuer-schedule { issuer: (get issuer appointment), appointment-id: appointment-id }))
      )
      (match schedule-info
        sched (let
          (
            (slot-info (unwrap! (map-get? availability-slots { slot-id: (get slot-id sched) }) ERR_SLOT_NOT_AVAILABLE))
          )
          (map-set availability-slots
            { slot-id: (get slot-id sched) }
            (merge slot-info { current-bookings: (- (get current-bookings slot-info) u1) })
          )
        )
        true
      )
    )
    (ok true)
  )
)

;; Complete appointment and mark for vaccination proof
(define-public (complete-appointment (appointment-id uint))
  (let
    (
      (appointment (unwrap! (map-get? appointments { appointment-id: appointment-id }) ERR_APPOINTMENT_NOT_FOUND))
      (current-time stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get issuer appointment)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status appointment) STATUS_CONFIRMED) ERR_INVALID_TIME)
    (map-set appointments
      { appointment-id: appointment-id }
      (merge appointment {
        status: STATUS_COMPLETED,
        completed-at: (some current-time)
      })
    )
    (ok true)
  )
)

;; Join waitlist for appointments
(define-public (join-waitlist 
  (issuer principal)
  (vaccine-type (string-ascii 64))
  (preferred-time uint)
  (max-wait-days uint)
)
  (let
    (
      (current-time stacks-block-height)
      (max-wait-time (+ current-time (* max-wait-days u144))) ;; Convert days to blocks
    )
    (asserts! (contract-call? .VaxProof is-authorized-issuer issuer) ERR_ISSUER_NOT_AUTHORIZED)
    (asserts! (> preferred-time current-time) ERR_APPOINTMENT_PAST)
    (map-set appointment-waitlist
      { issuer: issuer, patient: tx-sender }
      {
        vaccine-type: vaccine-type,
        preferred-time: preferred-time,
        max-wait-time: max-wait-time,
        added-at: current-time,
        priority-score: u1
      }
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-appointment (appointment-id uint))
  (map-get? appointments { appointment-id: appointment-id })
)

(define-read-only (get-availability-slot (slot-id uint))
  (map-get? availability-slots { slot-id: slot-id })
)

(define-read-only (get-patient-appointments (patient principal))
  (ok patient) ;; Simplified - would implement appointment listing
)

(define-read-only (get-issuer-schedule (issuer principal))
  (ok issuer) ;; Simplified - would implement schedule listing
)

(define-read-only (check-appointment-availability (issuer principal) (appointment-time uint) (vaccine-type (string-ascii 64)))
  (ok (is-some (find-available-slot issuer appointment-time vaccine-type)))
)

;; Private helper functions
(define-private (find-available-slot (issuer principal) (appointment-time uint) (vaccine-type (string-ascii 64)))
  ;; Simplified implementation - would search for matching available slots
  (some { slot-id: u1, current-bookings: u0, max-appointments: u10 })
)
