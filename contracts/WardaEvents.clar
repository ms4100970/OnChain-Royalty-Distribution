;; Community Event Management System
;; Enables residents to create and manage community events within their ward

;; Error constants
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-event-not-found (err u122))
(define-constant err-event-full (err u123))
(define-constant err-already-registered (err u124))
(define-constant err-registration-closed (err u125))
(define-constant err-event-already-started (err u126))
(define-constant err-unauthorized-action (err u127))
(define-constant err-invalid-date (err u128))

;; Contract constants
(define-constant contract-owner tx-sender)
(define-constant event-creation-fee u500000) ;; 0.5 STX

;; Data variables
(define-data-var next-event-id uint u1)
(define-data-var total-events uint u0)

;; Event status constants
(define-constant STATUS-PLANNING "planning")
(define-constant STATUS-ACTIVE "active")  
(define-constant STATUS-COMPLETED "completed")
(define-constant STATUS-CANCELLED "cancelled")

;; Event type constants
(define-constant TYPE-MEETING "meeting")
(define-constant TYPE-WORKSHOP "workshop")
(define-constant TYPE-SOCIAL "social")
(define-constant TYPE-CLEANUP "cleanup")
(define-constant TYPE-OTHER "other")

;; Maps
(define-map community-events uint
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    organizer: principal,
    ward: (string-ascii 50),
    event-type: (string-ascii 20),
    location: (string-ascii 100),
    start-time: uint,
    end-time: uint,
    max-capacity: uint,
    current-attendees: uint,
    registration-deadline: uint,
    status: (string-ascii 20),
    created-at: uint,
    total-cost: uint,
    resource-id: (optional uint)
  })

(define-map event-registrations {event-id: uint, attendee: principal}
  {
    registered-at: uint,
    confirmed: bool,
    notes: (string-ascii 200)
  })

(define-map event-feedback {event-id: uint, attendee: principal}
  {
    rating: uint,
    feedback: (string-ascii 300),
    submitted-at: uint
  })

(define-map organizer-stats principal
  {
    events-organized: uint,
    total-attendees: uint,
    average-rating: uint,
    successful-events: uint
  })

;; Create a community event
(define-public (create-event 
                (title (string-ascii 100))
                (description (string-ascii 300))
                (ward (string-ascii 50))
                (event-type (string-ascii 20))
                (location (string-ascii 100))
                (start-time uint)
                (end-time uint)
                (max-capacity uint)
                (registration-deadline uint)
                (resource-id (optional uint)))
  (let ((caller tx-sender)
        (event-id (var-get next-event-id)))
    ;; Validate caller is registered resident (call to main Warda contract)
    (asserts! (is-some (contract-call? .Warda get-resident caller)) err-not-registered)
    ;; Validate times
    (asserts! (> start-time stacks-block-height) err-invalid-date)
    (asserts! (> end-time start-time) err-invalid-date)
    (asserts! (> registration-deadline stacks-block-height) err-invalid-date)
    (asserts! (< registration-deadline start-time) err-invalid-date)
    ;; Validate capacity
    (asserts! (> max-capacity u0) err-invalid-date)
    ;; Pay creation fee
    (try! (stx-transfer? event-creation-fee caller contract-owner))
    
    ;; Create event
    (map-set community-events event-id
      {
        title: title,
        description: description,
        organizer: caller,
        ward: ward,
        event-type: event-type,
        location: location,
        start-time: start-time,
        end-time: end-time,
        max-capacity: max-capacity,
        current-attendees: u0,
        registration-deadline: registration-deadline,
        status: STATUS-PLANNING,
        created-at: stacks-block-height,
        total-cost: event-creation-fee,
        resource-id: resource-id
      })
    
    ;; Update organizer stats
    (update-organizer-stats caller true false u0)
    
    ;; Update counters
    (var-set next-event-id (+ event-id u1))
    (var-set total-events (+ (var-get total-events) u1))
    
    (ok event-id)))

;; Register for an event (RSVP)
(define-public (register-for-event (event-id uint) (notes (string-ascii 200)))
  (let ((caller tx-sender))
    ;; Check if caller is registered resident
    (asserts! (is-some (contract-call? .Warda get-resident caller)) err-not-registered)
    
    (match (map-get? community-events event-id)
      event-data (begin
        ;; Check if event exists and registration is open
        (asserts! (is-eq (get status event-data) STATUS-PLANNING) err-registration-closed)
        (asserts! (<= stacks-block-height (get registration-deadline event-data)) err-registration-closed)
        (asserts! (< (get current-attendees event-data) (get max-capacity event-data)) err-event-full)
        (asserts! (is-none (map-get? event-registrations {event-id: event-id, attendee: caller})) err-already-registered)
        
        ;; Register attendee
        (map-set event-registrations {event-id: event-id, attendee: caller}
          {
            registered-at: stacks-block-height,
            confirmed: true,
            notes: notes
          })
        
        ;; Update event attendee count
        (map-set community-events event-id
          (merge event-data {
            current-attendees: (+ (get current-attendees event-data) u1)
          }))
        
        (ok true))
      err-event-not-found)))

;; Cancel event registration
(define-public (cancel-event-registration (event-id uint))
  (let ((caller tx-sender))
    (match (map-get? community-events event-id)
      event-data (begin
        ;; Check if registration exists
        (match (map-get? event-registrations {event-id: event-id, attendee: caller})
          registration-data (begin
            ;; Check if can still cancel (before event starts)
            (asserts! (> (get start-time event-data) stacks-block-height) err-event-already-started)
            
            ;; Remove registration
            (map-delete event-registrations {event-id: event-id, attendee: caller})
            
            ;; Update attendee count
            (map-set community-events event-id
              (merge event-data {
                current-attendees: (- (get current-attendees event-data) u1)
              }))
            
            (ok true))
          err-already-registered))
      err-event-not-found)))

;; Update event status (organizer or owner only)
(define-public (update-event-status (event-id uint) (new-status (string-ascii 20)))
  (let ((caller tx-sender))
    (match (map-get? community-events event-id)
      event-data (begin
        ;; Check authorization
        (asserts! (or (is-eq caller (get organizer event-data))
                     (is-eq caller contract-owner)) err-unauthorized-action)
        
        ;; Update status
        (map-set community-events event-id
          (merge event-data {status: new-status}))
        
        ;; Update organizer stats if completed successfully
        (if (is-eq new-status STATUS-COMPLETED)
          (update-organizer-stats (get organizer event-data) false true (get current-attendees event-data))
          true)
        
        (ok true))
      err-event-not-found)))

;; Submit event feedback
(define-public (submit-event-feedback (event-id uint) (rating uint) (feedback (string-ascii 300)))
  (let ((caller tx-sender))
    ;; Validate rating (1-5 scale)
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-date)
    
    (match (map-get? community-events event-id)
      event-data (begin
        ;; Check if attendee was registered for the event
        (asserts! (is-some (map-get? event-registrations {event-id: event-id, attendee: caller})) err-unauthorized-action)
        ;; Check if event is completed
        (asserts! (is-eq (get status event-data) STATUS-COMPLETED) err-event-not-found)
        
        ;; Submit feedback
        (map-set event-feedback {event-id: event-id, attendee: caller}
          {
            rating: rating,
            feedback: feedback,
            submitted-at: stacks-block-height
          })
        
        (ok true))
      err-event-not-found)))

;; Private function to update organizer statistics
(define-private (update-organizer-stats (organizer principal) (new-event bool) (completed bool) (attendees uint))
  (let ((current-stats (default-to 
                         {events-organized: u0, total-attendees: u0, average-rating: u0, successful-events: u0}
                         (map-get? organizer-stats organizer))))
    (map-set organizer-stats organizer
      {
        events-organized: (if new-event (+ (get events-organized current-stats) u1) (get events-organized current-stats)),
        total-attendees: (if completed (+ (get total-attendees current-stats) attendees) (get total-attendees current-stats)),
        average-rating: (get average-rating current-stats), ;; Simplified - would need complex calculation
        successful-events: (if completed (+ (get successful-events current-stats) u1) (get successful-events current-stats))
      })
    true))

;; Read-only functions
(define-read-only (get-event (event-id uint))
  (map-get? community-events event-id))

(define-read-only (get-event-registration (event-id uint) (attendee principal))
  (map-get? event-registrations {event-id: event-id, attendee: attendee}))

(define-read-only (get-event-feedback (event-id uint) (attendee principal))
  (map-get? event-feedback {event-id: event-id, attendee: attendee}))

(define-read-only (get-organizer-stats (organizer principal))
  (map-get? organizer-stats organizer))

(define-read-only (get-total-events)
  (var-get total-events))

(define-read-only (get-next-event-id)
  (var-get next-event-id))

(define-read-only (is-event-full (event-id uint))
  (match (map-get? community-events event-id)
    event-data (>= (get current-attendees event-data) (get max-capacity event-data))
    false))

(define-read-only (can-register-for-event (event-id uint) (resident principal))
  (match (map-get? community-events event-id)
    event-data (and
      (is-eq (get status event-data) STATUS-PLANNING)
      (<= stacks-block-height (get registration-deadline event-data))
      (< (get current-attendees event-data) (get max-capacity event-data))
      (is-none (map-get? event-registrations {event-id: event-id, attendee: resident}))
      (is-some (contract-call? .Warda get-resident resident)))
    false))

(define-read-only (get-event-attendee-count (event-id uint))
  (match (map-get? community-events event-id)
    event-data (some (get current-attendees event-data))
    none))

(define-read-only (get-events-by-organizer (organizer principal))
  ;; Simplified - returns list of event IDs, would need iteration in practice
  (list u1 u2 u3 u4 u5))

(define-read-only (get-upcoming-events)
  ;; Simplified - returns list of upcoming event IDs, would need filtering
  (list u1 u2 u3 u4 u5))
