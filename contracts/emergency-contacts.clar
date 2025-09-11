;; title: Emergency Contact Network
;; version: 1.0.0
;; summary: Emergency contact management and automatic notification system
;; description: Allows users to register emergency contacts and automatically notify them during emergencies

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_CONTACT_NOT_FOUND (err u201))
(define-constant ERR_MAX_CONTACTS_REACHED (err u202))
(define-constant ERR_INVALID_RELATIONSHIP (err u203))
(define-constant ERR_DUPLICATE_CONTACT (err u204))
(define-constant ERR_NOTIFICATION_FAILED (err u205))
(define-constant ERR_INVALID_PRIORITY (err u206))

(define-constant MAX_CONTACTS_PER_USER u5)
(define-constant NOTIFICATION_FEE u50000)

;; relationship types
(define-constant RELATIONSHIP_FAMILY u0)
(define-constant RELATIONSHIP_FRIEND u1)
(define-constant RELATIONSHIP_MEDICAL u2)
(define-constant RELATIONSHIP_WORKPLACE u3)
(define-constant RELATIONSHIP_NEIGHBOR u4)

;; priority levels
(define-constant PRIORITY_HIGH u0)
(define-constant PRIORITY_MEDIUM u1)
(define-constant PRIORITY_LOW u2)

;; data vars
(define-data-var contact-counter uint u0)
(define-data-var notification-counter uint u0)
(define-data-var total-notifications uint u0)

;; data maps
(define-map user-contacts
  { user: principal, contact-id: uint }
  {
    contact-address: principal,
    contact-name: (string-ascii 50),
    relationship-type: uint,
    priority-level: uint,
    phone-hash: (optional (buff 32)),
    email-hash: (optional (buff 32)),
    active: bool,
    created-at: uint,
    last-notified: (optional uint)
  }
)

(define-map user-contact-count
  { user: principal }
  { total-contacts: uint }
)

(define-map emergency-notifications
  { notification-id: uint }
  {
    emergency-id: uint,
    caller: principal,
    contact-address: principal,
    relationship-type: uint,
    priority-level: uint,
    notification-time: uint,
    message-hash: (buff 32),
    acknowledged: bool,
    acknowledged-at: (optional uint)
  }
)

(define-map contact-preferences
  { user: principal }
  {
    auto-notify-enabled: bool,
    max-priority-level: uint,
    notification-fee-balance: uint,
    last-updated: uint
  }
)

(define-map notification-stats
  { user: principal }
  {
    total-sent: uint,
    total-received: uint,
    total-acknowledged: uint,
    avg-response-time: uint
  }
)

;; private functions (must be defined before public functions that use them)
(define-private (find-contact-by-address (user principal) (target-address principal) (current-id uint) (max-id uint))
  (if (> current-id max-id)
    none
    (match (map-get? user-contacts { user: user, contact-id: current-id })
      contact (if (is-eq (get contact-address contact) target-address) 
                 (some current-id)
                 (find-contact-by-address user target-address (+ current-id u1) max-id))
      (find-contact-by-address user target-address (+ current-id u1) max-id)
    )
  )
)

(define-private (send-notification-loop (caller principal) (emergency-id uint) (message-hash (buff 32)) (max-priority uint) (current-id uint) (max-id uint) (sent-count uint))
  (if (> current-id max-id)
    sent-count
    (let
      (
        (contact-data (map-get? user-contacts { user: caller, contact-id: current-id }))
      )
      (match contact-data
        contact (if (and (get active contact) (<= (get priority-level contact) max-priority))
                   (let
                     (
                       (notification-id (+ (var-get notification-counter) u1))
                       (current-block stacks-block-height)
                     )
                     (map-set emergency-notifications
                       { notification-id: notification-id }
                       {
                         emergency-id: emergency-id,
                         caller: caller,
                         contact-address: (get contact-address contact),
                         relationship-type: (get relationship-type contact),
                         priority-level: (get priority-level contact),
                         notification-time: current-block,
                         message-hash: message-hash,
                         acknowledged: false,
                         acknowledged-at: none
                       }
                     )
                     (var-set notification-counter notification-id)
                     (send-notification-loop caller emergency-id message-hash max-priority (+ current-id u1) max-id (+ sent-count u1))
                   )
                   (send-notification-loop caller emergency-id message-hash max-priority (+ current-id u1) max-id sent-count))
        (send-notification-loop caller emergency-id message-hash max-priority (+ current-id u1) max-id sent-count)
      )
    )
  )
)

(define-private (send-notifications-to-contacts (caller principal) (emergency-id uint) (message-hash (buff 32)) (max-priority uint))
  (let
    (
      (contact-count (get total-contacts (default-to { total-contacts: u0 } (map-get? user-contact-count { user: caller }))))
    )
    (send-notification-loop caller emergency-id message-hash max-priority u1 contact-count u0)
  )
)

(define-private (update-notification-stats (user principal) (notifications-sent uint))
  (let
    (
      (current-stats (default-to { total-sent: u0, total-received: u0, total-acknowledged: u0, avg-response-time: u0 } 
                                 (map-get? notification-stats { user: user })))
    )
    (map-set notification-stats
      { user: user }
      (merge current-stats { total-sent: (+ (get total-sent current-stats) notifications-sent) })
    )
    true
  )
)

(define-private (update-received-notification-stats (contact-address principal))
  (let
    (
      (current-stats (default-to { total-sent: u0, total-received: u0, total-acknowledged: u0, avg-response-time: u0 } 
                                 (map-get? notification-stats { user: contact-address })))
    )
    (map-set notification-stats
      { user: contact-address }
      (merge current-stats { 
        total-received: (+ (get total-received current-stats) u1),
        total-acknowledged: (+ (get total-acknowledged current-stats) u1)
      })
    )
    true
  )
)

;; public functions
(define-public (register-contact (contact-address principal) (contact-name (string-ascii 50)) (relationship-type uint) (priority-level uint) (phone-hash (optional (buff 32))) (email-hash (optional (buff 32))))
  (let
    (
      (current-count (get total-contacts (default-to { total-contacts: u0 } (map-get? user-contact-count { user: tx-sender }))))
      (contact-id (+ (var-get contact-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (< current-count MAX_CONTACTS_PER_USER) ERR_MAX_CONTACTS_REACHED)
    (asserts! (<= relationship-type RELATIONSHIP_NEIGHBOR) ERR_INVALID_RELATIONSHIP)
    (asserts! (<= priority-level PRIORITY_LOW) ERR_INVALID_PRIORITY)
    (asserts! (not (is-eq tx-sender contact-address)) ERR_DUPLICATE_CONTACT)
    
    ;; Check if contact already exists
    (asserts! (is-none (find-contact-by-address tx-sender contact-address u1 current-count)) ERR_DUPLICATE_CONTACT)
    
    (map-set user-contacts
      { user: tx-sender, contact-id: contact-id }
      {
        contact-address: contact-address,
        contact-name: contact-name,
        relationship-type: relationship-type,
        priority-level: priority-level,
        phone-hash: phone-hash,
        email-hash: email-hash,
        active: true,
        created-at: current-block,
        last-notified: none
      }
    )
    
    (map-set user-contact-count
      { user: tx-sender }
      { total-contacts: (+ current-count u1) }
    )
    
    (var-set contact-counter contact-id)
    (ok contact-id)
  )
)

(define-public (update-contact-preferences (auto-notify-enabled bool) (max-priority-level uint))
  (let
    (
      (current-block stacks-block-height)
      (current-prefs (default-to { auto-notify-enabled: true, max-priority-level: PRIORITY_LOW, notification-fee-balance: u0, last-updated: u0 } 
                                 (map-get? contact-preferences { user: tx-sender })))
    )
    (asserts! (<= max-priority-level PRIORITY_LOW) ERR_INVALID_PRIORITY)
    
    (map-set contact-preferences
      { user: tx-sender }
      (merge current-prefs {
        auto-notify-enabled: auto-notify-enabled,
        max-priority-level: max-priority-level,
        last-updated: current-block
      })
    )
    (ok true)
  )
)

(define-public (notify-emergency-contacts (emergency-id uint) (caller principal) (message-hash (buff 32)))
  (let
    (
      (user-prefs (default-to { auto-notify-enabled: true, max-priority-level: PRIORITY_LOW, notification-fee-balance: u0, last-updated: u0 } 
                              (map-get? contact-preferences { user: caller })))
      (current-block stacks-block-height)
    )
    (asserts! (or (is-eq tx-sender caller) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (asserts! (get auto-notify-enabled user-prefs) ERR_NOTIFICATION_FAILED)
    
    ;; Send notifications to all eligible contacts
    (let
      (
        (notifications-sent (send-notifications-to-contacts caller emergency-id message-hash (get max-priority-level user-prefs)))
      )
      (if (> notifications-sent u0)
        (begin
          (var-set total-notifications (+ (var-get total-notifications) notifications-sent))
          (update-notification-stats caller notifications-sent)
          (ok notifications-sent)
        )
        (err ERR_NOTIFICATION_FAILED)
      )
    )
  )
)

(define-public (acknowledge-notification (notification-id uint))
  (let
    (
      (notification (unwrap! (map-get? emergency-notifications { notification-id: notification-id }) ERR_CONTACT_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get contact-address notification)) ERR_UNAUTHORIZED)
    (asserts! (not (get acknowledged notification)) ERR_NOTIFICATION_FAILED)
    
    (map-set emergency-notifications
      { notification-id: notification-id }
      (merge notification {
        acknowledged: true,
        acknowledged-at: (some current-block)
      })
    )
    
    (update-received-notification-stats (get contact-address notification))
    (ok true)
  )
)

(define-public (deactivate-contact (contact-id uint))
  (let
    (
      (contact (unwrap! (map-get? user-contacts { user: tx-sender, contact-id: contact-id }) ERR_CONTACT_NOT_FOUND))
    )
    (map-set user-contacts
      { user: tx-sender, contact-id: contact-id }
      (merge contact { active: false })
    )
    (ok true)
  )
)

;; read-only functions
(define-read-only (get-user-contact (user principal) (contact-id uint))
  (map-get? user-contacts { user: user, contact-id: contact-id })
)

(define-read-only (get-contact-count (user principal))
  (get total-contacts (default-to { total-contacts: u0 } (map-get? user-contact-count { user: user })))
)

(define-read-only (get-contact-preferences (user principal))
  (map-get? contact-preferences { user: user })
)

(define-read-only (get-notification (notification-id uint))
  (map-get? emergency-notifications { notification-id: notification-id })
)

(define-read-only (get-notification-stats (user principal))
  (map-get? notification-stats { user: user })
)

(define-read-only (get-system-stats)
  {
    total-contacts: (var-get contact-counter),
    total-notifications: (var-get total-notifications),
    notification-counter: (var-get notification-counter)
  }
)
