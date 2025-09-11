;; Professional Skills Certification System
;; Allows authorized organizations to issue verifiable skill certifications

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_NOT_CERTIFIED_AUTHORITY (err u201))
(define-constant ERR_CERTIFICATION_NOT_FOUND (err u202))
(define-constant ERR_INVALID_SKILL_NAME (err u203))
(define-constant ERR_INVALID_LEVEL (err u204))
(define-constant ERR_CERTIFICATION_EXPIRED (err u205))
(define-constant ERR_AUTHORITY_NOT_FOUND (err u206))
(define-constant ERR_AUTHORITY_EXISTS (err u207))
(define-constant ERR_INVALID_DURATION (err u208))

;; Data variables
(define-data-var next-certification-id uint u1)
(define-data-var next-authority-id uint u1)
(define-data-var contract-paused bool false)

;; Certification authorities (universities, training centers, etc.)
(define-map certification-authorities
  uint ;; authority-id
  {
    name: (string-ascii 100),
    address: principal,
    domain: (string-ascii 50), ;; e.g., "programming", "design", "finance"
    accreditation-level: uint, ;; 1-5, higher = more prestigious
    is-active: bool,
    certified-at: uint,
    total-certifications-issued: uint
  }
)

;; Authority lookup by address
(define-map authority-addresses
  principal
  uint ;; authority-id
)

;; Skill certifications
(define-map skill-certifications
  uint ;; certification-id
  {
    authority-id: uint,
    recipient: principal,
    skill-name: (string-ascii 50),
    skill-level: uint, ;; 1-5 (Beginner to Expert)
    certification-type: (string-ascii 30), ;; "course", "exam", "project", "experience"
    credential-hash: (string-ascii 64), ;; IPFS hash or similar
    issued-at: uint,
    expires-at: (optional uint), ;; Some certifications may not expire
    is-verified: bool,
    verification-score: uint ;; Based on authority reputation
  }
)

;; User certifications lookup
(define-map user-certifications
  principal
  (list 200 uint) ;; certification-ids
)

;; Skill-based certification lookup
(define-map skill-certifications-index
  (string-ascii 50) ;; skill-name
  (list 1000 uint) ;; certification-ids
)

;; Authority certifications
(define-map authority-certifications
  uint ;; authority-id
  (list 500 uint) ;; certification-ids
)

;; Read-only functions
(define-read-only (get-certification (certification-id uint))
  (map-get? skill-certifications certification-id)
)

(define-read-only (get-authority (authority-id uint))
  (map-get? certification-authorities authority-id)
)

(define-read-only (get-authority-by-address (address principal))
  (match (map-get? authority-addresses address)
    auth-id (map-get? certification-authorities auth-id)
    none
  )
)

(define-read-only (get-user-certifications (user principal))
  (default-to (list) (map-get? user-certifications user))
)

(define-read-only (get-skill-certifications (skill-name (string-ascii 50)))
  (default-to (list) (map-get? skill-certifications-index skill-name))
)

(define-read-only (is-certification-valid (certification-id uint))
  (match (map-get? skill-certifications certification-id)
    cert (and 
      (get is-verified cert)
      (match (get expires-at cert)
        expiry (< stacks-block-height expiry)
        true ;; No expiry means it's always valid
      )
    )
    false
  )
)

;; Public functions - Authority Management
(define-public (register-certification-authority (name (string-ascii 100)) (domain (string-ascii 50)) (accreditation-level uint))
  (let
    (
      (authority-id (var-get next-authority-id))
      (current-block stacks-block-height)
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> (len name) u0) ERR_INVALID_SKILL_NAME)
    (asserts! (> (len domain) u0) ERR_INVALID_SKILL_NAME)
    (asserts! (and (>= accreditation-level u1) (<= accreditation-level u5)) ERR_INVALID_LEVEL)
    (asserts! (is-none (map-get? authority-addresses tx-sender)) ERR_AUTHORITY_EXISTS)
    
    (map-set certification-authorities authority-id
      {
        name: name,
        address: tx-sender,
        domain: domain,
        accreditation-level: accreditation-level,
        is-active: true,
        certified-at: current-block,
        total-certifications-issued: u0
      }
    )
    
    (map-set authority-addresses tx-sender authority-id)
    (var-set next-authority-id (+ authority-id u1))
    (ok authority-id)
  )
)

(define-public (approve-certification-authority (authority-address principal))
  (let
    (
      (authority-id (unwrap! (map-get? authority-addresses authority-address) ERR_AUTHORITY_NOT_FOUND))
      (authority (unwrap! (map-get? certification-authorities authority-id) ERR_AUTHORITY_NOT_FOUND))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    (map-set certification-authorities authority-id
      (merge authority { is-active: true })
    )
    (ok true)
  )
)

;; Public functions - Certification Issuance
(define-public (issue-skill-certification 
    (recipient principal) 
    (skill-name (string-ascii 50)) 
    (skill-level uint) 
    (certification-type (string-ascii 30))
    (credential-hash (string-ascii 64))
    (duration-blocks (optional uint)))
  (let
    (
      (authority-id (unwrap! (map-get? authority-addresses tx-sender) ERR_NOT_CERTIFIED_AUTHORITY))
      (authority (unwrap! (map-get? certification-authorities authority-id) ERR_AUTHORITY_NOT_FOUND))
      (certification-id (var-get next-certification-id))
      (current-block stacks-block-height)
      (expires-at (match duration-blocks
        duration (some (+ current-block duration))
        none
      ))
      (verification-score (* (get accreditation-level authority) u20)) ;; Max 100 for level 5 authorities
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active authority) ERR_NOT_AUTHORIZED)
    (asserts! (> (len skill-name) u0) ERR_INVALID_SKILL_NAME)
    (asserts! (and (>= skill-level u1) (<= skill-level u5)) ERR_INVALID_LEVEL)
    (asserts! (> (len certification-type) u0) ERR_INVALID_SKILL_NAME)
    (asserts! (> (len credential-hash) u0) ERR_INVALID_SKILL_NAME)
    (asserts! (match duration-blocks
      duration (> duration u0)
      true
    ) ERR_INVALID_DURATION)
    
    (map-set skill-certifications certification-id
      {
        authority-id: authority-id,
        recipient: recipient,
        skill-name: skill-name,
        skill-level: skill-level,
        certification-type: certification-type,
        credential-hash: credential-hash,
        issued-at: current-block,
        expires-at: expires-at,
        is-verified: true,
        verification-score: verification-score
      }
    )
    
    ;; Update user certifications
    (map-set user-certifications recipient
      (unwrap! (as-max-len? 
        (append (get-user-certifications recipient) certification-id) 
        u200) ERR_NOT_AUTHORIZED)
    )
    
    ;; Update skill index
    (map-set skill-certifications-index skill-name
      (unwrap! (as-max-len? 
        (append (get-skill-certifications skill-name) certification-id) 
        u1000) ERR_NOT_AUTHORIZED)
    )
    
    ;; Update authority certifications
    (map-set authority-certifications authority-id
      (unwrap! (as-max-len? 
        (append (default-to (list) (map-get? authority-certifications authority-id)) certification-id) 
        u500) ERR_NOT_AUTHORIZED)
    )
    
    ;; Update authority stats
    (map-set certification-authorities authority-id
      (merge authority { 
        total-certifications-issued: (+ (get total-certifications-issued authority) u1) 
      })
    )
    
    (var-set next-certification-id (+ certification-id u1))
    (ok certification-id)
  )
)

(define-public (revoke-certification (certification-id uint))
  (let
    (
      (cert (unwrap! (map-get? skill-certifications certification-id) ERR_CERTIFICATION_NOT_FOUND))
      (authority-id (unwrap! (map-get? authority-addresses tx-sender) ERR_NOT_CERTIFIED_AUTHORITY))
    )
    (asserts! (not (var-get contract-paused)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get authority-id cert) authority-id) ERR_NOT_AUTHORIZED)
    
    (map-set skill-certifications certification-id
      (merge cert { is-verified: false })
    )
    (ok true)
  )
)

;; Admin functions
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set contract-paused false)
    (ok true)
  )
)

;; Helper functions
(define-read-only (get-user-skill-certifications (user principal) (skill-name (string-ascii 50)))
  (let
    (
      (user-certs (get-user-certifications user))
    )
    (filter (is-cert-for-skill skill-name) user-certs)
  )
)

(define-private (is-cert-for-skill (target-skill (string-ascii 50)) (cert-id uint))
  (match (map-get? skill-certifications cert-id)
    cert (is-eq (get skill-name cert) target-skill)
    false
  )
)

(define-read-only (calculate-skill-credibility-score (user principal) (skill-name (string-ascii 50)))
  (let
    (
      (skill-certs (get-user-skill-certifications user skill-name))
      (total-score (fold sum-verification-scores skill-certs u0))
      (cert-count (len skill-certs))
    )
    (if (> cert-count u0) (/ total-score cert-count) u0)
  )
)

(define-private (sum-verification-scores (cert-id uint) (acc uint))
  (match (map-get? skill-certifications cert-id)
    cert (if (is-certification-valid cert-id) 
           (+ acc (get verification-score cert)) 
           acc)
    acc
  )
)

(define-read-only (get-contract-stats)
  {
    total-certifications: (- (var-get next-certification-id) u1),
    total-authorities: (- (var-get next-authority-id) u1),
    contract-paused: (var-get contract-paused)
  }
)
