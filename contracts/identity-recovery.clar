;; Self-Sovereign Identity Recovery System
;; Decentralized recovery through trusted guardian networks and threshold consensus

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u500))
(define-constant err-guardian-not-found (err u501))
(define-constant err-already-guardian (err u502))
(define-constant err-insufficient-guardians (err u503))
(define-constant err-recovery-not-found (err u504))
(define-constant err-recovery-expired (err u505))
(define-constant err-already-voted (err u506))
(define-constant err-threshold-not-met (err u507))
(define-constant err-recovery-executed (err u508))
(define-constant err-self-guardian (err u509))
(define-constant err-invalid-threshold (err u510))

(define-data-var next-recovery-id uint u1)
(define-data-var default-threshold uint u3) ;; Minimum guardians needed for recovery
(define-data-var recovery-window-blocks uint u4320) ;; 30 days recovery window
(define-data-var guardian-challenge-blocks uint u1440) ;; 10 days for guardian challenges
(define-data-var max-guardians-per-identity uint u7)

;; Guardian network for each identity
(define-map identity-guardians
  { identity: principal }
  {
    guardians: (list 7 principal),
    guardian-count: uint,
    threshold: uint,
    last-updated: uint,
    active: bool
  }
)

;; Guardian reputation and reliability tracking
(define-map guardian-reputation
  { guardian: principal }
  {
    total-recoveries: uint,
    successful-recoveries: uint,
    challenges-won: uint,
    challenges-lost: uint,
    reputation-score: uint,
    last-activity: uint,
    active: bool
  }
)

;; Recovery proposals initiated by lost identity owners
(define-map recovery-proposals
  { recovery-id: uint }
  {
    claimant: principal,
    target-identity: principal,
    new-address: principal,
    evidence-hash: (string-utf8 128),
    created-at: uint,
    expires-at: uint,
    status: (string-ascii 20),
    votes-count: uint,
    votes-threshold: uint,
    executed-at: (optional uint)
  }
)

;; Guardian votes on recovery proposals
(define-map recovery-votes
  { recovery-id: uint, guardian: principal }
  {
    vote: bool,
    vote-weight: uint,
    voted-at: uint,
    reasoning: (string-utf8 200)
  }
)

;; Challenge system for disputed recoveries
(define-map recovery-challenges
  { recovery-id: uint }
  {
    challenger: principal,
    challenge-reason: (string-utf8 300),
    challenged-at: uint,
    resolved: bool,
    resolution: (string-ascii 20)
  }
)

;; Emergency recovery override for extreme cases
(define-map emergency-overrides
  { identity: principal }
  {
    override-code: (string-ascii 32),
    created-by: principal,
    expires-at: uint,
    used: bool
  }
)

;; Read-only functions for data retrieval
(define-read-only (get-identity-guardians (identity principal))
  (default-to 
    { guardians: (list), guardian-count: u0, threshold: u0, last-updated: u0, active: false }
    (map-get? identity-guardians { identity: identity })))

(define-read-only (get-guardian-reputation (guardian principal))
  (default-to
    { total-recoveries: u0, successful-recoveries: u0, challenges-won: u0, 
      challenges-lost: u0, reputation-score: u100, last-activity: u0, active: false }
    (map-get? guardian-reputation { guardian: guardian })))

(define-read-only (get-recovery-proposal (recovery-id uint))
  (map-get? recovery-proposals { recovery-id: recovery-id }))

(define-read-only (get-recovery-vote (recovery-id uint) (guardian principal))
  (map-get? recovery-votes { recovery-id: recovery-id, guardian: guardian }))

(define-read-only (get-recovery-challenge (recovery-id uint))
  (map-get? recovery-challenges { recovery-id: recovery-id }))

;; Setup guardian network for an identity
(define-public (setup-guardian-network (guardians (list 7 principal)) (threshold uint))
  (let ((guardian-list-length (len guardians)))
    (asserts! (and (>= threshold u2) (<= threshold guardian-list-length)) err-invalid-threshold)
    (asserts! (and (>= guardian-list-length u3) (<= guardian-list-length (var-get max-guardians-per-identity))) err-insufficient-guardians)
    (asserts! (not (index-of guardians tx-sender)) err-self-guardian)
    
    (map-set identity-guardians
      { identity: tx-sender }
      {
        guardians: guardians,
        guardian-count: guardian-list-length,
        threshold: threshold,
        last-updated: stacks-block-height,
        active: true
      })
    
    ;; Initialize guardian reputation for new guardians
    (fold initialize-guardian-if-new guardians (ok true))))

;; Helper function to initialize guardian reputation
(define-private (initialize-guardian-if-new (guardian principal) (result (response bool uint)))
  (match result
    success
    (if (is-none (map-get? guardian-reputation { guardian: guardian }))
      (begin
        (map-set guardian-reputation
          { guardian: guardian }
          {
            total-recoveries: u0,
            successful-recoveries: u0,
            challenges-won: u0,
            challenges-lost: u0,
            reputation-score: u100,
            last-activity: stacks-block-height,
            active: true
          })
        (ok true))
      (ok true))
    error result))

;; Initiate identity recovery process
(define-public (initiate-recovery (target-identity principal) (new-address principal) (evidence-hash (string-utf8 128)))
  (let (
    (recovery-id (var-get next-recovery-id))
    (guardian-data (get-identity-guardians target-identity))
    (recovery-window (+ stacks-block-height (var-get recovery-window-blocks)))
  )
    (asserts! (not (is-eq tx-sender target-identity)) err-not-authorized)
    (asserts! (get active guardian-data) err-guardian-not-found)
    (asserts! (>= (get guardian-count guardian-data) u3) err-insufficient-guardians)
    
    (map-set recovery-proposals
      { recovery-id: recovery-id }
      {
        claimant: tx-sender,
        target-identity: target-identity,
        new-address: new-address,
        evidence-hash: evidence-hash,
        created-at: stacks-block-height,
        expires-at: recovery-window,
        status: "pending",
        votes-count: u0,
        votes-threshold: (get threshold guardian-data),
        executed-at: none
      })
    
    (var-set next-recovery-id (+ recovery-id u1))
    (ok recovery-id)))

;; Guardian votes on recovery proposal
(define-public (vote-on-recovery (recovery-id uint) (approve bool) (reasoning (string-utf8 200)))
  (let (
    (proposal (unwrap! (get-recovery-proposal recovery-id) err-recovery-not-found))
    (guardian-data (get-identity-guardians (get target-identity proposal)))
    (guardian-rep (get-guardian-reputation tx-sender))
    (existing-vote (get-recovery-vote recovery-id tx-sender))
  )
    (asserts! (index-of (get guardians guardian-data) tx-sender) err-not-authorized)
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (< stacks-block-height (get expires-at proposal)) err-recovery-expired)
    (asserts! (is-eq (get status proposal) "pending") err-recovery-executed)
    
    (let ((vote-weight (+ u1 (/ (get reputation-score guardian-rep) u100))))
      (map-set recovery-votes
        { recovery-id: recovery-id, guardian: tx-sender }
        {
          vote: approve,
          vote-weight: vote-weight,
          voted-at: stacks-block-height,
          reasoning: reasoning
        })
      
      (if approve
        (map-set recovery-proposals
          { recovery-id: recovery-id }
          (merge proposal { votes-count: (+ (get votes-count proposal) vote-weight) }))
        (ok true))
      
      ;; Update guardian activity
      (map-set guardian-reputation
        { guardian: tx-sender }
        (merge guardian-rep { 
          last-activity: stacks-block-height,
          total-recoveries: (+ (get total-recoveries guardian-rep) u1)
        }))
      
      (ok true))))

;; Execute recovery if threshold is met
(define-public (execute-recovery (recovery-id uint))
  (let (
    (proposal (unwrap! (get-recovery-proposal recovery-id) err-recovery-not-found))
    (challenge (get-recovery-challenge recovery-id))
  )
    (asserts! (is-eq (get status proposal) "pending") err-recovery-executed)
    (asserts! (>= (get votes-count proposal) (get votes-threshold proposal)) err-threshold-not-met)
    (asserts! (< stacks-block-height (get expires-at proposal)) err-recovery-expired)
    (asserts! (is-none challenge) (err u511)) ;; No active challenges
    
    ;; Mark proposal as executed
    (map-set recovery-proposals
      { recovery-id: recovery-id }
      (merge proposal {
        status: "executed",
        executed-at: (some stacks-block-height)
      }))
    
    ;; Transfer identity NFT to new address (simplified)
    (unwrap! (contract-call? .POI verify-address (get new-address proposal)) (err u512))
    
    ;; Update guardian reputations for successful recovery
    (unwrap! (update-guardian-reputations-for-recovery recovery-id true) (err u513))
    
    (ok true)))

;; Challenge a recovery proposal if suspicious
(define-public (challenge-recovery (recovery-id uint) (challenge-reason (string-utf8 300)))
  (let (
    (proposal (unwrap! (get-recovery-proposal recovery-id) err-recovery-not-found))
    (challenger-rep (get-guardian-reputation tx-sender))
  )
    (asserts! (is-eq (get status proposal) "pending") err-recovery-executed)
    (asserts! (< stacks-block-height (get expires-at proposal)) err-recovery-expired)
    (asserts! (>= (get reputation-score challenger_rep) u200) err-not-authorized)
    (asserts! (is-none (get-recovery-challenge recovery-id)) (err u514))
    
    (map-set recovery-challenges
      { recovery-id: recovery-id }
      {
        challenger: tx-sender,
        challenge-reason: challenge-reason,
        challenged-at: stacks-block-height,
        resolved: false,
        resolution: ""
      })
    
    ;; Pause recovery process during challenge period
    (map-set recovery-proposals
      { recovery-id: recovery-id }
      (merge proposal { status: "challenged" }))
    
    (ok true)))

;; Helper function to update guardian reputations after recovery
(define-private (update-guardian-reputations-for-recovery (recovery-id uint) (successful bool))
  (let ((proposal (unwrap! (get-recovery-proposal recovery-id) err-recovery-not-found)))
    (ok true))) ;; Simplified implementation

;; Add trusted guardian to network
(define-public (add-guardian (new-guardian principal))
  (let ((current-guardians (get-identity-guardians tx-sender)))
    (asserts! (< (get guardian-count current-guardians) (var-get max-guardians-per-identity)) err-insufficient-guardians)
    (asserts! (not (index-of (get guardians current-guardians) new-guardian)) err-already-guardian)
    (asserts! (not (is-eq new-guardian tx-sender)) err-self-guardian)
    
    (map-set identity-guardians
      { identity: tx-sender }
      {
        guardians: (unwrap! (as-max-len? (append (get guardians current-guardians) new-guardian) u7) err-insufficient-guardians),
        guardian-count: (+ (get guardian-count current-guardians) u1),
        threshold: (get threshold current-guardians),
        last-updated: stacks-block-height,
        active: true
      })
    
    (ok true)))

;; Remove guardian from network
(define-public (remove-guardian (guardian-to-remove principal))
  (let ((current-guardians (get-identity-guardians tx-sender)))
    (asserts! (index-of (get guardians current-guardians) guardian-to-remove) err-guardian-not-found)
    (asserts! (> (get guardian-count current-guardians) (get threshold current-guardians)) err-insufficient-guardians)
    
    (map-set identity-guardians
      { identity: tx-sender }
      {
        guardians: (filter-guardian (get guardians current-guardians) guardian-to-remove),
        guardian-count: (- (get guardian-count current-guardians) u1),
        threshold: (get threshold current-guardians),
        last-updated: stacks-block-height,
        active: true
      })
    
    (ok true)))

;; Helper function to filter out a guardian
(define-private (filter-guardian (guardians (list 7 principal)) (to-remove principal))
  (filter is-not-target-guardian guardians))

;; Helper function for filtering
(define-private (is-not-target-guardian (guardian principal))
  true) ;; Simplified implementation

;; Get recovery proposal status and progress
(define-read-only (get-recovery-status (recovery-id uint))
  (let ((proposal (unwrap! (get-recovery-proposal recovery-id) none)))
    (match proposal
      proposal-data
      (some {
        recovery-id: recovery-id,
        claimant: (get claimant proposal-data),
        target-identity: (get target-identity proposal-data),
        status: (get status proposal-data),
        votes-received: (get votes-count proposal-data),
        votes-needed: (get votes-threshold proposal-data),
        time-remaining: (if (> (get expires-at proposal-data) stacks-block-height)
                         (- (get expires-at proposal-data) stacks-block-height)
                         u0),
        challenged: (is-some (get-recovery-challenge recovery-id))
      })
      none)))

;; Administrative function to update recovery parameters
(define-public (update-recovery-parameters (threshold uint) (window-blocks uint) (challenge-blocks uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (and (>= threshold u2) (<= threshold u5)) err-invalid-threshold)
    (asserts! (> window-blocks u1440) (err u515)) ;; At least 10 days
    
    (var-set default-threshold threshold)
    (var-set recovery-window-blocks window-blocks)
    (var-set guardian-challenge-blocks challenge-blocks)
    (ok true)))

;; Get comprehensive recovery analytics for identity
(define-read-only (get-recovery-analytics (identity principal))
  (let ((guardian-data (get-identity-guardians identity)))
    {
      identity: identity,
      has-guardian-network: (get active guardian-data),
      guardian-count: (get guardian-count guardian-data),
      recovery-threshold: (get threshold guardian-data),
      network-strength: (calculate-network-strength identity),
      last-network-update: (get last-updated guardian-data)
    }))

;; Calculate guardian network strength
(define-read-only (calculate-network-strength (identity principal))
  (let ((guardian-data (get-identity-guardians identity)))
    (if (get active guardian-data)
      (/ (* (get guardian-count guardian-data) u100) (var-get max-guardians-per-identity))
      u0)))