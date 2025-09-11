(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_REPORT (err u101))
(define-constant ERR_ALREADY_VOTED (err u102))
(define-constant ERR_REPORT_NOT_FOUND (err u103))
(define-constant ERR_VOTING_CLOSED (err u104))
(define-constant ERR_INSUFFICIENT_STAKE (err u105))
(define-constant ERR_ALREADY_CLAIMED (err u106))
(define-constant ERR_ZONE_NOT_FOUND (err u107))
(define-constant ERR_ALERT_ALREADY_SET (err u108))
(define-constant ERR_INVALID_COORDINATES (err u109))
(define-constant ERR_INVALID_RISK_LEVEL (err u110))
(define-constant ERR_SOURCE_NOT_FOUND (err u111))
(define-constant ERR_SOURCE_ALREADY_REGISTERED (err u112))
(define-constant ERR_INVALID_EMISSION_DATA (err u113))
(define-constant ERR_COMPLIANCE_PERIOD_ACTIVE (err u114))
(define-constant ERR_INSUFFICIENT_DEPOSIT (err u115))

(define-constant MIN_STAKE u1000000)
(define-constant VOTING_PERIOD u144)
(define-constant CONSENSUS_THRESHOLD u66)
(define-constant REWARD_AMOUNT u500000)
(define-constant RISK_CALCULATION_WINDOW u1008)
(define-constant HIGH_RISK_THRESHOLD u70)
(define-constant CRITICAL_RISK_THRESHOLD u90)
(define-constant ZONE_ALERT_REWARD u250000)
(define-constant SOURCE_REGISTRATION_DEPOSIT u2000000)
(define-constant COMPLIANCE_PERIOD u720)
(define-constant EMISSION_PENALTY_RATE u100000)
(define-constant COMPLIANCE_REWARD u750000)
(define-constant COMMUNITY_REPORT_REWARD u150000)

;; Badge system constants
(define-constant BADGE_ROOKIE_REPORTER "rookie-reporter")
(define-constant BADGE_VETERAN_REPORTER "veteran-reporter")
(define-constant BADGE_TRUSTED_VALIDATOR "trusted-validator")
(define-constant BADGE_COMMUNITY_CHAMPION "community-champion")
(define-constant BADGE_ZONE_EXPLORER "zone-explorer")
(define-constant BADGE_SOURCE_GUARDIAN "source-guardian")

;; Badge thresholds
(define-constant ROOKIE_THRESHOLD u5)
(define-constant VETERAN_THRESHOLD u50)
(define-constant VALIDATOR_THRESHOLD u20)
(define-constant CHAMPION_REP_THRESHOLD u500)
(define-constant EXPLORER_ZONE_THRESHOLD u10)
(define-constant GUARDIAN_SOURCE_THRESHOLD u5)

(define-data-var next-report-id uint u1)
(define-data-var next-source-id uint u1)
(define-data-var next-zone-id uint u1)
(define-data-var total-reports uint u0)
(define-data-var total-validated-reports uint u0)

(define-map pollution-reports
  uint
  {
    reporter: principal,
    location: (string-ascii 100),
    pollution-type: (string-ascii 50),
    severity: uint,
    description: (string-ascii 500),
    timestamp: uint,
    voting-end: uint,
    total-votes: uint,
    approve-votes: uint,
    reject-votes: uint,
    validated: bool,
    reward-claimed: bool
  }
)

(define-map user-stakes
  principal
  uint
)

(define-map report-votes
  { report-id: uint, voter: principal }
  { vote: bool, stake: uint }
)

(define-map user-reputation
  principal
  { score: uint, total-reports: uint, validated-reports: uint }
)

(define-map validator-rewards
  { report-id: uint, validator: principal }
  { amount: uint, claimed: bool }
)

(define-map pollution-zones
  uint
  {
    zone-name: (string-ascii 100),
    latitude: int,
    longitude: int,
    radius: uint,
    creator: principal,
    creation-time: uint,
    total-reports: uint,
    validated-reports: uint,
    risk-level: uint,
    last-updated: uint,
    alert-active: bool
  }
)

(define-map zone-reports
  { zone-id: uint, report-id: uint }
  bool
)

(define-map zone-alerts
  { zone-id: uint, alert-type: (string-ascii 20) }
  {
    creator: principal,
    creation-time: uint,
    expiry-time: uint,
    active: bool,
    reward-claimed: bool
  }
)

(define-map zone-subscribers
  { zone-id: uint, subscriber: principal }
  { subscribed-at: uint, notifications-enabled: bool }
)

(define-map zone-risk-history
  { zone-id: uint, block-height: uint }
  { risk-level: uint, report-count: uint }
)

;; Badge system map
(define-map user-badges
  principal
  { badges: (list 20 (string-ascii 20)), zones-created: uint, correct-votes: uint }
)

(define-public (stake-tokens (amount uint))
  (let ((current-stake (default-to u0 (map-get? user-stakes tx-sender))))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set user-stakes tx-sender (+ current-stake amount))
    (ok true)
  )
)

(define-public (unstake-tokens (amount uint))
  (let ((current-stake (default-to u0 (map-get? user-stakes tx-sender))))
    (asserts! (>= current-stake amount) ERR_INSUFFICIENT_STAKE)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set user-stakes tx-sender (- current-stake amount))
    (ok true)
  )
)

(define-public (submit-report 
  (location (string-ascii 100))
  (pollution-type (string-ascii 50))
  (severity uint)
  (description (string-ascii 500))
)
  (let (
    (report-id (var-get next-report-id))
    (current-block stacks-block-height)
    (user-stake (default-to u0 (map-get? user-stakes tx-sender)))
  )
    (asserts! (>= user-stake MIN_STAKE) ERR_INSUFFICIENT_STAKE)
    (asserts! (and (> severity u0) (<= severity u10)) ERR_INVALID_REPORT)
    
    (map-set pollution-reports report-id {
      reporter: tx-sender,
      location: location,
      pollution-type: pollution-type,
      severity: severity,
      description: description,
      timestamp: current-block,
      voting-end: (+ current-block VOTING_PERIOD),
      total-votes: u0,
      approve-votes: u0,
      reject-votes: u0,
      validated: false,
      reward-claimed: false
    })
    
    (var-set next-report-id (+ report-id u1))
    (var-set total-reports (+ (var-get total-reports) u1))
    
    (update-user-reputation tx-sender u0)
    (ok report-id)
  )
)

(define-public (vote-on-report (report-id uint) (approve bool))
  (let (
    (report (unwrap! (map-get? pollution-reports report-id) ERR_REPORT_NOT_FOUND))
    (voter-stake (default-to u0 (map-get? user-stakes tx-sender)))
    (current-block stacks-block-height)
    (existing-vote (map-get? report-votes { report-id: report-id, voter: tx-sender }))
  )
    (asserts! (>= voter-stake MIN_STAKE) ERR_INSUFFICIENT_STAKE)
    (asserts! (< current-block (get voting-end report)) ERR_VOTING_CLOSED)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    
    (map-set report-votes 
      { report-id: report-id, voter: tx-sender }
      { vote: approve, stake: voter-stake }
    )
    
    (let (
      (new-total-votes (+ (get total-votes report) u1))
      (new-approve-votes (if approve (+ (get approve-votes report) u1) (get approve-votes report)))
      (new-reject-votes (if approve (get reject-votes report) (+ (get reject-votes report) u1)))
    )
      (map-set pollution-reports report-id (merge report {
        total-votes: new-total-votes,
        approve-votes: new-approve-votes,
        reject-votes: new-reject-votes
      }))
    )
    
    (ok true)
  )
)

(define-public (finalize-report (report-id uint))
  (let (
    (report (unwrap! (map-get? pollution-reports report-id) ERR_REPORT_NOT_FOUND))
    (current-block stacks-block-height)
  )
    (asserts! (>= current-block (get voting-end report)) ERR_VOTING_CLOSED)
    (asserts! (not (get validated report)) ERR_ALREADY_CLAIMED)
    
    (let (
      (total-votes (get total-votes report))
      (approve-votes (get approve-votes report))
      (approval-percentage (if (> total-votes u0) (* (/ approve-votes total-votes) u100) u0))
      (is-validated (>= approval-percentage CONSENSUS_THRESHOLD))
    )
      (map-set pollution-reports report-id (merge report { validated: is-validated }))
      
      (if is-validated
        (begin
          (var-set total-validated-reports (+ (var-get total-validated-reports) u1))
          (update-user-reputation (get reporter report) u1)
        )
        (update-user-reputation (get reporter report) u0)
      )
      
      (ok is-validated)
    )
  )
)

(define-public (claim-validator-reward (report-id uint))
  (let (
    (report (unwrap! (map-get? pollution-reports report-id) ERR_REPORT_NOT_FOUND))
    (vote-data (unwrap! (map-get? report-votes { report-id: report-id, voter: tx-sender }) ERR_NOT_AUTHORIZED))
    (reward-key { report-id: report-id, validator: tx-sender })
    (existing-reward (map-get? validator-rewards reward-key))
  )
    (asserts! (get validated report) ERR_INVALID_REPORT)
    (asserts! (is-none existing-reward) ERR_ALREADY_CLAIMED)
    
    (let (
      (vote-was-correct (is-eq (get vote vote-data) (get validated report)))
      (reward-amount (if vote-was-correct REWARD_AMOUNT u0))
    )
      (if (> reward-amount u0)
        (begin
          (try! (as-contract (stx-transfer? reward-amount tx-sender tx-sender)))
          (map-set validator-rewards reward-key { amount: reward-amount, claimed: true })
          (increment-correct-votes tx-sender)
          (check-and-award-badges tx-sender)
          (ok reward-amount)
        )
        (ok u0)
      )
    )
  )
)

(define-public (claim-reporter-reward (report-id uint))
  (let (
    (report (unwrap! (map-get? pollution-reports report-id) ERR_REPORT_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get reporter report)) ERR_NOT_AUTHORIZED)
    (asserts! (get validated report) ERR_INVALID_REPORT)
    (asserts! (not (get reward-claimed report)) ERR_ALREADY_CLAIMED)
    
    (try! (as-contract (stx-transfer? (* REWARD_AMOUNT u2) tx-sender tx-sender)))
    (map-set pollution-reports report-id (merge report { reward-claimed: true }))
    (check-and-award-badges tx-sender)
    (ok (* REWARD_AMOUNT u2))
  )
)

(define-private (update-user-reputation (user principal) (validated-increment uint))
  (let (
    (current-rep (default-to { score: u0, total-reports: u0, validated-reports: u0 } 
                             (map-get? user-reputation user)))
  )
    (map-set user-reputation user {
      score: (+ (get score current-rep) (if (> validated-increment u0) u10 u1)),
      total-reports: (+ (get total-reports current-rep) u1),
      validated-reports: (+ (get validated-reports current-rep) validated-increment)
    })
  )
)

(define-read-only (get-report (report-id uint))
  (map-get? pollution-reports report-id)
)

(define-read-only (get-user-stake (user principal))
  (default-to u0 (map-get? user-stakes user))
)

(define-read-only (get-user-reputation (user principal))
  (default-to { score: u0, total-reports: u0, validated-reports: u0 } 
              (map-get? user-reputation user))
)

(define-read-only (get-vote (report-id uint) (voter principal))
  (map-get? report-votes { report-id: report-id, voter: voter })
)

(define-read-only (get-contract-stats)
  {
    total-reports: (var-get total-reports),
    total-validated-reports: (var-get total-validated-reports),
    next-report-id: (var-get next-report-id)
  }
)

(define-read-only (can-vote (report-id uint) (voter principal))
  (let (
    (report (map-get? pollution-reports report-id))
    (voter-stake (default-to u0 (map-get? user-stakes voter)))
    (current-block stacks-block-height)
    (existing-vote (map-get? report-votes { report-id: report-id, voter: voter }))
  )
    (match report
      some-report (and 
        (>= voter-stake MIN_STAKE)
        (< current-block (get voting-end some-report))
        (is-none existing-vote)
      )
      false
    )
  )
)

(define-public (create-pollution-zone 
  (zone-name (string-ascii 100))
  (latitude int)
  (longitude int)
  (radius uint)
)
  (let (
    (zone-id (var-get next-zone-id))
    (current-block stacks-block-height)
  )
    (asserts! (and (>= latitude -90000000) (<= latitude 90000000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= longitude -180000000) (<= longitude 180000000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= radius u100) (<= radius u50000)) ERR_INVALID_COORDINATES)
    
    (map-set pollution-zones zone-id {
      zone-name: zone-name,
      latitude: latitude,
      longitude: longitude,
      radius: radius,
      creator: tx-sender,
      creation-time: current-block,
      total-reports: u0,
      validated-reports: u0,
      risk-level: u0,
      last-updated: current-block,
      alert-active: false
    })
    
    (var-set next-zone-id (+ zone-id u1))
    (increment-zones-created tx-sender)
    (check-and-award-badges tx-sender)
    (ok zone-id)
  )
)

(define-public (subscribe-to-zone (zone-id uint))
  (let (
    (zone (unwrap! (map-get? pollution-zones zone-id) ERR_ZONE_NOT_FOUND))
    (current-block stacks-block-height)
  )
    (map-set zone-subscribers 
      { zone-id: zone-id, subscriber: tx-sender }
      { subscribed-at: current-block, notifications-enabled: true }
    )
    (ok true)
  )
)

(define-public (unsubscribe-from-zone (zone-id uint))
  (let (
    (zone (unwrap! (map-get? pollution-zones zone-id) ERR_ZONE_NOT_FOUND))
  )
    (map-delete zone-subscribers { zone-id: zone-id, subscriber: tx-sender })
    (ok true)
  )
)

(define-public (link-report-to-zone (report-id uint) (zone-id uint))
  (let (
    (report (unwrap! (map-get? pollution-reports report-id) ERR_REPORT_NOT_FOUND))
    (zone (unwrap! (map-get? pollution-zones zone-id) ERR_ZONE_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get reporter report)) ERR_NOT_AUTHORIZED)
    
    (map-set zone-reports { zone-id: zone-id, report-id: report-id } true)
    (update-zone-statistics zone-id report-id)
    (ok true)
  )
)

(define-public (create-zone-alert 
  (zone-id uint)
  (alert-type (string-ascii 20))
  (duration uint)
)
  (let (
    (zone (unwrap! (map-get? pollution-zones zone-id) ERR_ZONE_NOT_FOUND))
    (current-block stacks-block-height)
    (alert-key { zone-id: zone-id, alert-type: alert-type })
    (existing-alert (map-get? zone-alerts alert-key))
  )
    (asserts! (>= (get risk-level zone) HIGH_RISK_THRESHOLD) ERR_INVALID_RISK_LEVEL)
    (asserts! (is-none existing-alert) ERR_ALERT_ALREADY_SET)
    (asserts! (and (>= duration u144) (<= duration u1008)) ERR_INVALID_REPORT)
    
    (map-set zone-alerts alert-key {
      creator: tx-sender,
      creation-time: current-block,
      expiry-time: (+ current-block duration),
      active: true,
      reward-claimed: false
    })
    
    (map-set pollution-zones zone-id (merge zone { alert-active: true }))
    (ok true)
  )
)

(define-public (claim-zone-alert-reward (zone-id uint) (alert-type (string-ascii 20)))
  (let (
    (alert-key { zone-id: zone-id, alert-type: alert-type })
    (alert (unwrap! (map-get? zone-alerts alert-key) ERR_REPORT_NOT_FOUND))
    (zone (unwrap! (map-get? pollution-zones zone-id) ERR_ZONE_NOT_FOUND))
    (current-block stacks-block-height)
  )
    (asserts! (is-eq tx-sender (get creator alert)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get reward-claimed alert)) ERR_ALREADY_CLAIMED)
    (asserts! (>= current-block (get expiry-time alert)) ERR_VOTING_CLOSED)
    (asserts! (>= (get risk-level zone) HIGH_RISK_THRESHOLD) ERR_INVALID_RISK_LEVEL)
    
    (try! (as-contract (stx-transfer? ZONE_ALERT_REWARD tx-sender tx-sender)))
    (map-set zone-alerts alert-key (merge alert { reward-claimed: true }))
    (ok ZONE_ALERT_REWARD)
  )
)

(define-public (update-zone-risk-level (zone-id uint))
  (let (
    (zone (unwrap! (map-get? pollution-zones zone-id) ERR_ZONE_NOT_FOUND))
    (current-block stacks-block-height)
    (risk-calculation (calculate-zone-risk zone-id))
  )
    (map-set pollution-zones zone-id (merge zone {
      risk-level: risk-calculation,
      last-updated: current-block
    }))
    
    (map-set zone-risk-history 
      { zone-id: zone-id, block-height: current-block }
      { risk-level: risk-calculation, report-count: (get total-reports zone) }
    )
    
    (ok risk-calculation)
  )
)

(define-private (update-zone-statistics (zone-id uint) (report-id uint))
  (match (map-get? pollution-zones zone-id)
    some-zone (match (map-get? pollution-reports report-id)
      some-report (let (
        (current-block stacks-block-height)
        (new-total-reports (+ (get total-reports some-zone) u1))
        (new-validated-reports (if (get validated some-report) 
                                 (+ (get validated-reports some-zone) u1) 
                                 (get validated-reports some-zone)))
      )
        (map-set pollution-zones zone-id (merge some-zone {
          total-reports: new-total-reports,
          validated-reports: new-validated-reports,
          last-updated: current-block
        }))
        true
      )
      false
    )
    false
  )
)

(define-private (calculate-zone-risk (zone-id uint))
  (let (
    (zone (unwrap! (map-get? pollution-zones zone-id) u0))
    (zone-total-reports (get total-reports zone))
    (zone-validated-reports (get validated-reports zone))
  )
    (if (is-eq zone-total-reports u0)
      u0
      (let (
        (validation-rate (* (/ zone-validated-reports zone-total-reports) u100))
        (report-density (if (<= zone-total-reports u10) zone-total-reports u10))
        (base-risk (* validation-rate report-density))
      )
        (if (<= base-risk u100) base-risk u100)
      )
    )
  )
)

(define-read-only (get-zone (zone-id uint))
  (map-get? pollution-zones zone-id)
)

(define-read-only (get-zone-alerts (zone-id uint) (alert-type (string-ascii 20)))
  (map-get? zone-alerts { zone-id: zone-id, alert-type: alert-type })
)

(define-read-only (get-zone-subscription (zone-id uint) (subscriber principal))
  (map-get? zone-subscribers { zone-id: zone-id, subscriber: subscriber })
)

(define-read-only (is-report-in-zone (zone-id uint) (report-id uint))
  (default-to false (map-get? zone-reports { zone-id: zone-id, report-id: report-id }))
)

(define-read-only (get-zone-risk-history (zone-id uint) (target-block uint))
  (map-get? zone-risk-history { zone-id: zone-id, block-height: target-block })
)

(define-read-only (get-zone-count)
  (var-get next-zone-id)
)

(define-read-only (is-zone-high-risk (zone-id uint))
  (match (map-get? pollution-zones zone-id)
    some-zone (>= (get risk-level some-zone) HIGH_RISK_THRESHOLD)
    false
  )
)

(define-map pollution-sources
  uint
  {
    source-name: (string-ascii 100),
    owner: principal,
    source-type: (string-ascii 50),
    location: (string-ascii 100),
    registration-time: uint,
    deposit-amount: uint,
    active: bool,
    compliance-score: uint,
    total-emissions: uint,
    last-report-time: uint
  }
)

(define-map source-emissions
  { source-id: uint, period: uint }
  {
    emission-amount: uint,
    emission-type: (string-ascii 50),
    reported-by: principal,
    report-time: uint,
    verified: bool,
    exceeds-limit: bool
  }
)

(define-map compliance-records
  { source-id: uint, period: uint }
  {
    compliance-status: bool,
    penalty-amount: uint,
    reward-amount: uint,
    assessment-time: uint,
    assessed-by: principal
  }
)

(define-map source-community-reports
  uint
  {
    source-id: uint,
    reporter: principal,
    report-type: (string-ascii 50),
    description: (string-ascii 300),
    severity: uint,
    report-time: uint,
    verified: bool,
    reward-claimed: bool
  }
)

(define-map source-deposits
  principal
  uint
)

(define-public (register-pollution-source 
  (source-name (string-ascii 100))
  (source-type (string-ascii 50))
  (location (string-ascii 100))
)
  (let (
    (source-id (var-get next-source-id))
    (current-block stacks-block-height)
    (existing-source (map-get? pollution-sources source-id))
  )
    (asserts! (is-none existing-source) ERR_SOURCE_ALREADY_REGISTERED)
    (asserts! (>= (len source-name) u3) ERR_INVALID_EMISSION_DATA)
    
    (try! (stx-transfer? SOURCE_REGISTRATION_DEPOSIT tx-sender (as-contract tx-sender)))
    
    (map-set pollution-sources source-id {
      source-name: source-name,
      owner: tx-sender,
      source-type: source-type,
      location: location,
      registration-time: current-block,
      deposit-amount: SOURCE_REGISTRATION_DEPOSIT,
      active: true,
      compliance-score: u100,
      total-emissions: u0,
      last-report-time: u0
    })
    
    (map-set source-deposits tx-sender SOURCE_REGISTRATION_DEPOSIT)
    (var-set next-source-id (+ source-id u1))
    (ok source-id)
  )
)

(define-public (report-emissions 
  (source-id uint)
  (emission-amount uint)
  (emission-type (string-ascii 50))
  (period uint)
)
  (let (
    (source (unwrap! (map-get? pollution-sources source-id) ERR_SOURCE_NOT_FOUND))
    (current-block stacks-block-height)
    (emission-key { source-id: source-id, period: period })
    (existing-emission (map-get? source-emissions emission-key))
  )
    (asserts! (is-eq tx-sender (get owner source)) ERR_NOT_AUTHORIZED)
    (asserts! (get active source) ERR_SOURCE_NOT_FOUND)
    (asserts! (is-none existing-emission) ERR_COMPLIANCE_PERIOD_ACTIVE)
    (asserts! (> emission-amount u0) ERR_INVALID_EMISSION_DATA)
    
    (let (
      (exceeds-limit (> emission-amount u1000))
      (new-total-emissions (+ (get total-emissions source) emission-amount))
    )
      (map-set source-emissions emission-key {
        emission-amount: emission-amount,
        emission-type: emission-type,
        reported-by: tx-sender,
        report-time: current-block,
        verified: false,
        exceeds-limit: exceeds-limit
      })
      
      (map-set pollution-sources source-id (merge source {
        total-emissions: new-total-emissions,
        last-report-time: current-block
      }))
      
      (ok true)
    )
  )
)

(define-public (assess-compliance 
  (source-id uint)
  (period uint)
  (is-compliant bool)
)
  (let (
    (source (unwrap! (map-get? pollution-sources source-id) ERR_SOURCE_NOT_FOUND))
    (emission-key { source-id: source-id, period: period })
    (emission (unwrap! (map-get? source-emissions emission-key) ERR_INVALID_EMISSION_DATA))
    (compliance-key { source-id: source-id, period: period })
    (existing-record (map-get? compliance-records compliance-key))
    (current-block stacks-block-height)
  )
    (asserts! (is-none existing-record) ERR_COMPLIANCE_PERIOD_ACTIVE)
    (asserts! (get verified emission) ERR_INVALID_EMISSION_DATA)
    
    (let (
      (penalty (if is-compliant u0 (* (get emission-amount emission) EMISSION_PENALTY_RATE)))
      (reward (if is-compliant COMPLIANCE_REWARD u0))
      (new-score (if is-compliant 
                   (if (< (get compliance-score source) u100) 
                     (+ (get compliance-score source) u5) 
                     u100)
                   (if (> (get compliance-score source) u10) 
                     (- (get compliance-score source) u10) 
                     u0)))
    )
      (map-set compliance-records compliance-key {
        compliance-status: is-compliant,
        penalty-amount: penalty,
        reward-amount: reward,
        assessment-time: current-block,
        assessed-by: tx-sender
      })
      
      (map-set pollution-sources source-id (merge source {
        compliance-score: new-score
      }))
      
      (if is-compliant
        (try! (as-contract (stx-transfer? reward (get owner source) (get owner source))))
        (try! (stx-transfer? penalty (get owner source) (as-contract tx-sender)))
      )
      
      (ok is-compliant)
    )
  )
)

(define-public (submit-community-source-report 
  (source-id uint)
  (report-type (string-ascii 50))
  (description (string-ascii 300))
  (severity uint)
)
  (let (
    (source (unwrap! (map-get? pollution-sources source-id) ERR_SOURCE_NOT_FOUND))
    (report-id (var-get next-report-id))
    (current-block stacks-block-height)
  )
    (asserts! (get active source) ERR_SOURCE_NOT_FOUND)
    (asserts! (and (>= severity u1) (<= severity u10)) ERR_INVALID_EMISSION_DATA)
    (asserts! (>= (len description) u10) ERR_INVALID_EMISSION_DATA)
    
    (map-set source-community-reports report-id {
      source-id: source-id,
      reporter: tx-sender,
      report-type: report-type,
      description: description,
      severity: severity,
      report-time: current-block,
      verified: false,
      reward-claimed: false
    })
    
    (var-set next-report-id (+ report-id u1))
    (ok report-id)
  )
)

(define-public (verify-community-report (report-id uint) (is-valid bool))
  (let (
    (report (unwrap! (map-get? source-community-reports report-id) ERR_REPORT_NOT_FOUND))
    (source (unwrap! (map-get? pollution-sources (get source-id report)) ERR_SOURCE_NOT_FOUND))
  )
    (asserts! (not (get verified report)) ERR_ALREADY_CLAIMED)
    
    (map-set source-community-reports report-id (merge report { verified: is-valid }))
    
    (if is-valid
      (let (
        (new-score (if (> (get compliance-score source) u5) 
                     (- (get compliance-score source) u5) 
                     u0))
      )
        (map-set pollution-sources (get source-id report) (merge source {
          compliance-score: new-score
        }))
        (ok true)
      )
      (ok false)
    )
  )
)

(define-public (claim-community-report-reward (report-id uint))
  (let (
    (report (unwrap! (map-get? source-community-reports report-id) ERR_REPORT_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get reporter report)) ERR_NOT_AUTHORIZED)
    (asserts! (get verified report) ERR_INVALID_EMISSION_DATA)
    (asserts! (not (get reward-claimed report)) ERR_ALREADY_CLAIMED)
    
    (try! (as-contract (stx-transfer? COMMUNITY_REPORT_REWARD tx-sender tx-sender)))
    (map-set source-community-reports report-id (merge report { reward-claimed: true }))
    (ok COMMUNITY_REPORT_REWARD)
  )
)

(define-public (deactivate-source (source-id uint))
  (let (
    (source (unwrap! (map-get? pollution-sources source-id) ERR_SOURCE_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get owner source)) ERR_NOT_AUTHORIZED)
    (asserts! (get active source) ERR_SOURCE_NOT_FOUND)
    
    (map-set pollution-sources source-id (merge source { active: false }))
    
    (let (
      (deposit-refund (if (>= (get compliance-score source) u80) 
                        (get deposit-amount source) 
                        (/ (get deposit-amount source) u2)))
    )
      (try! (as-contract (stx-transfer? deposit-refund tx-sender tx-sender)))
      (ok deposit-refund)
    )
  )
)

(define-read-only (get-pollution-source (source-id uint))
  (map-get? pollution-sources source-id)
)

(define-read-only (get-source-emissions (source-id uint) (period uint))
  (map-get? source-emissions { source-id: source-id, period: period })
)

(define-read-only (get-compliance-record (source-id uint) (period uint))
  (map-get? compliance-records { source-id: source-id, period: period })
)

(define-read-only (get-community-source-report (report-id uint))
  (map-get? source-community-reports report-id)
)

(define-read-only (get-source-count)
  (var-get next-source-id)
)

(define-read-only (is-source-compliant (source-id uint))
  (match (map-get? pollution-sources source-id)
    some-source (>= (get compliance-score some-source) u70)
    false
  )
)

;; Badge system functions
(define-private (check-and-award-badges (user principal))
  (let (
    (user-rep (get-user-reputation user))
    (current-badges (default-to { badges: (list), zones-created: u0, correct-votes: u0 }
                                (map-get? user-badges user)))
    (user-reports (get total-reports user-rep))
    (validated-reports (get validated-reports user-rep))
    (user-score (get score user-rep))
    (zones-created (get zones-created current-badges))
    (correct-votes (get correct-votes current-badges))
    (current-badge-list (get badges current-badges))
  )
    (let (
      (new-badges (award-reporter-badges current-badge-list user-reports validated-reports))
      (final-badges (award-validator-badges new-badges correct-votes))
      (champion-badges (award-champion-badge final-badges user-score))
      (explorer-badges (award-explorer-badge champion-badges zones-created))
    )
      (map-set user-badges user (merge current-badges { badges: explorer-badges }))
    )
  )
)

(define-private (award-reporter-badges (current-badges (list 20 (string-ascii 20))) (total-reports uint) (validated-reports uint))
  (let (
    (has-rookie (is-some (index-of current-badges BADGE_ROOKIE_REPORTER)))
    (has-veteran (is-some (index-of current-badges BADGE_VETERAN_REPORTER)))
    (rookie-qualified (and (not has-rookie) (>= total-reports ROOKIE_THRESHOLD)))
    (veteran-qualified (and (not has-veteran) (>= validated-reports VETERAN_THRESHOLD)))
  )
    (if rookie-qualified
      (let ((with-rookie (unwrap! (as-max-len? (append current-badges BADGE_ROOKIE_REPORTER) u20) current-badges)))
        (if veteran-qualified
          (unwrap! (as-max-len? (append with-rookie BADGE_VETERAN_REPORTER) u20) with-rookie)
          with-rookie
        )
      )
      (if veteran-qualified
        (unwrap! (as-max-len? (append current-badges BADGE_VETERAN_REPORTER) u20) current-badges)
        current-badges
      )
    )
  )
)

(define-private (award-validator-badges (current-badges (list 20 (string-ascii 20))) (correct-votes uint))
  (let (
    (has-trusted (is-some (index-of current-badges BADGE_TRUSTED_VALIDATOR)))
    (trusted-qualified (and (not has-trusted) (>= correct-votes VALIDATOR_THRESHOLD)))
  )
    (if trusted-qualified
      (unwrap! (as-max-len? (append current-badges BADGE_TRUSTED_VALIDATOR) u20) current-badges)
      current-badges
    )
  )
)

(define-private (award-champion-badge (current-badges (list 20 (string-ascii 20))) (user-score uint))
  (let (
    (has-champion (is-some (index-of current-badges BADGE_COMMUNITY_CHAMPION)))
    (champion-qualified (and (not has-champion) (>= user-score CHAMPION_REP_THRESHOLD)))
  )
    (if champion-qualified
      (unwrap! (as-max-len? (append current-badges BADGE_COMMUNITY_CHAMPION) u20) current-badges)
      current-badges
    )
  )
)

(define-private (award-explorer-badge (current-badges (list 20 (string-ascii 20))) (zones-created uint))
  (let (
    (has-explorer (is-some (index-of current-badges BADGE_ZONE_EXPLORER)))
    (explorer-qualified (and (not has-explorer) (>= zones-created EXPLORER_ZONE_THRESHOLD)))
  )
    (if explorer-qualified
      (unwrap! (as-max-len? (append current-badges BADGE_ZONE_EXPLORER) u20) current-badges)
      current-badges
    )
  )
)

(define-private (increment-correct-votes (user principal))
  (let (
    (current-badges (default-to { badges: (list), zones-created: u0, correct-votes: u0 }
                                (map-get? user-badges user)))
    (new-correct-votes (+ (get correct-votes current-badges) u1))
  )
    (map-set user-badges user (merge current-badges { correct-votes: new-correct-votes }))
  )
)

(define-private (increment-zones-created (user principal))
  (let (
    (current-badges (default-to { badges: (list), zones-created: u0, correct-votes: u0 }
                                (map-get? user-badges user)))
    (new-zones-created (+ (get zones-created current-badges) u1))
  )
    (map-set user-badges user (merge current-badges { zones-created: new-zones-created }))
  )
)

(define-read-only (get-user-badges (user principal))
  (default-to { badges: (list), zones-created: u0, correct-votes: u0 }
              (map-get? user-badges user))
)


