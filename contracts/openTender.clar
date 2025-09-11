(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-TENDER-CLOSED (err u101))
(define-constant ERR-TENDER-OPEN (err u102))
(define-constant ERR-LOW-BID (err u103))
(define-constant ERR-NO-BIDS (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))
(define-constant ERR-NOT-FOUND (err u106))
(define-constant ERR-DEADLINE-PASSED (err u107))

(define-data-var tender-counter uint u0)

(define-map tenders
    { tender-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        owner: principal,
        deadline: uint,
        minimum-bid: uint,
        status: (string-ascii 20),
        winner: (optional principal)
    }
)

(define-map bids
    { tender-id: uint, bidder: principal }
    {
        amount: uint,
        proposal: (string-ascii 500),
        timestamp: uint
    }
)

(define-read-only (get-tender (tender-id uint))
    (map-get? tenders { tender-id: tender-id })
)

(define-read-only (get-bid (tender-id uint) (bidder principal))
    (map-get? bids { tender-id: tender-id, bidder: bidder })
)

(define-public (create-tender (title (string-ascii 100)) (description (string-ascii 500)) (deadline uint) (minimum-bid uint))
    (let (
        (tender-id (+ (var-get tender-counter) u1))
        (required-deposit (/ (* minimum-bid (var-get deposit-percentage)) u100))
    )
        (if (> deadline stacks-block-height)
            (begin
                (map-set tenders
                    { tender-id: tender-id }
                    {
                        title: title,
                        description: description,
                        owner: tx-sender,
                        deadline: deadline,
                        minimum-bid: minimum-bid,
                        status: "open",
                        winner: none
                    }
                )
                (map-set tender-deposit-requirements
                    { tender-id: tender-id }
                    {
                        required-deposit: required-deposit,
                        total-deposits: u0
                    }
                )
                (var-set tender-counter tender-id)
                (ok tender-id))
            (err ERR-DEADLINE-PASSED)
        )
    )
)

(define-public (submit-bid (tender-id uint) (amount uint) (proposal (string-ascii 500)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (deposit-req (unwrap! (map-get? tender-deposit-requirements { tender-id: tender-id }) (err ERR-NOT-FOUND)))
        (required-deposit (get required-deposit deposit-req))
        (existing-bid (map-get? bids { tender-id: tender-id, bidder: tx-sender }))
    )
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        (asserts! (>= amount (get minimum-bid tender)) (err ERR-LOW-BID))
        (asserts! (< stacks-block-height (get deadline tender)) (err ERR-DEADLINE-PASSED))
        (if (is-none existing-bid)
            (begin
                (unwrap! (stx-transfer? required-deposit tx-sender (as-contract tx-sender)) (err ERR-DEPOSIT-TRANSFER-FAILED))
                (map-set bid-deposits
                    { tender-id: tender-id, bidder: tx-sender }
                    {
                        amount: required-deposit,
                        refunded: false,
                        timestamp: stacks-block-height
                    }
                )
                (map-set tender-deposit-requirements
                    { tender-id: tender-id }
                    (merge deposit-req { total-deposits: (+ (get total-deposits deposit-req) required-deposit) })
                )
            )
            true
        )
        (map-set bids
            { tender-id: tender-id, bidder: tx-sender }
            {
                amount: amount,
                proposal: proposal,
                timestamp: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (close-tender (tender-id uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (owner (get owner tender))
    )
        (asserts! (is-eq tx-sender owner) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        (map-set tenders
            { tender-id: tender-id }
            (merge tender { status: "closed" })
        )
        (ok true)
    )
)

(define-public (select-winner (tender-id uint) (winner principal))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (owner (get owner tender))
        (bid (unwrap! (get-bid tender-id winner) (err ERR-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender owner) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status tender) "closed") (err ERR-TENDER-OPEN))
        (map-set tenders
            { tender-id: tender-id }
            (merge tender { winner: (some winner) })
        )

        (ok true)
    )
)

(define-read-only (get-all-bids (tender-id uint))
    (map-get? bids { tender-id: tender-id, bidder: tx-sender })
)


(define-constant ERR-NO-BID-EXISTS (err u108))
(define-constant ERR-BID-LOCKED (err u109))
(define-constant ERR-INSUFFICIENT-DEPOSIT (err u110))
(define-constant ERR-DEPOSIT-TRANSFER-FAILED (err u111))
(define-constant ERR-REFUND-FAILED (err u116))
(define-constant ERR-DEPOSIT-ALREADY-REFUNDED (err u117))
(define-constant ERR-DEPOSIT-NOT-FOUND (err u118))

(define-data-var deposit-percentage uint u10)

(define-map bid-deposits
    { tender-id: uint, bidder: principal }
    {
        amount: uint,
        refunded: bool,
        timestamp: uint
    }
)

(define-map tender-deposit-requirements
    { tender-id: uint }
    {
        required-deposit: uint,
        total-deposits: uint
    }
)

(define-private (refund-losing-bidders (tender-id uint) (winner principal))
    (ok true)
)

(define-public (refund-deposit (tender-id uint) (bidder principal))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (deposit (unwrap! (map-get? bid-deposits { tender-id: tender-id, bidder: bidder }) (err ERR-DEPOSIT-NOT-FOUND)))
        (tender-winner (get winner tender))
    )
        (asserts! (is-some tender-winner) (err ERR-TENDER-OPEN))
        (asserts! (not (is-eq bidder (unwrap-panic tender-winner))) (err ERR-NOT-AUTHORIZED))
        (asserts! (not (get refunded deposit)) (err ERR-DEPOSIT-ALREADY-REFUNDED))
        (unwrap! (as-contract (stx-transfer? (get amount deposit) tx-sender bidder)) (err ERR-REFUND-FAILED))
        (map-set bid-deposits
            { tender-id: tender-id, bidder: bidder }
            (merge deposit { refunded: true })
        )
        (ok true)
    )
)

(define-public (claim-winner-deposit (tender-id uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (winner (unwrap! (get winner tender) (err ERR-NOT-FOUND)))
        (deposit (unwrap! (map-get? bid-deposits { tender-id: tender-id, bidder: tx-sender }) (err ERR-DEPOSIT-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender winner) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status tender) "completed") (err ERR-TENDER-NOT-COMPLETED))
        (asserts! (not (get refunded deposit)) (err ERR-DEPOSIT-ALREADY-REFUNDED))
        (unwrap! (as-contract (stx-transfer? (get amount deposit) tx-sender tx-sender)) (err ERR-REFUND-FAILED))
        (map-set bid-deposits
            { tender-id: tender-id, bidder: tx-sender }
            (merge deposit { refunded: true })
        )
        (ok true)
    )
)

(define-read-only (get-bid-deposit (tender-id uint) (bidder principal))
    (map-get? bid-deposits { tender-id: tender-id, bidder: bidder })
)

(define-read-only (get-deposit-requirements (tender-id uint))
    (map-get? tender-deposit-requirements { tender-id: tender-id })
)

(define-public (set-deposit-percentage (new-percentage uint))
    (begin
        (asserts! (<= new-percentage u50) (err ERR-INVALID-RATING))
        (var-set deposit-percentage new-percentage)
        (ok true)
    )
)

(define-read-only (get-deposit-percentage)
    (var-get deposit-percentage)
)

(define-public (emergency-refund (tender-id uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (deposit (unwrap! (map-get? bid-deposits { tender-id: tender-id, bidder: tx-sender }) (err ERR-DEPOSIT-NOT-FOUND)))
    )
        (asserts! (> stacks-block-height (+ (get deadline tender) u1000)) (err ERR-NOT-AUTHORIZED))
        (asserts! (not (get refunded deposit)) (err ERR-DEPOSIT-ALREADY-REFUNDED))
        (unwrap! (as-contract (stx-transfer? (get amount deposit) tx-sender tx-sender)) (err ERR-REFUND-FAILED))
        (map-set bid-deposits
            { tender-id: tender-id, bidder: tx-sender }
            (merge deposit { refunded: true })
        )
        (ok true)
    )
)

(define-public (withdraw-bid (tender-id uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (bid (unwrap! (get-bid tender-id tx-sender) (err ERR-NO-BID-EXISTS)))
        (deposit (map-get? bid-deposits { tender-id: tender-id, bidder: tx-sender }))
    )
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        (asserts! (< stacks-block-height (get deadline tender)) (err ERR-DEADLINE-PASSED))
        (map-delete bids { tender-id: tender-id, bidder: tx-sender })
        (match deposit
            deposit-data
            (begin
                (unwrap! (as-contract (stx-transfer? (get amount deposit-data) tx-sender tx-sender)) (err ERR-REFUND-FAILED))
                (map-set bid-deposits
                    { tender-id: tender-id, bidder: tx-sender }
                    (merge deposit-data { refunded: true })
                )
            )
            true
        )
        (ok true)
    )
)


(define-map tender-categories 
    { tender-id: uint }
    { category: (string-ascii 50) }
)

(define-public (create-tender-with-category 
    (title (string-ascii 100)) 
    (description (string-ascii 500)) 
    (deadline uint) 
    (minimum-bid uint)
    (category (string-ascii 50)))
    (let (
        (tender-id (+ (var-get tender-counter) u1))
        (required-deposit (/ (* minimum-bid (var-get deposit-percentage)) u100))
    )
        (if (> deadline stacks-block-height)
            (begin
                (map-set tenders
                    { tender-id: tender-id }
                    {
                        title: title,
                        description: description,
                        owner: tx-sender,
                        deadline: deadline,
                        minimum-bid: minimum-bid,
                        status: "open",
                        winner: none
                    }
                )
                (map-set tender-categories
                    { tender-id: tender-id }
                    { category: category }
                )
                (map-set tender-deposit-requirements
                    { tender-id: tender-id }
                    {
                        required-deposit: required-deposit,
                        total-deposits: u0
                    }
                )
                (var-set tender-counter tender-id)
                (ok tender-id))
            (err ERR-DEADLINE-PASSED)
        )
    )
)

(define-read-only (get-tender-category (tender-id uint))
    (map-get? tender-categories { tender-id: tender-id })
)


(define-public (update-tender-category (tender-id uint) (new-category (string-ascii 50)))
    (let ((tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND))))
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (map-set tender-categories
            { tender-id: tender-id }
            { category: new-category }
        )
        (ok true)
    )
)


(define-constant ERR-RATING-EXISTS (err u112))
(define-constant ERR-INVALID-RATING (err u113))
(define-constant ERR-TENDER-NOT-COMPLETED (err u114))
(define-constant ERR-NOT-PARTICIPANT (err u115))

(define-map user-ratings
    { user: principal }
    {
        total-rating: uint,
        rating-count: uint,
        completed-tenders: uint
    }
)

(define-map tender-ratings
    { tender-id: uint, rater: principal, rated: principal }
    {
        rating: uint,
        comment: (string-ascii 200),
        timestamp: uint
    }
)

(define-public (rate-user (tender-id uint) (rated-user principal) (rating uint) (comment (string-ascii 200)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (tender-owner (get owner tender))
        (tender-winner (get winner tender))
    )
        (asserts! (and (>= rating u1) (<= rating u5)) (err ERR-INVALID-RATING))
        (asserts! (is-eq (get status tender) "completed") (err ERR-TENDER-NOT-COMPLETED))
        (asserts! (is-none (map-get? tender-ratings { tender-id: tender-id, rater: tx-sender, rated: rated-user })) (err ERR-RATING-EXISTS))
        (asserts! 
            (or 
                (and (is-eq tx-sender tender-owner) (is-eq rated-user (unwrap! tender-winner (err ERR-NOT-PARTICIPANT))))
                (and (is-eq tx-sender (unwrap! tender-winner (err ERR-NOT-PARTICIPANT))) (is-eq rated-user tender-owner))
            ) 
            (err ERR-NOT-PARTICIPANT)
        )
        (let (
            (current-ratings (default-to { total-rating: u0, rating-count: u0, completed-tenders: u0 } 
                                       (map-get? user-ratings { user: rated-user })))
            (new-total (+ (get total-rating current-ratings) rating))
            (new-count (+ (get rating-count current-ratings) u1))
            (new-completed (if (is-eq rated-user (unwrap! tender-winner (err ERR-NOT-PARTICIPANT)))
                             (+ (get completed-tenders current-ratings) u1)
                             (get completed-tenders current-ratings)))
        )
            (map-set tender-ratings
                { tender-id: tender-id, rater: tx-sender, rated: rated-user }
                {
                    rating: rating,
                    comment: comment,
                    timestamp: stacks-block-height
                }
            )
            (map-set user-ratings
                { user: rated-user }
                {
                    total-rating: new-total,
                    rating-count: new-count,
                    completed-tenders: new-completed
                }
            )
            (ok true)
        )
    )
)

(define-read-only (get-user-reputation (user principal))
    (match (map-get? user-ratings { user: user })
        rating-data
        (if (> (get rating-count rating-data) u0)
            (some {
                average-rating: (/ (* (get total-rating rating-data) u100) (get rating-count rating-data)),
                total-ratings: (get rating-count rating-data),
                completed-tenders: (get completed-tenders rating-data)
            })
            (some { average-rating: u0, total-ratings: u0, completed-tenders: u0 }))
        none
    )
)

(define-read-only (get-tender-rating (tender-id uint) (rater principal) (rated principal))
    (map-get? tender-ratings { tender-id: tender-id, rater: rater, rated: rated })
)

(define-read-only (get-user-rating-summary (user principal))
    (let ((reputation (get-user-reputation user)))
        (match reputation
            rep-data
            (some {
                average-rating-display: (/ (get average-rating rep-data) u20),
                star-rating: (if (>= (get average-rating rep-data) u500) u5
                            (if (>= (get average-rating rep-data) u400) u4
                            (if (>= (get average-rating rep-data) u300) u3
                            (if (>= (get average-rating rep-data) u200) u2 u1)))),
                total-reviews: (get total-ratings rep-data),
                projects-completed: (get completed-tenders rep-data)
            })
            none
        )
    )
)

(define-read-only (is-reputable-user (user principal) (min-rating uint) (min-completed uint))
    (match (get-user-reputation user)
        reputation
        (and 
            (>= (get average-rating reputation) (* min-rating u20))
            (>= (get completed-tenders reputation) min-completed)
            (>= (get total-ratings reputation) u1)
        )
        false
    )
)

(define-public (complete-tender-with-rating (tender-id uint) (winner principal) (rating uint) (comment (string-ascii 200)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (owner (get owner tender))
        (bid (unwrap! (get-bid tender-id winner) (err ERR-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender owner) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status tender) "closed") (err ERR-TENDER-OPEN))
        (asserts! (and (>= rating u1) (<= rating u5)) (err ERR-INVALID-RATING))
        (map-set tenders
            { tender-id: tender-id }
            (merge tender { winner: (some winner), status: "completed" })
        )
        (rate-user tender-id winner rating comment)
    )
)

(define-constant ERR-MILESTONE-NOT-FOUND (err u201))
(define-constant ERR-MILESTONE-ALREADY-COMPLETED (err u202))
(define-constant ERR-MILESTONE-NOT-APPROVED (err u203))
(define-constant ERR-INSUFFICIENT-ESCROW (err u204))
(define-constant ERR-INVALID-MILESTONE-INDEX (err u205))
(define-constant ERR-MILESTONE-DISPUTED (err u206))
(define-constant ERR-ESCROW-RELEASE-FAILED (err u207))
(define-constant ERR-DISPUTE-PERIOD-EXPIRED (err u208))

(define-data-var dispute-period-blocks uint u1440)

(define-map tender-milestones
    { tender-id: uint }
    {
        milestone-count: uint,
        current-milestone: uint,
        total-escrow: uint,
        released-escrow: uint,
        milestones-enabled: bool
    }
)

(define-map milestone-details
    { tender-id: uint, milestone-index: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 300),
        payment-amount: uint,
        deadline: uint,
        status: (string-ascii 20),
        submitted-at: (optional uint),
        approved-at: (optional uint),
        disputed-at: (optional uint),
        deliverable-hash: (optional (string-ascii 64))
    }
)

(define-map milestone-disputes
    { tender-id: uint, milestone-index: uint }
    {
        disputed-by: principal,
        dispute-reason: (string-ascii 300),
        resolved: bool,
        resolution: (string-ascii 300),
        resolved-at: (optional uint)
    }
)

(define-map tender-escrow
    { tender-id: uint }
    {
        total-amount: uint,
        locked-amount: uint,
        released-amount: uint,
        depositor: principal
    }
)

(define-private (create-single-milestone 
    (tender-id uint)
    (milestone-index uint)
    (title (string-ascii 100))
    (description (string-ascii 300))
    (payment-amount uint)
    (deadline uint))
    (begin
        (map-set milestone-details
            { tender-id: tender-id, milestone-index: milestone-index }
            {
                title: title,
                description: description,
                payment-amount: payment-amount,
                deadline: deadline,
                status: "pending",
                submitted-at: none,
                approved-at: none,
                disputed-at: none,
                deliverable-hash: none
            }
        )
        (ok true)
    )
)

(define-public (create-milestone-tender 
    (title (string-ascii 100)) 
    (description (string-ascii 500)) 
    (deadline uint) 
    (minimum-bid uint)
    (milestone-titles (list 10 (string-ascii 100)))
    (milestone-descriptions (list 10 (string-ascii 300)))
    (milestone-payments (list 10 uint))
    (milestone-deadlines (list 10 uint)))
    (let (
        (tender-id (+ (var-get tender-counter) u1))
        (required-deposit (/ (* minimum-bid (var-get deposit-percentage)) u100))
        (milestone-count (len milestone-titles))
        (total-milestone-payment (fold + milestone-payments u0))
    )
        (asserts! (and (> milestone-count u0) (<= milestone-count u10)) (err ERR-INVALID-MILESTONE-INDEX))
        (asserts! (is-eq total-milestone-payment minimum-bid) (err ERR-INSUFFICIENT-ESCROW))
        (asserts! (> deadline stacks-block-height) (err ERR-DEADLINE-PASSED))
        (map-set tenders
            { tender-id: tender-id }
            {
                title: title,
                description: description,
                owner: tx-sender,
                deadline: deadline,
                minimum-bid: minimum-bid,
                status: "open",
                winner: none
            }
        )
        (map-set tender-milestones
            { tender-id: tender-id }
            {
                milestone-count: milestone-count,
                current-milestone: u0,
                total-escrow: u0,
                released-escrow: u0,
                milestones-enabled: true
            }
        )
        (map-set tender-deposit-requirements
            { tender-id: tender-id }
            {
                required-deposit: required-deposit,
                total-deposits: u0
            }
        )
        (var-set tender-counter tender-id)
        (ok tender-id)
    )
)

(define-public (setup-milestone (tender-id uint) (milestone-index uint) (title (string-ascii 100)) (description (string-ascii 300)) (payment-amount uint) (deadline uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (milestone-data (unwrap! (map-get? tender-milestones { tender-id: tender-id }) (err ERR-MILESTONE-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (< milestone-index (get milestone-count milestone-data)) (err ERR-INVALID-MILESTONE-INDEX))
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        (create-single-milestone tender-id milestone-index title description payment-amount deadline)
    )
)

(define-public (fund-milestone-escrow (tender-id uint) (amount uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (milestone-data (unwrap! (map-get? tender-milestones { tender-id: tender-id }) (err ERR-MILESTONE-NOT-FOUND)))
        (current-escrow (default-to { total-amount: u0, locked-amount: u0, released-amount: u0, depositor: tx-sender } 
                                   (map-get? tender-escrow { tender-id: tender-id })))
    )
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (get milestones-enabled milestone-data) (err ERR-MILESTONE-NOT-FOUND))
        (unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender)) (err ERR-DEPOSIT-TRANSFER-FAILED))
        (map-set tender-escrow
            { tender-id: tender-id }
            {
                total-amount: (+ (get total-amount current-escrow) amount),
                locked-amount: (+ (get locked-amount current-escrow) amount),
                released-amount: (get released-amount current-escrow),
                depositor: tx-sender
            }
        )
        (ok true)
    )
)

(define-public (submit-milestone-deliverable (tender-id uint) (milestone-index uint) (deliverable-hash (string-ascii 64)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (milestone (unwrap! (map-get? milestone-details { tender-id: tender-id, milestone-index: milestone-index }) (err ERR-MILESTONE-NOT-FOUND)))
        (milestone-data (unwrap! (map-get? tender-milestones { tender-id: tender-id }) (err ERR-MILESTONE-NOT-FOUND)))
        (winner (unwrap! (get winner tender) (err ERR-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender winner) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status milestone) "pending") (err ERR-MILESTONE-ALREADY-COMPLETED))
        (asserts! (is-eq milestone-index (get current-milestone milestone-data)) (err ERR-INVALID-MILESTONE-INDEX))
        (map-set milestone-details
            { tender-id: tender-id, milestone-index: milestone-index }
            (merge milestone {
                status: "submitted",
                submitted-at: (some stacks-block-height),
                deliverable-hash: (some deliverable-hash)
            })
        )
        (ok true)
    )
)

(define-public (approve-milestone (tender-id uint) (milestone-index uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (milestone (unwrap! (map-get? milestone-details { tender-id: tender-id, milestone-index: milestone-index }) (err ERR-MILESTONE-NOT-FOUND)))
        (milestone-data (unwrap! (map-get? tender-milestones { tender-id: tender-id }) (err ERR-MILESTONE-NOT-FOUND)))
        (escrow (unwrap! (map-get? tender-escrow { tender-id: tender-id }) (err ERR-INSUFFICIENT-ESCROW)))
        (winner (unwrap! (get winner tender) (err ERR-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status milestone) "submitted") (err ERR-MILESTONE-NOT-APPROVED))
        (asserts! (>= (get locked-amount escrow) (get payment-amount milestone)) (err ERR-INSUFFICIENT-ESCROW))
        (unwrap! (as-contract (stx-transfer? (get payment-amount milestone) tx-sender winner)) (err ERR-ESCROW-RELEASE-FAILED))
        (map-set milestone-details
            { tender-id: tender-id, milestone-index: milestone-index }
            (merge milestone {
                status: "completed",
                approved-at: (some stacks-block-height)
            })
        )
        (map-set tender-escrow
            { tender-id: tender-id }
            {
                total-amount: (get total-amount escrow),
                locked-amount: (- (get locked-amount escrow) (get payment-amount milestone)),
                released-amount: (+ (get released-amount escrow) (get payment-amount milestone)),
                depositor: (get depositor escrow)
            }
        )
        (map-set tender-milestones
            { tender-id: tender-id }
            (merge milestone-data {
                current-milestone: (+ (get current-milestone milestone-data) u1),
                released-escrow: (+ (get released-escrow milestone-data) (get payment-amount milestone))
            })
        )
        (ok true)
    )
)

(define-public (dispute-milestone (tender-id uint) (milestone-index uint) (reason (string-ascii 300)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (milestone (unwrap! (map-get? milestone-details { tender-id: tender-id, milestone-index: milestone-index }) (err ERR-MILESTONE-NOT-FOUND)))
        (submitted-at (unwrap! (get submitted-at milestone) (err ERR-MILESTONE-NOT-APPROVED)))
    )
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status milestone) "submitted") (err ERR-MILESTONE-NOT-APPROVED))
        (asserts! (< (- stacks-block-height submitted-at) (var-get dispute-period-blocks)) (err ERR-DISPUTE-PERIOD-EXPIRED))
        (map-set milestone-details
            { tender-id: tender-id, milestone-index: milestone-index }
            (merge milestone {
                status: "disputed",
                disputed-at: (some stacks-block-height)
            })
        )
        (map-set milestone-disputes
            { tender-id: tender-id, milestone-index: milestone-index }
            {
                disputed-by: tx-sender,
                dispute-reason: reason,
                resolved: false,
                resolution: "",
                resolved-at: none
            }
        )
        (ok true)
    )
)

(define-public (resolve-milestone-dispute (tender-id uint) (milestone-index uint) (approve bool) (resolution (string-ascii 300)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (milestone (unwrap! (map-get? milestone-details { tender-id: tender-id, milestone-index: milestone-index }) (err ERR-MILESTONE-NOT-FOUND)))
        (dispute (unwrap! (map-get? milestone-disputes { tender-id: tender-id, milestone-index: milestone-index }) (err ERR-MILESTONE-NOT-FOUND)))
        (escrow (unwrap! (map-get? tender-escrow { tender-id: tender-id }) (err ERR-INSUFFICIENT-ESCROW)))
        (milestone-data (unwrap! (map-get? tender-milestones { tender-id: tender-id }) (err ERR-MILESTONE-NOT-FOUND)))
        (winner (unwrap! (get winner tender) (err ERR-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status milestone) "disputed") (err ERR-MILESTONE-NOT-FOUND))
        (asserts! (not (get resolved dispute)) (err ERR-MILESTONE-ALREADY-COMPLETED))
        (map-set milestone-disputes
            { tender-id: tender-id, milestone-index: milestone-index }
            (merge dispute {
                resolved: true,
                resolution: resolution,
                resolved-at: (some stacks-block-height)
            })
        )
        (if approve
            (begin
                (unwrap! (as-contract (stx-transfer? (get payment-amount milestone) tx-sender winner)) (err ERR-ESCROW-RELEASE-FAILED))
                (map-set milestone-details
                    { tender-id: tender-id, milestone-index: milestone-index }
                    (merge milestone { status: "completed", approved-at: (some stacks-block-height) })
                )
                (map-set tender-escrow
                    { tender-id: tender-id }
                    {
                        total-amount: (get total-amount escrow),
                        locked-amount: (- (get locked-amount escrow) (get payment-amount milestone)),
                        released-amount: (+ (get released-amount escrow) (get payment-amount milestone)),
                        depositor: (get depositor escrow)
                    }
                )
                (map-set tender-milestones
                    { tender-id: tender-id }
                    (merge milestone-data {
                        current-milestone: (+ (get current-milestone milestone-data) u1),
                        released-escrow: (+ (get released-escrow milestone-data) (get payment-amount milestone))
                    })
                )
            )
            (map-set milestone-details
                { tender-id: tender-id, milestone-index: milestone-index }
                (merge milestone { status: "rejected" })
            )
        )
        (ok true)
    )
)

(define-read-only (get-milestone-details (tender-id uint) (milestone-index uint))
    (map-get? milestone-details { tender-id: tender-id, milestone-index: milestone-index })
)

(define-read-only (get-tender-milestones (tender-id uint))
    (map-get? tender-milestones { tender-id: tender-id })
)

(define-read-only (get-milestone-dispute (tender-id uint) (milestone-index uint))
    (map-get? milestone-disputes { tender-id: tender-id, milestone-index: milestone-index })
)

(define-read-only (get-tender-escrow (tender-id uint))
    (map-get? tender-escrow { tender-id: tender-id })
)

(define-read-only (get-milestone-progress (tender-id uint))
    (match (map-get? tender-milestones { tender-id: tender-id })
        milestone-data
        (some {
            current-milestone: (get current-milestone milestone-data),
            total-milestones: (get milestone-count milestone-data),
            completion-percentage: (if (> (get milestone-count milestone-data) u0)
                                     (/ (* (get current-milestone milestone-data) u100) (get milestone-count milestone-data))
                                     u0),
            total-escrow: (get total-escrow milestone-data),
            released-escrow: (get released-escrow milestone-data)
        })
        none
    )
)

(define-public (withdraw-remaining-escrow (tender-id uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (escrow (unwrap! (map-get? tender-escrow { tender-id: tender-id }) (err ERR-INSUFFICIENT-ESCROW)))
        (milestone-data (unwrap! (map-get? tender-milestones { tender-id: tender-id }) (err ERR-MILESTONE-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status tender) "completed") (err ERR-TENDER-NOT-COMPLETED))
        (asserts! (>= (get current-milestone milestone-data) (get milestone-count milestone-data)) (err ERR-MILESTONE-NOT-FOUND))
        (asserts! (> (get locked-amount escrow) u0) (err ERR-INSUFFICIENT-ESCROW))
        (unwrap! (as-contract (stx-transfer? (get locked-amount escrow) tx-sender tx-sender)) (err ERR-ESCROW-RELEASE-FAILED))
        (map-set tender-escrow
            { tender-id: tender-id }
            (merge escrow {
                locked-amount: u0,
                released-amount: (get total-amount escrow)
            })
        )
        (ok true)
    )
)

;; Insurance & Bonding System Constants
(define-constant ERR-INSURANCE-NOT-FOUND (err u301))
(define-constant ERR-INVALID-POLICY-TYPE (err u302))
(define-constant ERR-INSUFFICIENT-PREMIUM (err u303))
(define-constant ERR-CLAIM-ALREADY-EXISTS (err u304))
(define-constant ERR-CLAIM-NOT-ELIGIBLE (err u305))
(define-constant ERR-INSURANCE-EXPIRED (err u306))
(define-constant ERR-CLAIM-PROCESSING-FAILED (err u307))
(define-constant ERR-INVALID-COVERAGE-AMOUNT (err u308))

;; Insurance system variables
(define-data-var insurance-pool-balance uint u0)
(define-data-var base-premium-rate uint u5) ;; 5% base rate
(define-data-var claim-processing-fee uint u2) ;; 2% processing fee

;; Insurance policy storage
(define-map insurance-policies
    { policy-id: uint }
    {
        tender-id: uint,
        policyholder: principal,
        policy-type: (string-ascii 20), ;; "performance" or "completion"
        coverage-amount: uint,
        premium-paid: uint,
        start-block: uint,
        expiry-block: uint,
        active: bool,
        claims-count: uint
    }
)

;; Claims tracking
(define-map insurance-claims
    { claim-id: uint }
    {
        policy-id: uint,
        claimant: principal,
        claim-amount: uint,
        claim-reason: (string-ascii 300),
        submitted-at: uint,
        status: (string-ascii 20), ;; "pending", "approved", "denied", "paid"
        evidence-hash: (optional (string-ascii 64)),
        processed-at: (optional uint),
        payout-amount: (optional uint)
    }
)

;; Risk assessment profiles
(define-map user-risk-profiles
    { user: principal }
    {
        risk-score: uint, ;; 1-100 scale, lower is better
        total-policies: uint,
        successful-completions: uint,
        failed-projects: uint,
        total-claims: uint,
        last-updated: uint
    }
)

;; Counter variables
(define-data-var policy-counter uint u0)
(define-data-var claim-counter uint u0)

;; Calculate risk-adjusted premium based on user reputation and tender value
(define-private (calculate-premium (user principal) (coverage-amount uint) (policy-type (string-ascii 20)))
    (let (
        (base-rate (var-get base-premium-rate))
        (user-reputation (get-user-reputation user))
        (risk-profile (default-to 
            { risk-score: u50, total-policies: u0, successful-completions: u0, failed-projects: u0, total-claims: u0, last-updated: u0 }
            (map-get? user-risk-profiles { user: user })
        ))
    )
        (let (
            (reputation-factor (match user-reputation
                rep-data (if (> (get total-ratings rep-data) u0)
                    (if (>= (get average-rating rep-data) u400) u80  ;; Good rating = 20% discount
                    (if (>= (get average-rating rep-data) u300) u90  ;; Average rating = 10% discount
                    (if (>= (get average-rating rep-data) u200) u100 ;; Poor rating = no discount
                    u120)))  ;; Very poor rating = 20% surcharge
                    u110) ;; No rating = 10% surcharge
                u110)) ;; No reputation data = 10% surcharge
            (risk-factor (if (> (get failed-projects risk-profile) u0)
                (+ u100 (* (get failed-projects risk-profile) u10)) ;; 10% surcharge per failed project
                u100))
            (policy-type-factor (if (is-eq policy-type "performance") u100 u120)) ;; Performance bonds cost 20% less
            (adjusted-rate (/ (* base-rate reputation-factor risk-factor policy-type-factor) u10000))
        )
            (/ (* coverage-amount adjusted-rate) u100)
        )
    )
)

;; Purchase insurance policy for a tender
(define-public (purchase-insurance (tender-id uint) (policy-type (string-ascii 20)) (coverage-amount uint) (duration-blocks uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (policy-id (+ (var-get policy-counter) u1))
        (premium (calculate-premium tx-sender coverage-amount policy-type))
        (expiry-block (+ stacks-block-height duration-blocks))
    )
        ;; Validate inputs
        (asserts! (or (is-eq policy-type "performance") (is-eq policy-type "completion")) (err ERR-INVALID-POLICY-TYPE))
        (asserts! (and (> coverage-amount u0) (<= coverage-amount (* (get minimum-bid tender) u5))) (err ERR-INVALID-COVERAGE-AMOUNT))
        (asserts! (> duration-blocks u144) (err ERR-INSURANCE-EXPIRED)) ;; Minimum 1 day
        
        ;; Transfer premium to insurance pool
        (unwrap! (stx-transfer? premium tx-sender (as-contract tx-sender)) (err ERR-INSUFFICIENT-PREMIUM))
        
        ;; Create insurance policy
        (map-set insurance-policies
            { policy-id: policy-id }
            {
                tender-id: tender-id,
                policyholder: tx-sender,
                policy-type: policy-type,
                coverage-amount: coverage-amount,
                premium-paid: premium,
                start-block: stacks-block-height,
                expiry-block: expiry-block,
                active: true,
                claims-count: u0
            }
        )
        
        ;; Update insurance pool balance
        (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) premium))
        
        ;; Update user risk profile
        (unwrap! (update-user-risk-profile tx-sender) (err ERR-CLAIM-PROCESSING-FAILED))
        
        ;; Update counter
        (var-set policy-counter policy-id)
        
        (ok policy-id)
    )
)

;; Submit insurance claim
(define-public (submit-claim (policy-id uint) (claim-amount uint) (claim-reason (string-ascii 300)) (evidence-hash (string-ascii 64)))
    (let (
        (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) (err ERR-INSURANCE-NOT-FOUND)))
        (claim-id (+ (var-get claim-counter) u1))
        (tender (unwrap! (get-tender (get tender-id policy)) (err ERR-NOT-FOUND)))
    )
        ;; Validate claim eligibility
        (asserts! (is-eq tx-sender (get policyholder policy)) (err ERR-NOT-AUTHORIZED))
        (asserts! (get active policy) (err ERR-INSURANCE-EXPIRED))
        (asserts! (< stacks-block-height (get expiry-block policy)) (err ERR-INSURANCE-EXPIRED))
        (asserts! (<= claim-amount (get coverage-amount policy)) (err ERR-INVALID-COVERAGE-AMOUNT))
        
        ;; Validate claim conditions based on policy type
        (if (is-eq (get policy-type policy) "performance")
            ;; Performance bond claims require tender to be completed or cancelled
            (asserts! (or (is-eq (get status tender) "completed") (is-eq (get status tender) "cancelled")) (err ERR-CLAIM-NOT-ELIGIBLE))
            ;; Completion insurance claims require bidder to be winner and project incomplete
            (begin
                (asserts! (is-some (get winner tender)) (err ERR-CLAIM-NOT-ELIGIBLE))
                (asserts! (is-eq tx-sender (unwrap-panic (get winner tender))) (err ERR-NOT-AUTHORIZED))
                (asserts! (not (is-eq (get status tender) "completed")) (err ERR-CLAIM-NOT-ELIGIBLE))
            )
        )
        
        ;; Create claim record
        (map-set insurance-claims
            { claim-id: claim-id }
            {
                policy-id: policy-id,
                claimant: tx-sender,
                claim-amount: claim-amount,
                claim-reason: claim-reason,
                submitted-at: stacks-block-height,
                status: "pending",
                evidence-hash: (some evidence-hash),
                processed-at: none,
                payout-amount: none
            }
        )
        
        ;; Update policy claims count
        (map-set insurance-policies
            { policy-id: policy-id }
            (merge policy { claims-count: (+ (get claims-count policy) u1) })
        )
        
        ;; Update counter
        (var-set claim-counter claim-id)
        
        (ok claim-id)
    )
)

;; Process insurance claim (automated based on conditions)
(define-public (process-claim (claim-id uint) (approve bool) (payout-percentage uint))
    (let (
        (claim (unwrap! (map-get? insurance-claims { claim-id: claim-id }) (err ERR-INSURANCE-NOT-FOUND)))
        (policy (unwrap! (map-get? insurance-policies { policy-id: (get policy-id claim) }) (err ERR-INSURANCE-NOT-FOUND)))
        (tender (unwrap! (get-tender (get tender-id policy)) (err ERR-NOT-FOUND)))
        (processing-fee-amount (/ (* (get claim-amount claim) (var-get claim-processing-fee)) u100))
        (payout-amount (if approve 
            (- (/ (* (get claim-amount claim) payout-percentage) u100) processing-fee-amount)
            u0))
    )
        ;; Only tender owner or policyholder can process certain claims
        (asserts! (or (is-eq tx-sender (get owner tender)) (is-eq tx-sender (get policyholder policy))) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status claim) "pending") (err ERR-CLAIM-ALREADY-EXISTS))
        (asserts! (<= payout-percentage u100) (err ERR-INVALID-COVERAGE-AMOUNT))
        (asserts! (<= payout-amount (var-get insurance-pool-balance)) (err ERR-INSUFFICIENT-ESCROW))
        
        ;; Update claim status
        (map-set insurance-claims
            { claim-id: claim-id }
            (merge claim {
                status: (if approve "approved" "denied"),
                processed-at: (some stacks-block-height),
                payout-amount: (if approve (some payout-amount) none)
            })
        )
        
        ;; Process payout if approved
        (if approve
            (begin
                (unwrap! (as-contract (stx-transfer? payout-amount tx-sender (get claimant claim))) (err ERR-CLAIM-PROCESSING-FAILED))
                (var-set insurance-pool-balance (- (var-get insurance-pool-balance) payout-amount))
                (unwrap! (update-user-risk-profile-after-claim (get claimant claim) approve) (err ERR-CLAIM-PROCESSING-FAILED))
            )
            (unwrap! (update-user-risk-profile-after-claim (get claimant claim) approve) (err ERR-CLAIM-PROCESSING-FAILED))
        )
        
        (ok payout-amount)
    )
)

;; Update user risk profile
(define-private (update-user-risk-profile (user principal))
    (let (
        (current-profile (default-to 
            { risk-score: u50, total-policies: u0, successful-completions: u0, failed-projects: u0, total-claims: u0, last-updated: u0 }
            (map-get? user-risk-profiles { user: user })
        ))
        (user-reputation (get-user-reputation user))
    )
        (map-set user-risk-profiles
            { user: user }
            (merge current-profile {
                total-policies: (+ (get total-policies current-profile) u1),
                risk-score: (match user-reputation
                    rep-data (if (>= (get average-rating rep-data) u400) u30  ;; Excellent rating
                             (if (>= (get average-rating rep-data) u300) u40  ;; Good rating
                             (if (>= (get average-rating rep-data) u200) u60  ;; Average rating
                             u80)))  ;; Poor rating
                    u70), ;; No reputation
                last-updated: stacks-block-height
            })
        )
        (ok true)
    )
)

;; Update risk profile after claim processing
(define-private (update-user-risk-profile-after-claim (user principal) (claim-approved bool))
    (let (
        (current-profile (default-to 
            { risk-score: u50, total-policies: u0, successful-completions: u0, failed-projects: u0, total-claims: u0, last-updated: u0 }
            (map-get? user-risk-profiles { user: user })
        ))
    )
        (map-set user-risk-profiles
            { user: user }
            (merge current-profile {
                total-claims: (+ (get total-claims current-profile) u1),
                failed-projects: (if claim-approved (+ (get failed-projects current-profile) u1) (get failed-projects current-profile)),
                risk-score: (if claim-approved 
                    (if (> (+ (get risk-score current-profile) u10) u100) u100 (+ (get risk-score current-profile) u10))  ;; Increase risk score
                    (if (< (- (get risk-score current-profile) u5) u10) u10 (- (get risk-score current-profile) u5))),   ;; Decrease risk score slightly
                last-updated: stacks-block-height
            })
        )
        (ok true)
    )
)

;; Read-only functions for insurance system
(define-read-only (get-insurance-policy (policy-id uint))
    (map-get? insurance-policies { policy-id: policy-id })
)

(define-read-only (get-insurance-claim (claim-id uint))
    (map-get? insurance-claims { claim-id: claim-id })
)

(define-read-only (get-user-risk-profile (user principal))
    (map-get? user-risk-profiles { user: user })
)

(define-read-only (get-insurance-pool-balance)
    (var-get insurance-pool-balance)
)

(define-read-only (calculate-insurance-premium (user principal) (coverage-amount uint) (policy-type (string-ascii 20)))
    (calculate-premium user coverage-amount policy-type)
)

;; Check if user is eligible for insurance based on risk profile
(define-read-only (is-insurance-eligible (user principal) (coverage-amount uint))
    (let (
        (risk-profile (default-to 
            { risk-score: u50, total-policies: u0, successful-completions: u0, failed-projects: u0, total-claims: u0, last-updated: u0 }
            (map-get? user-risk-profiles { user: user })
        ))
        (max-coverage (/ (* coverage-amount u200) (get risk-score risk-profile))) ;; Higher risk = lower max coverage
    )
        (and 
            (< (get risk-score risk-profile) u90)  ;; Risk score must be below 90
            (< (get failed-projects risk-profile) u5)  ;; Less than 5 failed projects
            (>= max-coverage coverage-amount)  ;; Coverage amount within limits
        )
    )
)

;; =====================================
;; TENDER AMENDMENT SYSTEM
;; =====================================

;; Amendment System Constants
(define-constant ERR-AMENDMENT-LIMIT-EXCEEDED (err u401))
(define-constant ERR-INVALID-AMENDMENT-TYPE (err u402))
(define-constant ERR-AMENDMENT-NOT-ALLOWED (err u403))
(define-constant ERR-DEADLINE-TOO-SHORT (err u404))
(define-constant ERR-BID-DECREASE-NOT-ALLOWED (err u405))

;; Amendment system variables
(define-data-var max-amendments-per-tender uint u3)
(define-data-var amendment-counter uint u0)
(define-data-var min-deadline-extension uint u144) ;; Minimum 1 day extension

;; Track tender amendments
(define-map tender-amendments
    { tender-id: uint }
    {
        amendment-count: uint,
        last-amended-at: uint,
        major-amendments: uint  ;; Amendments that affect bidding conditions
    }
)

;; Store amendment history
(define-map amendment-history
    { amendment-id: uint }
    {
        tender-id: uint,
        amendment-type: (string-ascii 20), ;; "deadline", "description", "min-bid", "category"
        old-value: (string-ascii 200),
        new-value: (string-ascii 200),
        reason: (string-ascii 300),
        amended-by: principal,
        amended-at: uint,
        bidder-notification-sent: bool
    }
)

;; Track bidder acknowledgments of amendments
(define-map amendment-acknowledgments
    { tender-id: uint, bidder: principal }
    {
        last-acknowledged-amendment: uint,
        withdrawal-grace-period: uint  ;; Blocks until withdrawal is allowed
    }
)

;; Extend tender deadline
(define-public (amend-tender-deadline (tender-id uint) (new-deadline uint) (reason (string-ascii 300)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (amendment-data (default-to { amendment-count: u0, last-amended-at: u0, major-amendments: u0 }
                                   (map-get? tender-amendments { tender-id: tender-id })))
        (amendment-id (+ (var-get amendment-counter) u1))
        (current-deadline (get deadline tender))
    )
        ;; Validation checks
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        (asserts! (< (get amendment-count amendment-data) (var-get max-amendments-per-tender)) (err ERR-AMENDMENT-LIMIT-EXCEEDED))
        (asserts! (>= new-deadline (+ current-deadline (var-get min-deadline-extension))) (err ERR-DEADLINE-TOO-SHORT))
        (asserts! (< stacks-block-height current-deadline) (err ERR-DEADLINE-PASSED))
        
        ;; Update tender deadline
        (map-set tenders
            { tender-id: tender-id }
            (merge tender { deadline: new-deadline })
        )
        
        ;; Record amendment
        (map-set amendment-history
            { amendment-id: amendment-id }
            {
                tender-id: tender-id,
                amendment-type: "deadline",
                old-value: (unwrap-panic (as-max-len? (int-to-ascii current-deadline) u200)),
                new-value: (unwrap-panic (as-max-len? (int-to-ascii new-deadline) u200)),
                reason: reason,
                amended-by: tx-sender,
                amended-at: stacks-block-height,
                bidder-notification-sent: true
            }
        )
        
        ;; Update amendment tracking
        (map-set tender-amendments
            { tender-id: tender-id }
            {
                amendment-count: (+ (get amendment-count amendment-data) u1),
                last-amended-at: stacks-block-height,
                major-amendments: (+ (get major-amendments amendment-data) u1)
            }
        )
        
        ;; Increment amendment counter
        (var-set amendment-counter amendment-id)
        
        (ok amendment-id)
    )
)

;; Amend tender description for clarification
(define-public (amend-tender-description (tender-id uint) (new-description (string-ascii 500)) (reason (string-ascii 300)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (amendment-data (default-to { amendment-count: u0, last-amended-at: u0, major-amendments: u0 }
                                   (map-get? tender-amendments { tender-id: tender-id })))
        (amendment-id (+ (var-get amendment-counter) u1))
    )
        ;; Validation checks
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        (asserts! (< (get amendment-count amendment-data) (var-get max-amendments-per-tender)) (err ERR-AMENDMENT-LIMIT-EXCEEDED))
        (asserts! (< stacks-block-height (get deadline tender)) (err ERR-DEADLINE-PASSED))
        
        ;; Update tender description
        (map-set tenders
            { tender-id: tender-id }
            (merge tender { description: new-description })
        )
        
        ;; Record amendment
        (map-set amendment-history
            { amendment-id: amendment-id }
            {
                tender-id: tender-id,
                amendment-type: "description",
                old-value: (unwrap-panic (as-max-len? (get description tender) u200)),
                new-value: (unwrap-panic (as-max-len? new-description u200)),
                reason: reason,
                amended-by: tx-sender,
                amended-at: stacks-block-height,
                bidder-notification-sent: true
            }
        )
        
        ;; Update amendment tracking (minor amendment - doesn't affect bidding conditions)
        (map-set tender-amendments
            { tender-id: tender-id }
            {
                amendment-count: (+ (get amendment-count amendment-data) u1),
                last-amended-at: stacks-block-height,
                major-amendments: (get major-amendments amendment-data)
            }
        )
        
        ;; Increment amendment counter
        (var-set amendment-counter amendment-id)
        
        (ok amendment-id)
    )
)

;; Allow bidders to withdraw within grace period after major amendments
(define-public (withdraw-after-amendment (tender-id uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (bid (unwrap! (get-bid tender-id tx-sender) (err ERR-NO-BID-EXISTS)))
        (amendment-data (unwrap! (map-get? tender-amendments { tender-id: tender-id }) (err ERR-AMENDMENT-LIMIT-EXCEEDED)))
        (acknowledgment (map-get? amendment-acknowledgments { tender-id: tender-id, bidder: tx-sender }))
        (deposit (map-get? bid-deposits { tender-id: tender-id, bidder: tx-sender }))
    )
        ;; Check if there have been major amendments
        (asserts! (> (get major-amendments amendment-data) u0) (err ERR-AMENDMENT-NOT-ALLOWED))
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        
        ;; Check if within grace period (72 blocks = ~12 hours)
        (asserts! (< (- stacks-block-height (get last-amended-at amendment-data)) u72) (err ERR-DEADLINE-PASSED))
        
        ;; Remove bid
        (map-delete bids { tender-id: tender-id, bidder: tx-sender })
        
        ;; Refund deposit if exists
        (match deposit
            deposit-data
            (begin
                (unwrap! (as-contract (stx-transfer? (get amount deposit-data) tx-sender tx-sender)) (err ERR-REFUND-FAILED))
                (map-set bid-deposits
                    { tender-id: tender-id, bidder: tx-sender }
                    (merge deposit-data { refunded: true })
                )
            )
            true
        )
        
        (ok true)
    )
)

;; Acknowledge amendments (bidders can explicitly acknowledge to show they agree)
(define-public (acknowledge-amendments (tender-id uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (bid (unwrap! (get-bid tender-id tx-sender) (err ERR-NO-BID-EXISTS)))
        (amendment-data (unwrap! (map-get? tender-amendments { tender-id: tender-id }) (err ERR-AMENDMENT-LIMIT-EXCEEDED)))
    )
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        (asserts! (> (get amendment-count amendment-data) u0) (err ERR-AMENDMENT-NOT_ALLOWED))
        
        (map-set amendment-acknowledgments
            { tender-id: tender-id, bidder: tx-sender }
            {
                last-acknowledged-amendment: (get amendment-count amendment-data),
                withdrawal-grace-period: (+ stacks-block-height u72)
            }
        )
        
        (ok true)
    )
)

;; Read-only functions for amendment system
(define-read-only (get-tender-amendments (tender-id uint))
    (map-get? tender-amendments { tender-id: tender-id })
)

(define-read-only (get-amendment-history (amendment-id uint))
    (map-get? amendment-history { amendment-id: amendment-id })
)

(define-read-only (get-bidder-acknowledgment (tender-id uint) (bidder principal))
    (map-get? amendment-acknowledgments { tender-id: tender-id, bidder: bidder })
)

;; Get all amendments for a tender (returns list of amendment IDs)
(define-read-only (get-tender-amendment-ids (tender-id uint))
    (match (map-get? tender-amendments { tender-id: tender-id })
        amendment-data (some (get amendment-count amendment-data))
        none
    )
)

;; Check if bidder can withdraw due to amendments
(define-read-only (can-withdraw-after-amendment (tender-id uint) (bidder principal))
    (match (map-get? tender-amendments { tender-id: tender-id })
        amendment-data
        (and 
            (> (get major-amendments amendment-data) u0)
            (< (- stacks-block-height (get last-amended-at amendment-data)) u72)
            (is-some (get-bid tender-id bidder))
        )
        false
    )
)


