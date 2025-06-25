;; Governance & Security Referendum Contract
;; Implements admin controls, parameter management, emergency stops, and proposals

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INVALID-PARAMS (err u102))
(define-constant ERR-REFERENDUM-ACTIVE (err u103))
(define-constant ERR-REFERENDUM-ENDED (err u104))
(define-constant ERR-EMERGENCY-STOP (err u105))
(define-constant ERR-ALREADY-VOTED (err u106))
(define-constant ERR-PROPOSAL-EXISTS (err u107))

;; Data Variables
(define-data-var emergency-stop bool false)
(define-data-var referendum-counter uint u0)
(define-data-var proposal-counter uint u0)

;; Default voting parameters
(define-data-var default-voting-period uint u1008) ;; ~1 week in blocks
(define-data-var default-quorum uint u1000) ;; 10% (basis points)
(define-data-var min-proposal-threshold uint u100) ;; Minimum tokens to propose

;; Admin Management
(define-map admins principal bool)
(define-map admin-permissions principal (list 10 (string-ascii 50)))

;; Referendum Data Structure
(define-map referendums uint {
    title: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    start-block: uint,
    end-block: uint,
    voting-period: uint,
    quorum: uint,
    yes-votes: uint,
    no-votes: uint,
    total-voters: uint,
    status: (string-ascii 20), ;; "active", "ended", "cancelled"
    emergency-stopped: bool
})

;; Voting Records
(define-map votes {referendum-id: uint, voter: principal} {
    vote: bool, ;; true for yes, false for no
    stacks-block-height: uint,
    voting-power: uint
})

;; Proposals for System Changes
(define-map proposals uint {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    proposal-type: (string-ascii 30), ;; "parameter-change", "admin-change", "system-upgrade"
    target-parameter: (string-ascii 50),
    new-value: uint,
    votes-for: uint,
    votes-against: uint,
    status: (string-ascii 20), ;; "pending", "approved", "rejected", "executed"
    created-at: uint,
    voting-deadline: uint
})

;; Proposal Votes
(define-map proposal-votes {proposal-id: uint, voter: principal} bool)

;; Initialize contract with owner as admin
(map-set admins CONTRACT-OWNER true)
(map-set admin-permissions CONTRACT-OWNER 
    (list "create-referendum" "end-referendum" "emergency-stop" "manage-admins" "change-parameters"))

;; Admin Management Functions
(define-public (add-admin (new-admin principal) (permissions (list 10 (string-ascii 50))))
    (begin
        (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
        (asserts! (has-permission tx-sender "manage-admins") ERR-UNAUTHORIZED)
        (map-set admins new-admin true)
        (map-set admin-permissions new-admin permissions)
        (ok true)
    )
)

(define-public (remove-admin (admin principal))
    (begin
        (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
        (asserts! (has-permission tx-sender "manage-admins") ERR-UNAUTHORIZED)
        (asserts! (not (is-eq admin CONTRACT-OWNER)) ERR-UNAUTHORIZED) ;; Cannot remove contract owner
        (map-delete admins admin)
        (map-delete admin-permissions admin)
        (ok true)
    )
)

;; Referendum Management Functions
(define-public (create-referendum 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (voting-period uint)
    (quorum uint))
    (let (
        (referendum-id (+ (var-get referendum-counter) u1))
        (start-block stacks-block-height)
        (end-block (+ stacks-block-height voting-period))
    )
        (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
        (asserts! (has-permission tx-sender "create-referendum") ERR-UNAUTHORIZED)
        (asserts! (not (var-get emergency-stop)) ERR-EMERGENCY-STOP)
        (asserts! (> voting-period u0) ERR-INVALID-PARAMS)
        (asserts! (<= quorum u10000) ERR-INVALID-PARAMS) ;; Max 100%
        
        (map-set referendums referendum-id {
            title: title,
            description: description,
            creator: tx-sender,
            start-block: start-block,
            end-block: end-block,
            voting-period: voting-period,
            quorum: quorum,
            yes-votes: u0,
            no-votes: u0,
            total-voters: u0,
            status: "active",
            emergency-stopped: false
        })
        
        (var-set referendum-counter referendum-id)
        (ok referendum-id)
    )
)

(define-public (end-referendum (referendum-id uint))
    (let (
        (referendum (unwrap! (map-get? referendums referendum-id) ERR-NOT-FOUND))
    )
        (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
        (asserts! (has-permission tx-sender "end-referendum") ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status referendum) "active") ERR-REFERENDUM-ENDED)
        
        (map-set referendums referendum-id 
            (merge referendum {status: "ended"}))
        (ok true)
    )
)

;; Emergency Stop Functions
(define-public (activate-emergency-stop)
    (begin
        (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
        (asserts! (has-permission tx-sender "emergency-stop") ERR-UNAUTHORIZED)
        (var-set emergency-stop true)
        (ok true)
    )
)

(define-public (deactivate-emergency-stop)
    (begin
        (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
        (asserts! (has-permission tx-sender "emergency-stop") ERR-UNAUTHORIZED)
        (var-set emergency-stop false)
        (ok true)
    )
)

(define-public (emergency-stop-referendum (referendum-id uint))
    (let (
        (referendum (unwrap! (map-get? referendums referendum-id) ERR-NOT-FOUND))
    )
        (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
        (asserts! (has-permission tx-sender "emergency-stop") ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status referendum) "active") ERR-REFERENDUM-ENDED)
        
        (map-set referendums referendum-id 
            (merge referendum {
                status: "cancelled",
                emergency-stopped: true
            }))
        (ok true)
    )
)

;; Parameter Management Functions
(define-public (change-voting-parameters 
    (new-voting-period uint)
    (new-quorum uint)
    (new-proposal-threshold uint))
    (begin
        (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
        (asserts! (has-permission tx-sender "change-parameters") ERR-UNAUTHORIZED)
        (asserts! (> new-voting-period u0) ERR-INVALID-PARAMS)
        (asserts! (<= new-quorum u10000) ERR-INVALID-PARAMS)
        (asserts! (> new-proposal-threshold u0) ERR-INVALID-PARAMS)
        
        (var-set default-voting-period new-voting-period)
        (var-set default-quorum new-quorum)
        (var-set min-proposal-threshold new-proposal-threshold)
        (ok true)
    )
)

;; Voting Functions
(define-public (vote (referendum-id uint) (vote-choice bool) (voting-power uint))
    (let (
        (referendum (unwrap! (map-get? referendums referendum-id) ERR-NOT-FOUND))
        (vote-key {referendum-id: referendum-id, voter: tx-sender})
    )
        (asserts! (not (var-get emergency-stop)) ERR-EMERGENCY-STOP)
        (asserts! (is-eq (get status referendum) "active") ERR-REFERENDUM-ENDED)
        (asserts! (not (get emergency-stopped referendum)) ERR-EMERGENCY-STOP)
        (asserts! (<= stacks-block-height (get end-block referendum)) ERR-REFERENDUM-ENDED)
        (asserts! (is-none (map-get? votes vote-key)) ERR-ALREADY-VOTED)
        
        ;; Record the vote
        (map-set votes vote-key {
            vote: vote-choice,
            stacks-block-height: stacks-block-height,
            voting-power: voting-power
        })
        
        ;; Update referendum totals
        (map-set referendums referendum-id 
            (merge referendum {
                yes-votes: (if vote-choice 
                    (+ (get yes-votes referendum) voting-power)
                    (get yes-votes referendum)),
                no-votes: (if vote-choice 
                    (get no-votes referendum)
                    (+ (get no-votes referendum) voting-power)),
                total-voters: (+ (get total-voters referendum) u1)
            }))
        
        (ok true)
    )
)

;; Proposal System Functions
(define-public (create-proposal 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (proposal-type (string-ascii 30))
    (target-parameter (string-ascii 50))
    (new-value uint))
    (let (
        (proposal-id (+ (var-get proposal-counter) u1))
        (voting-deadline (+ stacks-block-height (var-get default-voting-period)))
    )
        (asserts! (not (var-get emergency-stop)) ERR-EMERGENCY-STOP)
        ;; Add token balance check here if needed
        
        (map-set proposals proposal-id {
            title: title,
            description: description,
            proposer: tx-sender,
            proposal-type: proposal-type,
            target-parameter: target-parameter,
            new-value: new-value,
            votes-for: u0,
            votes-against: u0,
            status: "pending",
            created-at: stacks-block-height,
            voting-deadline: voting-deadline
        })
        
        (var-set proposal-counter proposal-id)
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal (proposal-id uint) (support bool))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-NOT-FOUND))
        (vote-key {proposal-id: proposal-id, voter: tx-sender})
    )
        (asserts! (not (var-get emergency-stop)) ERR-EMERGENCY-STOP)
        (asserts! (is-eq (get status proposal) "pending") ERR-INVALID-PARAMS)
        (asserts! (<= stacks-block-height (get voting-deadline proposal)) ERR-REFERENDUM-ENDED)
        (asserts! (is-none (map-get? proposal-votes vote-key)) ERR-ALREADY-VOTED)
        
        (map-set proposal-votes vote-key support)
        
        (map-set proposals proposal-id 
            (merge proposal {
                votes-for: (if support 
                    (+ (get votes-for proposal) u1)
                    (get votes-for proposal)),
                votes-against: (if support 
                    (get votes-against proposal)
                    (+ (get votes-against proposal) u1))
            }))
        
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-NOT-FOUND))
    )
        (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status proposal) "pending") ERR-INVALID-PARAMS)
        (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR-INVALID-PARAMS)
        
        ;; Execute the proposal based on type
        (if (is-eq (get proposal-type proposal) "parameter-change")
            (begin
                (unwrap! (execute-parameter-change proposal) ERR-INVALID-PARAMS)
                (map-set proposals proposal-id 
                    (merge proposal {status: "executed"}))
                (ok true)
            )
            ;; Handle other proposal types here
            (begin
                ;; For now, just mark as executed for other types
                (map-set proposals proposal-id 
                    (merge proposal {status: "executed"}))
                (ok true)
            )
        )
    )
)

;; Helper Functions
(define-private (is-admin (user principal))
    (default-to false (map-get? admins user))
)

(define-private (has-permission (user principal) (permission (string-ascii 50)))
    (let (
        (user-permissions (default-to (list) (map-get? admin-permissions user)))
    )
        (is-some (index-of user-permissions permission))
    )
)

(define-private (execute-parameter-change (proposal {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    proposal-type: (string-ascii 30),
    target-parameter: (string-ascii 50),
    new-value: uint,
    votes-for: uint,
    votes-against: uint,
    status: (string-ascii 20),
    created-at: uint,
    voting-deadline: uint
}))
    (begin
        (if (is-eq (get target-parameter proposal) "voting-period")
            (var-set default-voting-period (get new-value proposal))
            (if (is-eq (get target-parameter proposal) "quorum")
                (var-set default-quorum (get new-value proposal))
                (var-set min-proposal-threshold (get new-value proposal))))
        (ok true)
    )
)

;; Read-only Functions
(define-read-only (get-referendum (referendum-id uint))
    (map-get? referendums referendum-id)
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-vote (referendum-id uint) (voter principal))
    (map-get? votes {referendum-id: referendum-id, voter: voter})
)

(define-read-only (get-proposal-vote (proposal-id uint) (voter principal))
    (map-get? proposal-votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (is-emergency-stopped)
    (var-get emergency-stop)
)

(define-read-only (get-voting-parameters)
    {
        voting-period: (var-get default-voting-period),
        quorum: (var-get default-quorum),
        proposal-threshold: (var-get min-proposal-threshold)
    }
)

(define-read-only (get-admin-status (user principal))
    {
        is-admin: (is-admin user),
        permissions: (default-to (list) (map-get? admin-permissions user))
    }
)
