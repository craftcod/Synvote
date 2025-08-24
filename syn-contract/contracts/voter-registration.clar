
;; title: voter-registration
;; version:
;; summary:
;; description:
;; Voter Registration & Management Smart Contract
;; Implements functionality for managing voter eligibility and status

;; Constants for voter status
(define-constant VOTER-ACTIVE u1)
(define-constant VOTER-INACTIVE u0)
(define-constant VOTER-BANNED u2)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-NOT-REGISTERED (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-BANNED (err u104))
(define-constant ERR-NOT-WHITELISTED (err u105))

;; Data Maps
(define-map voters
    { address: principal }
    {
        status: uint,
        kyc-verified: bool,
        registration-time: uint,
        last-vote-time: (optional uint),
        did: (optional (string-utf8 50))
    })

(define-map whitelist
    { address: principal }
    { approved: bool })

(define-map voter-votes
    { address: principal, proposal-id: uint }
    { has-voted: bool })

;; Administrative Variables
(define-data-var contract-owner principal tx-sender)
(define-data-var kyc-authority principal tx-sender)


;; Getters
(define-read-only (get-voter-info (address principal))
    (map-get? voters { address: address }))

(define-read-only (is-whitelisted (address principal))
    (default-to 
        false
        (get approved (map-get? whitelist { address: address }))))

(define-read-only (has-voted (address principal) (proposal-id uint))
    (default-to
        false
        (get has-voted (map-get? voter-votes { address: address, proposal-id: proposal-id }))))

;; Authorization check
(define-private (is-contract-owner)
    (is-eq tx-sender (var-get contract-owner)))

(define-private (is-kyc-authority)
    (is-eq tx-sender (var-get kyc-authority)))

;; Register a new voter
(define-public (register-voter (did (optional (string-utf8 50))))
    (let
        ((existing-voter (map-get? voters { address: tx-sender })))
        (asserts! (is-none existing-voter) ERR-ALREADY-REGISTERED)
        (asserts! (is-whitelisted tx-sender) ERR-NOT-WHITELISTED)
        (ok (map-set voters
            { address: tx-sender }
            {
                status: VOTER-ACTIVE,
                kyc-verified: false,
                registration-time: block-height,
                last-vote-time: none,
                did: did
            }))))

;; KYC verification
(define-public (verify-voter-kyc (voter-address principal))
    (begin
        (asserts! (is-kyc-authority) ERR-NOT-AUTHORIZED)
        (match (map-get? voters { address: voter-address })
            voter-data (ok (map-set voters
                { address: voter-address }
                (merge voter-data { kyc-verified: true })))
            ERR-NOT-REGISTERED)))

;; Deregister a voter
(define-public (deregister-voter (voter-address principal))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (match (map-get? voters { address: voter-address })
            voter-data (ok (map-set voters
                { address: voter-address }
                (merge voter-data { status: VOTER-INACTIVE })))
            ERR-NOT-REGISTERED)))

;; Ban a voter
(define-public (ban-voter (voter-address principal))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (match (map-get? voters { address: voter-address })
            voter-data (ok (map-set voters
                { address: voter-address }
                (merge voter-data { status: VOTER-BANNED })))
            ERR-NOT-REGISTERED)))

;; Whitelist management
(define-public (add-to-whitelist (voter-address principal))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (ok (map-set whitelist
            { address: voter-address }
            { approved: true }))))

(define-public (remove-from-whitelist (voter-address principal))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (ok (map-set whitelist
            { address: voter-address }
            { approved: false }))))

;; Record a vote
(define-public (record-vote (proposal-id uint))
    (let
        ((voter-info (map-get? voters { address: tx-sender })))
        (asserts! (is-some voter-info) ERR-NOT-REGISTERED)
        (asserts! (is-eq (get status (unwrap! voter-info ERR-NOT-REGISTERED)) VOTER-ACTIVE) ERR-BANNED)
        (asserts! (not (has-voted tx-sender proposal-id)) ERR-ALREADY-VOTED)
        (begin
            (map-set voter-votes
                { address: tx-sender, proposal-id: proposal-id }
                { has-voted: true })
            (map-set voters
                { address: tx-sender }
                (merge (unwrap! voter-info ERR-NOT-REGISTERED)
                    { last-vote-time: (some block-height) }))
            (ok true))))

;; Administrative functions
(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (ok (var-set contract-owner new-owner))))

(define-public (set-kyc-authority (new-authority principal))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (ok (var-set kyc-authority new-authority))))