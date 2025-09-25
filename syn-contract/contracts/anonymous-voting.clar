;; Basic Anonymous Voting Contract
;; Implements simple nullifier-based anonymous voting

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-INVALID-PROPOSAL (err u402))
(define-constant ERR-VOTING-NOT-ACTIVE (err u403))
(define-constant ERR-NULLIFIER-ALREADY-USED (err u404))
(define-constant ERR-INVALID-VOTE-OPTION (err u405))
(define-constant ERR-VOTING-ENDED (err u406))

;; Vote options
(define-constant VOTE-YES u1)
(define-constant VOTE-NO u2)
(define-constant VOTE-ABSTAIN u3)

;; Data Variables
(define-data-var proposal-counter uint u0)

;; Simple anonymous proposals
(define-map proposals uint {
    title: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    voting-end: uint,
    total-votes: uint,
    yes-votes: uint,
    no-votes: uint,
    abstain-votes: uint,
    is-active: bool
})

;; Nullifiers to prevent double voting
(define-map used-nullifiers { proposal-id: uint, nullifier: (buff 32) } bool)

;; Anonymous votes
(define-map anonymous-votes { proposal-id: uint, vote-id: uint } {
    nullifier: (buff 32),
    vote-option: uint,
    cast-at: uint
})

;; Vote counter per proposal
(define-map vote-counters uint uint)

;; Helper function to convert uint to buffer
(define-private (uint-to-buff (value uint))
    (if (is-eq value u0)
        0x0000000000000000
        (hash160 (concat 0x75696e743a (if (< value u1000000) 0x01 0x02)))))

;; Read-only functions

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-proposal-count)
    (var-get proposal-counter)
)

(define-read-only (is-nullifier-used (proposal-id uint) (nullifier (buff 32)))
    (default-to false (map-get? used-nullifiers { proposal-id: proposal-id, nullifier: nullifier }))
)

(define-read-only (is-voting-active (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal (and (get is-active proposal) (< block-height (get voting-end proposal)))
        false)
)

;; Generate nullifier from secret and proposal (prevents double voting)
(define-read-only (generate-nullifier (secret (buff 32)) (proposal-id uint))
    (hash160 (concat secret (uint-to-buff proposal-id)))
)

;; Basic proof verification (simplified)
(define-private (verify-basic-proof (nullifier (buff 32)) (secret (buff 32)) (proposal-id uint))
    ;; Verify that the nullifier was generated correctly from the secret
    (is-eq nullifier (generate-nullifier secret proposal-id))
)

;; Public Functions

;; Create anonymous proposal
(define-public (create-proposal 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (voting-duration uint))
    (let ((proposal-id (+ (var-get proposal-counter) u1))
          (voting-end (+ block-height voting-duration)))
        
        (map-set proposals proposal-id {
            title: title,
            description: description,
            creator: tx-sender,
            voting-end: voting-end,
            total-votes: u0,
            yes-votes: u0,
            no-votes: u0,
            abstain-votes: u0,
            is-active: true
        })
        
        (var-set proposal-counter proposal-id)
        (map-set vote-counters proposal-id u0)
        (ok proposal-id))
)

;; Cast anonymous vote
(define-public (cast-anonymous-vote 
    (proposal-id uint)
    (nullifier (buff 32))
    (vote-option uint)
    (secret (buff 32)))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
          (vote-counter (default-to u0 (map-get? vote-counters proposal-id))))
        
        ;; Validate voting is active
        (asserts! (is-voting-active proposal-id) ERR-VOTING-NOT-ACTIVE)
        
        ;; Validate vote option
        (asserts! (or (is-eq vote-option VOTE-YES) 
                     (or (is-eq vote-option VOTE-NO) 
                         (is-eq vote-option VOTE-ABSTAIN))) ERR-INVALID-VOTE-OPTION)
        
        ;; Check nullifier hasn't been used (prevents double voting)
        (asserts! (not (is-nullifier-used proposal-id nullifier)) ERR-NULLIFIER-ALREADY-USED)
        
        ;; Verify basic proof (that nullifier was generated correctly)
        (asserts! (verify-basic-proof nullifier secret proposal-id) ERR-NOT-AUTHORIZED)
        
        ;; Mark nullifier as used
        (map-set used-nullifiers { proposal-id: proposal-id, nullifier: nullifier } true)
        
        ;; Store anonymous vote
        (map-set anonymous-votes 
            { proposal-id: proposal-id, vote-id: (+ vote-counter u1) }
            {
                nullifier: nullifier,
                vote-option: vote-option,
                cast-at: block-height
            })
        
        ;; Update vote counters
        (map-set vote-counters proposal-id (+ vote-counter u1))
        
        ;; Update proposal vote tallies
        (let ((updated-proposal 
                (merge proposal { 
                    total-votes: (+ (get total-votes proposal) u1),
                    yes-votes: (if (is-eq vote-option VOTE-YES) (+ (get yes-votes proposal) u1) (get yes-votes proposal)),
                    no-votes: (if (is-eq vote-option VOTE-NO) (+ (get no-votes proposal) u1) (get no-votes proposal)),
                    abstain-votes: (if (is-eq vote-option VOTE-ABSTAIN) (+ (get abstain-votes proposal) u1) (get abstain-votes proposal))
                })))
            (map-set proposals proposal-id updated-proposal))
        
        (ok (+ vote-counter u1)))
)

;; Get voting results
(define-read-only (get-voting-results (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal 
            (ok {
                total-votes: (get total-votes proposal),
                yes-votes: (get yes-votes proposal),
                no-votes: (get no-votes proposal),
                abstain-votes: (get abstain-votes proposal),
                is-ended: (>= block-height (get voting-end proposal))
            })
        ERR-INVALID-PROPOSAL)
)

;; End proposal (creator only)
(define-public (end-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL)))
        (asserts! (is-eq tx-sender (get creator proposal)) ERR-NOT-AUTHORIZED)
        (map-set proposals proposal-id 
            (merge proposal { is-active: false }))
        (ok true))
)

;; Emergency end (admin only)
(define-public (emergency-end-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL)))
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set proposals proposal-id 
            (merge proposal { is-active: false }))
        (ok true))
)