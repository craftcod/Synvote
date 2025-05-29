;; Voting System Smart Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u200))
(define-constant err-ineligible-voter (err u201))
(define-constant err-invalid-vote (err u202))
(define-constant err-already-voted (err u203))
(define-constant err-referendum-not-active (err u204))
(define-constant err-vote-not-found (err u205))

;; Define data variables
(define-map votes
  { referendum-id: uint, voter: principal }
  { vote: bool }
)

(define-map voter-eligibility
  { voter: principal }
  { eligible: bool }
)

(define-map vote-counts
  { referendum-id: uint }
  { yes-votes: uint, no-votes: uint }
)

;; Define functions to interact with Referendum Management contract
(define-read-only (get-referendum-status (referendum-id uint))
  (contract-call? .Referendum-Management get-referendum referendum-id)
)

;; Helper functions
(define-private (is-authorized)
  (is-eq tx-sender contract-owner)
)

(define-private (is-referendum-active (referendum-id uint))
  (let ((referendum (unwrap! (get-referendum-status referendum-id) false)))
    (is-eq (get status referendum) "active")
  )
)

;; Main functions

;; Set Voter Eligibility
(define-public (set-voter-eligibility (voter principal) (is-eligible bool))
  (begin
    (asserts! (is-authorized) err-unauthorized)
    (map-set voter-eligibility
      { voter: voter }
      { eligible: is-eligible }
    )
    (ok true)
  )
)

;; Check Voting Eligibility
(define-read-only (check-voting-eligibility (voter principal))
  (default-to
    { eligible: false }
    (map-get? voter-eligibility { voter: voter })
  )
)

;; Vote
(define-public (vote (referendum-id uint) (vote-value bool))
  (let
    (
      (voter tx-sender)
      (eligibility (get eligible (check-voting-eligibility voter)))
    )
    (asserts! eligibility err-ineligible-voter)
    (asserts! (is-referendum-active referendum-id) err-referendum-not-active)
    (asserts! (is-none (map-get? votes { referendum-id: referendum-id, voter: voter })) err-already-voted)
    (map-set votes
      { referendum-id: referendum-id, voter: voter }
      { vote: vote-value }
    )
    (map-set vote-counts
      { referendum-id: referendum-id }
      (merge
        (default-to
          { yes-votes: u0, no-votes: u0 }
          (map-get? vote-counts { referendum-id: referendum-id })
        )
        {
          yes-votes: (+ (if vote-value u1 u0) (get yes-votes (default-to { yes-votes: u0, no-votes: u0 } (map-get? vote-counts { referendum-id: referendum-id })))),
          no-votes: (+ (if vote-value u0 u1) (get no-votes (default-to { yes-votes: u0, no-votes: u0 } (map-get? vote-counts { referendum-id: referendum-id }))))
        }
      )
    )
    (ok true)
  )
)

;; Vote Reversal
(define-public (reverse-vote (referendum-id uint))
  (let
    (
      (voter tx-sender)
      (current-vote (unwrap! (map-get? votes { referendum-id: referendum-id, voter: voter }) err-vote-not-found))
    )
    (asserts! (is-referendum-active referendum-id) err-referendum-not-active)
    (map-delete votes { referendum-id: referendum-id, voter: voter })
    (map-set vote-counts
      { referendum-id: referendum-id }
      (merge
        (default-to
          { yes-votes: u0, no-votes: u0 }
          (map-get? vote-counts { referendum-id: referendum-id })
        )
        {
          yes-votes: (- (get yes-votes (default-to { yes-votes: u0, no-votes: u0 } (map-get? vote-counts { referendum-id: referendum-id }))) (if (get vote current-vote) u1 u0)),
          no-votes: (- (get no-votes (default-to { yes-votes: u0, no-votes: u0 } (map-get? vote-counts { referendum-id: referendum-id }))) (if (get vote current-vote) u0 u1))
        }
      )
    )
    (ok true)
  )
)

;; Get Vote Count
(define-read-only (get-vote-count (referendum-id uint))
  (default-to
    { yes-votes: u0, no-votes: u0 }
    (map-get? vote-counts { referendum-id: referendum-id })
  )
)

;; Get Individual Vote (for voter to check their own vote)
(define-read-only (get-individual-vote (referendum-id uint))
  (map-get? votes { referendum-id: referendum-id, voter: tx-sender })
)