;; Referendum System - Transparency & Auditability Smart Contract
;; This contract allows for creating referendums, voting, and maintaining an auditable trail
;; while preserving voter privacy through cryptographic techniques

;; Define data maps and variables
(define-map referendums
  { referendum-id: uint }
  {
    title: (string-utf8 256),
    description: (string-utf8 1024),
    start-block: uint,
    end-block: uint,
    finalized: bool,
    total-votes: uint,
    yes-votes: uint,
    no-votes: uint
  }
)

;; Store votes with cryptographic proofs
;; The vote-hash is a hash of the voter's address and a nonce to preserve anonymity
;; The vote-proof is a cryptographic proof that the vote is valid without revealing the voter
(define-map votes
  { referendum-id: uint, vote-hash: (buff 32) }
  {
    choice: bool,  ;; true for yes, false for no
    vote-proof: (buff 64),
    vote-block: uint
  }
)

;; Track which referendums a voter has participated in without revealing their vote
;; This allows for audit trails while preserving privacy
(define-map voter-participation
  { voter: principal, referendum-id: uint }
  { participated: bool }
)

;; Counter for referendum IDs
(define-data-var referendum-counter uint u0)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED u1)
(define-constant ERR-REFERENDUM-NOT-FOUND u2)
(define-constant ERR-REFERENDUM-ENDED u3)
(define-constant ERR-REFERENDUM-NOT-ENDED u4)
(define-constant ERR-ALREADY-VOTED u5)
(define-constant ERR-INVALID-PROOF u6)

;; Create a new referendum
(define-public (create-referendum (title (string-utf8 256)) 
                                 (description (string-utf8 1024))
                                 (duration uint))
  (let ((referendum-id (var-get referendum-counter))
        (start-block stacks-block-height)
        (end-block (+ stacks-block-height duration)))
    
    ;; Only contract owner can create referendums (can be modified for governance)
    (asserts! (is-eq tx-sender contract-owner) (err ERR-NOT-AUTHORIZED))
    
    ;; Store the referendum details
    (map-set referendums
      { referendum-id: referendum-id }
      {
        title: title,
        description: description,
        start-block: start-block,
        end-block: end-block,
        finalized: false,
        total-votes: u0,
        yes-votes: u0,
        no-votes: u0
      }
    )
    
    ;; Increment the referendum counter
    (var-set referendum-counter (+ referendum-id u1))
    
    ;; Return the referendum ID
    (ok referendum-id)
  )
)

;; Cast a vote in a referendum
;; vote-hash is a hash of the voter's address and a nonce to preserve anonymity
;; vote-proof is a cryptographic proof that the vote is valid without revealing the voter
(define-public (cast-vote (referendum-id uint) 
                         (choice bool) 
                         (vote-hash (buff 32))
                         (vote-proof (buff 64)))
  (let ((referendum (unwrap! (map-get? referendums { referendum-id: referendum-id }) 
                            (err ERR-REFERENDUM-NOT-FOUND)))
        (current-block stacks-block-height))
    
    ;; Check if referendum is still active
    (asserts! (<= current-block (get end-block referendum)) (err ERR-REFERENDUM-ENDED))
    
    ;; Check if voter has already participated
    (asserts! (is-none (map-get? voter-participation { voter: tx-sender, referendum-id: referendum-id }))
              (err ERR-ALREADY-VOTED))
    
    ;; Verify the vote proof (this would be implemented with a proper zk-proof verification)
    ;; For simplicity, we're just checking that the proof is not empty
    (asserts! (> (len vote-proof) u0) (err ERR-INVALID-PROOF))
    
    ;; Record the vote
    (map-set votes
      { referendum-id: referendum-id, vote-hash: vote-hash }
      {
        choice: choice,
        vote-proof: vote-proof,
        vote-block: current-block
      }
    )
    
    ;; Record participation without revealing the vote
    (map-set voter-participation
      { voter: tx-sender, referendum-id: referendum-id }
      { participated: true }
    )
    
    ;; Update referendum vote counts
    (map-set referendums
      { referendum-id: referendum-id }
      (merge referendum {
        total-votes: (+ (get total-votes referendum) u1),
        yes-votes: (+ (get yes-votes referendum) (if choice u1 u0)),
        no-votes: (+ (get no-votes referendum) (if choice u0 u1))
      })
    )
    
    (ok true)
  )
)

;; Finalize referendum results
(define-public (finalize-referendum (referendum-id uint))
  (let ((referendum (unwrap! (map-get? referendums { referendum-id: referendum-id }) 
                            (err ERR-REFERENDUM-NOT-FOUND)))
        (current-block stacks-block-height))
    
    ;; Check if referendum has ended
    (asserts! (> current-block (get end-block referendum)) (err ERR-REFERENDUM-NOT-ENDED))
    
    ;; Check if referendum is not already finalized
    (asserts! (not (get finalized referendum)) (err u7))
    
    ;; Only contract owner can finalize (can be modified for governance)
    (asserts! (is-eq tx-sender contract-owner) (err ERR-NOT-AUTHORIZED))
    
    ;; Update referendum to finalized state
    (map-set referendums
      { referendum-id: referendum-id }
      (merge referendum { finalized: true })
    )
    
    ;; Emit an event with the results (for indexers to pick up)
    (print {
      event: "referendum-finalized",
      referendum-id: referendum-id,
      total-votes: (get total-votes referendum),
      yes-votes: (get yes-votes referendum),
      no-votes: (get no-votes referendum),
      passed: (> (get yes-votes referendum) (get no-votes referendum))
    })
    
    (ok true)
  )
)

;; Read-only functions for transparency and auditability

;; Get referendum details
(define-read-only (get-referendum (referendum-id uint))
  (map-get? referendums { referendum-id: referendum-id })
)

;; Check if a voter has participated in a referendum
(define-read-only (has-participated (voter principal) (referendum-id uint))
  (default-to { participated: false }
    (map-get? voter-participation { voter: voter, referendum-id: referendum-id }))
)

;; Get vote details by hash (preserves anonymity)
(define-read-only (get-vote (referendum-id uint) (vote-hash (buff 32)))
  (map-get? votes { referendum-id: referendum-id, vote-hash: vote-hash })
)

;; Get total number of referendums
(define-read-only (get-referendum-count)
  (var-get referendum-counter)
)

;; Contract owner (could be replaced with a DAO governance mechanism)
(define-constant contract-owner tx-sender)