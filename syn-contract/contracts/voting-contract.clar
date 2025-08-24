;; title: voting-contract
;; version:
;; summary:
;; description:

;; referendum-voting.clar
;; A smart contract for managing referendum voting

;; Define data variables
(define-data-var referendum-active bool true)
(define-data-var yes-votes int 0)
(define-data-var no-votes int 0)
(define-data-var passing-threshold int 50) ;; Default 50% threshold
(define-data-var referendum-ended bool false)
(define-data-var result-hash (optional (buff 32)) none)

;; Define data maps
(define-map voters principal bool)
(define-map vote-record { voter: principal } { vote: (string-ascii 3) })

;; Define contract owner
(define-constant contract-owner tx-sender)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED u100)
(define-constant ERR-ALREADY-VOTED u101)
(define-constant ERR-REFERENDUM-ENDED u102)
(define-constant ERR-REFERENDUM-ACTIVE u103)
(define-constant ERR-INVALID-VOTE u104)
(define-constant ERR-INVALID-THRESHOLD u105)

;; Read-only functions

;; Get the current vote count
(define-read-only (get-vote-count)
  {
    yes: (var-get yes-votes),
    no: (var-get no-votes),
    total: (+ (var-get yes-votes) (var-get no-votes)),
    active: (var-get referendum-active)
  }
)

;; Get referendum results
(define-read-only (get-referendum-results)
  (let (
    (yes (var-get yes-votes))
    (no (var-get no-votes))
    (total (+ yes no))
    (threshold (var-get passing-threshold))
    (yes-percentage (if (> total 0)
                        (/ (* yes 100) total)
                        0))
  )
  {
    yes-votes: yes,
    no-votes: no,
    total-votes: total,
    yes-percentage: yes-percentage,
    threshold: threshold,
    passed: (and (not (var-get referendum-active)) 
                (>= yes-percentage threshold)),
    ended: (var-get referendum-ended),
    result-hash: (var-get result-hash)
  })
)

;; Check if an address has voted
(define-read-only (has-voted (address principal))
  (default-to false (map-get? voters address))
)

;; Get a specific voter's vote
(define-read-only (get-vote (address principal))
  (map-get? vote-record { voter: address })
)

;; Public functions

;; Cast a vote (yes or no)
(define-public (vote (choice (string-ascii 3)))
  (begin
    ;; Check if referendum is still active
    (asserts! (var-get referendum-active) (err ERR-REFERENDUM-ENDED))
    
    ;; Check if voter has already voted
    (asserts! (not (has-voted tx-sender)) (err ERR-ALREADY-VOTED))
    
    ;; Check if vote is valid (must be "yes" or "no")
    (asserts! (or (is-eq choice "yes") (is-eq choice "no")) (err ERR-INVALID-VOTE))
    
    ;; Record the vote
    (map-set voters tx-sender true)
    (map-set vote-record { voter: tx-sender } { vote: choice })
    
    ;; Update vote counts
    (if (is-eq choice "yes")
        (var-set yes-votes (+ (var-get yes-votes) 1))
        (var-set no-votes (+ (var-get no-votes) 1)))
    
    (ok true)
  )
)

;; Admin functions



;; Set passing threshold (percentage of yes votes needed)
(define-public (set-threshold (new-threshold int))
  (begin
    ;; Only contract owner can set threshold
    (asserts! (is-eq tx-sender contract-owner) (err ERR-NOT-AUTHORIZED))
    
    ;; Ensure referendum is still active
    (asserts! (var-get referendum-active) (err ERR-REFERENDUM-ACTIVE))
    
    ;; Validate threshold (must be between 1 and 100)
    (asserts! (and (> new-threshold 0) (<= new-threshold 100)) (err ERR-INVALID-THRESHOLD))
    
    ;; Set new threshold
    (var-set passing-threshold new-threshold)
    
    (ok true)
  )
)

;; Verify result authenticity
(define-read-only (authenticate-results (expected-hash (buff 32)))
  (let (
    (stored-hash (default-to 0x (var-get result-hash)))
  )
    {
      authentic: (is-eq stored-hash expected-hash),
      stored-hash: stored-hash,
      referendum-ended: (var-get referendum-ended)
    }
  )
)

;; Restart referendum (for future use)
(define-public (restart-referendum)
  (begin
    ;; Only contract owner can restart
    (asserts! (is-eq tx-sender contract-owner) (err ERR-NOT-AUTHORIZED))
    
    ;; Ensure referendum has ended
    (asserts! (var-get referendum-ended) (err ERR-REFERENDUM-ACTIVE))
    
    ;; Reset variables
    (var-set referendum-active true)
    (var-set referendum-ended false)
    (var-set yes-votes 0)
    (var-set no-votes 0)
    (var-set result-hash none)
    
    (ok true)
  )
)