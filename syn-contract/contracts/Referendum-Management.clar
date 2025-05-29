;; Referendum Management Smart Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-invalid-referendum (err u101))
(define-constant err-referendum-already-started (err u102))
(define-constant err-referendum-not-started (err u103))
(define-constant err-referendum-ended (err u104))

;; Define data variables
(define-map referendums
  { referendum-id: uint }
  {
    question: (string-utf8 500),
    start-time: uint,
    end-time: uint,
    eligibility-criteria: (string-utf8 200),
    status: (string-ascii 20),
    result: (optional bool)
  }
)

(define-data-var referendum-count uint u0)

;; Helper functions
(define-private (is-authorized)
  (is-eq tx-sender contract-owner)
)

(define-read-only (get-referendum (referendum-id uint))
  (map-get? referendums { referendum-id: referendum-id })
)

;; Main functions

;; Create Referendum
(define-public (create-referendum (question (string-utf8 500)) (start-time uint) (end-time uint) (eligibility-criteria (string-utf8 200)))
  (begin
    (asserts! (is-authorized) err-unauthorized)
    (asserts! (> start-time stacks-block-height) err-invalid-referendum)
    (asserts! (> end-time start-time) err-invalid-referendum)
    (let
      (
        (new-id (+ (var-get referendum-count) u1))
      )
      (map-set referendums
        { referendum-id: new-id }
        {
          question: question,
          start-time: start-time,
          end-time: end-time,
          eligibility-criteria: eligibility-criteria,
          status: "created",
          result: none
        }
      )
      (var-set referendum-count new-id)
      (ok new-id)
    )
  )
)

;; Start Referendum
(define-public (start-referendum (referendum-id uint))
  (let
    (
      (referendum (unwrap! (get-referendum referendum-id) err-invalid-referendum))
    )
    (asserts! (is-authorized) err-unauthorized)
    (asserts! (is-eq (get status referendum) "created") err-referendum-already-started)
    (asserts! (<= (get start-time referendum) stacks-block-height) err-invalid-referendum)
    (map-set referendums
      { referendum-id: referendum-id }
      (merge referendum { status: "active" })
    )
    (ok true)
  )
)

;; End Referendum
(define-public (end-referendum (referendum-id uint) (result bool))
  (let
    (
      (referendum (unwrap! (get-referendum referendum-id) err-invalid-referendum))
    )
    (asserts! (is-authorized) err-unauthorized)
    (asserts! (is-eq (get status referendum) "active") err-referendum-not-started)
    (asserts! (>= stacks-block-height (get end-time referendum)) err-referendum-ended)
    (map-set referendums
      { referendum-id: referendum-id }
      (merge referendum { status: "ended", result: (some result) })
    )
    (ok true)
  )
)

;; Update Referendum
(define-public (update-referendum (referendum-id uint) (question (optional (string-utf8 500))) (start-time (optional uint)) (end-time (optional uint)) (eligibility-criteria (optional (string-utf8 200))))
  (let
    (
      (referendum (unwrap! (get-referendum referendum-id) err-invalid-referendum))
    )
    (asserts! (is-authorized) err-unauthorized)
    (asserts! (is-eq (get status referendum) "created") err-referendum-already-started)
    (asserts! (< stacks-block-height (get start-time referendum)) err-referendum-already-started)
    (let
      (
        (updated-referendum (merge referendum
          {
            question: (default-to (get question referendum) question),
            start-time: (default-to (get start-time referendum) start-time),
            end-time: (default-to (get end-time referendum) end-time),
            eligibility-criteria: (default-to (get eligibility-criteria referendum) eligibility-criteria)
          }
        ))
      )
      (asserts! (> (get end-time updated-referendum) (get start-time updated-referendum)) err-invalid-referendum)
      (map-set referendums
        { referendum-id: referendum-id }
        updated-referendum
      )
      (ok true)
    )
  )
)

;; Get Referendum Result
(define-read-only (get-referendum-result (referendum-id uint))
  (let
    (
      (referendum (unwrap! (get-referendum referendum-id) err-invalid-referendum))
    )
    (asserts! (is-eq (get status referendum) "ended") err-referendum-not-started)
    (ok (get result referendum))
  )
)