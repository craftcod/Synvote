;; Referendum Notifications & Alerts Smart Contract
;; Manages notifications for voting lifecycle events

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-REFERENDUM-NOT-FOUND (err u101))
(define-constant ERR-INVALID-NOTIFICATION-TYPE (err u102))
(define-constant ERR-NOTIFICATION-ALREADY-SENT (err u103))
(define-constant ERR-INVALID-RECIPIENT (err u104))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Notification types
(define-constant NOTIFICATION-START-VOTING u1)
(define-constant NOTIFICATION-VOTE-REMINDER u2)
(define-constant NOTIFICATION-END-VOTING u3)
(define-constant NOTIFICATION-RESULT-ANNOUNCEMENT u4)

;; Data structures
(define-map referendums
  { referendum-id: uint }
  {
    title: (string-ascii 100),
    start-block: uint,
    end-block: uint,
    is-active: bool,
    created-by: principal,
    total-eligible-voters: uint
  }
)

(define-map notifications
  { notification-id: uint }
  {
    referendum-id: uint,
    notification-type: uint,
    title: (string-ascii 100),
    message: (string-ascii 500),
    created-at: uint,
    sent-at: (optional uint),
    is-sent: bool,
    created-by: principal
  }
)

(define-map notification-recipients
  { notification-id: uint, recipient: principal }
  {
    delivered: bool,
    delivered-at: (optional uint),
    read: bool,
    read-at: (optional uint)
  }
)

(define-map eligible-voters
  { referendum-id: uint, voter: principal }
  {
    is-eligible: bool,
    has-voted: bool,
    notification-preferences: uint ;; Bitfield for notification preferences
  }
)

(define-map user-notification-settings
  { user: principal }
  {
    email-notifications: bool,
    push-notifications: bool,
    sms-notifications: bool,
    reminder-frequency: uint ;; Hours between reminders
  }
)

;; Counters
(define-data-var referendum-counter uint u0)
(define-data-var notification-counter uint u0)

;; Admin functions
(define-public (create-referendum (title (string-ascii 100)) (start-block uint) (end-block uint) (eligible-voters-list (list 100 principal)))
  (let
    (
      (referendum-id (+ (var-get referendum-counter) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> end-block start-block) (err u105))
    
    ;; Create referendum
    (map-set referendums
      { referendum-id: referendum-id }
      {
        title: title,
        start-block: start-block,
        end-block: end-block,
        is-active: true,
        created-by: tx-sender,
        total-eligible-voters: (len eligible-voters-list)
      }
    )
    
    ;; Add eligible voters using fold instead of map
    (fold add-eligible-voter-fold eligible-voters-list referendum-id)
    
    (var-set referendum-counter referendum-id)
    (ok referendum-id)
  )
)

(define-private (add-eligible-voter-fold (voter principal) (referendum-id uint))
  (begin
    (map-set eligible-voters
      { referendum-id: referendum-id, voter: voter }
      {
        is-eligible: true,
        has-voted: false,
        notification-preferences: u15 ;; All notifications enabled by default
      }
    )
    referendum-id ;; Return referendum-id to maintain fold accumulator
  )
)

;; Notification creation functions
(define-public (create-start-voting-notification (referendum-id uint))
  (let
    (
      (referendum (unwrap! (map-get? referendums { referendum-id: referendum-id }) ERR-REFERENDUM-NOT-FOUND))
      (notification-id (+ (var-get notification-counter) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= stacks-block-height (get start-block referendum)) (err u106))
    
    (map-set notifications
      { notification-id: notification-id }
      {
        referendum-id: referendum-id,
        notification-type: NOTIFICATION-START-VOTING,
        title: "Voting Now Open",
        message: (concat "Voting is now open for: " (get title referendum)),
        created-at: stacks-block-height,
        sent-at: none,
        is-sent: false,
        created-by: tx-sender
      }
    )
    
    (var-set notification-counter notification-id)
    (try! (send-notification-to-eligible-voters notification-id referendum-id))
    (ok notification-id)
  )
)

(define-public (create-vote-reminder (referendum-id uint))
  (let
    (
      (referendum (unwrap! (map-get? referendums { referendum-id: referendum-id }) ERR-REFERENDUM-NOT-FOUND))
      (notification-id (+ (var-get notification-counter) u1))
      (blocks-remaining (- (get end-block referendum) stacks-block-height))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= stacks-block-height (get start-block referendum)) 
                   (< stacks-block-height (get end-block referendum))) (err u107))
    
    (map-set notifications
      { notification-id: notification-id }
      {
        referendum-id: referendum-id,
        notification-type: NOTIFICATION-VOTE-REMINDER,
        title: "Vote Reminder",
        message: "Don't forget to cast your vote! Time remaining to vote.",
        created-at: stacks-block-height,
        sent-at: none,
        is-sent: false,
        created-by: tx-sender
      }
    )
    
    (var-set notification-counter notification-id)
    (try! (send-reminder-to-non-voters notification-id referendum-id))
    (ok notification-id)
  )
)

(define-public (create-end-voting-notification (referendum-id uint))
  (let
    (
      (referendum (unwrap! (map-get? referendums { referendum-id: referendum-id }) ERR-REFERENDUM-NOT-FOUND))
      (notification-id (+ (var-get notification-counter) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= stacks-block-height (get end-block referendum)) (err u108))
    
    (map-set notifications
      { notification-id: notification-id }
      {
        referendum-id: referendum-id,
        notification-type: NOTIFICATION-END-VOTING,
        title: "Voting Period Ended",
        message: (concat "Voting has ended for: " (get title referendum)),
        created-at: stacks-block-height,
        sent-at: none,
        is-sent: false,
        created-by: tx-sender
      }
    )
    
    (var-set notification-counter notification-id)
    (try! (send-notification-to-eligible-voters notification-id referendum-id))
    (ok notification-id)
  )
)

(define-public (create-result-announcement (referendum-id uint) (results (string-ascii 500)))
  (let
    (
      (referendum (unwrap! (map-get? referendums { referendum-id: referendum-id }) ERR-REFERENDUM-NOT-FOUND))
      (notification-id (+ (var-get notification-counter) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= stacks-block-height (get end-block referendum)) (err u109))
    
    (map-set notifications
      { notification-id: notification-id }
      {
        referendum-id: referendum-id,
        notification-type: NOTIFICATION-RESULT-ANNOUNCEMENT,
        title: "Results Announced",
        message: results,
        created-at: stacks-block-height,
        sent-at: none,
        is-sent: false,
        created-by: tx-sender
      }
    )
    
    (var-set notification-counter notification-id)
    (try! (send-notification-to-eligible-voters notification-id referendum-id))
    (ok notification-id)
  )
)

;; Notification delivery functions
(define-private (send-notification-to-eligible-voters (notification-id uint) (referendum-id uint))
  (let
    (
      (notification (unwrap! (map-get? notifications { notification-id: notification-id }) ERR-REFERENDUM-NOT-FOUND))
    )
    ;; Mark notification as sent
    (map-set notifications
      { notification-id: notification-id }
      (merge notification { is-sent: true, sent-at: (some stacks-block-height) })
    )
    (ok true)
  )
)

(define-private (send-reminder-to-non-voters (notification-id uint) (referendum-id uint))
  (let
    (
      (notification (unwrap! (map-get? notifications { notification-id: notification-id }) ERR-REFERENDUM-NOT-FOUND))
    )
    ;; Mark notification as sent
    (map-set notifications
      { notification-id: notification-id }
      (merge notification { is-sent: true, sent-at: (some stacks-block-height) })
    )
    (ok true)
  )
)

;; User functions
(define-public (mark-vote-cast (referendum-id uint))
  (let
    (
      (voter-record (unwrap! (map-get? eligible-voters { referendum-id: referendum-id, voter: tx-sender }) ERR-INVALID-RECIPIENT))
    )
    (asserts! (get is-eligible voter-record) ERR-NOT-AUTHORIZED)
    
    (map-set eligible-voters
      { referendum-id: referendum-id, voter: tx-sender }
      (merge voter-record { has-voted: true })
    )
    (ok true)
  )
)

(define-public (update-notification-settings (email bool) (push bool) (sms bool) (reminder-freq uint))
  (begin
    (map-set user-notification-settings
      { user: tx-sender }
      {
        email-notifications: email,
        push-notifications: push,
        sms-notifications: sms,
        reminder-frequency: reminder-freq
      }
    )
    (ok true)
  )
)

(define-public (mark-notification-read (notification-id uint))
  (let
    (
      (recipient-record (map-get? notification-recipients { notification-id: notification-id, recipient: tx-sender }))
    )
    (match recipient-record
      existing-record
      (begin
        (map-set notification-recipients
          { notification-id: notification-id, recipient: tx-sender }
          (merge existing-record { read: true, read-at: (some stacks-block-height) })
        )
        (ok true)
      )
      (begin
        (map-set notification-recipients
          { notification-id: notification-id, recipient: tx-sender }
          {
            delivered: true,
            delivered-at: (some stacks-block-height),
            read: true,
            read-at: (some stacks-block-height)
          }
        )
        (ok true)
      )
    )
  )
)

;; Read-only functions
(define-read-only (get-referendum (referendum-id uint))
  (map-get? referendums { referendum-id: referendum-id })
)

(define-read-only (get-notification (notification-id uint))
  (map-get? notifications { notification-id: notification-id })
)

(define-read-only (get-user-notifications (user principal) (referendum-id uint))
  (let
    (
      (voter-record (map-get? eligible-voters { referendum-id: referendum-id, voter: user }))
    )
    (match voter-record
      record (some record)
      none
    )
  )
)

(define-read-only (get-notification-status (notification-id uint) (recipient principal))
  (map-get? notification-recipients { notification-id: notification-id, recipient: recipient })
)

(define-read-only (get-user-notification-settings (user principal))
  (default-to
    {
      email-notifications: true,
      push-notifications: true,
      sms-notifications: false,
      reminder-frequency: u24
    }
    (map-get? user-notification-settings { user: user })
  )
)

(define-read-only (is-eligible-voter (referendum-id uint) (voter principal))
  (match (map-get? eligible-voters { referendum-id: referendum-id, voter: voter })
    record (get is-eligible record)
    false
  )
)

(define-read-only (has-voted (referendum-id uint) (voter principal))
  (match (map-get? eligible-voters { referendum-id: referendum-id, voter: voter })
    record (get has-voted record)
    false
  )
)

(define-read-only (get-referendum-counter)
  (var-get referendum-counter)
)

(define-read-only (get-notification-counter)
  (var-get notification-counter)
)

;; Utility functions for checking notification timing
(define-read-only (should-send-reminder (referendum-id uint))
  (let
    (
      (referendum (unwrap! (map-get? referendums { referendum-id: referendum-id }) false))
      (blocks-remaining (- (get end-block referendum) stacks-block-height))
    )
    (and 
      (>= stacks-block-height (get start-block referendum))
      (< stacks-block-height (get end-block referendum))
      (<= blocks-remaining u144) ;; Send reminder when less than 24 hours remain (assuming 10min blocks)
    )
  )
)

(define-read-only (is-voting-period-active (referendum-id uint))
  (let
    (
      (referendum (unwrap! (map-get? referendums { referendum-id: referendum-id }) false))
    )
    (and 
      (>= stacks-block-height (get start-block referendum))
      (< stacks-block-height (get end-block referendum))
      (get is-active referendum)
    )
  )
)
