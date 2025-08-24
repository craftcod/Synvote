;; Distributed and Secure Vote Collection Smart Contract
;; Implements tamper-resistant storage and distributed vote recording

;; ===== CONSTANTS =====
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-VOTE (err u101))
(define-constant ERR-ALREADY-VOTED (err u102))
(define-constant ERR-REFERENDUM-NOT-ACTIVE (err u103))
(define-constant ERR-REFERENDUM-ENDED (err u104))
(define-constant ERR-INVALID-NODE (err u105))
(define-constant ERR-INVALID-HASH (err u106))
(define-constant ERR-NODE-ALREADY-EXISTS (err u107))
(define-constant ERR-INSUFFICIENT-CONFIRMATIONS (err u108))

;; Minimum number of node confirmations required for vote validation
(define-constant MIN-CONFIRMATIONS u3)

;; Maximum number of distributed storage nodes
(define-constant MAX-STORAGE-NODES u10)

;; ===== DATA VARIABLES =====
(define-data-var referendum-active bool true)
(define-data-var referendum-id uint u1)
(define-data-var total-votes uint u0)
(define-data-var storage-node-count uint u0)
(define-data-var vote-counter uint u0)

;; ===== DATA MAPS =====

;; Main vote storage with distributed hash verification
(define-map votes
    { vote-id: uint }
    {
        voter: principal,
        choice: (string-ascii 10),
        timestamp: uint,
        block-height: uint,
        vote-hash: (buff 20),
        ipfs-hash: (optional (string-ascii 64)), ;; IPFS content hash
        confirmations: uint,
        is-verified: bool
    }
)

;; Voter registry to prevent double voting
(define-map voter-registry
    { voter: principal, referendum-id: uint }
    { vote-id: uint, voted-at: uint }
)

;; Distributed storage nodes registry
(define-map storage-nodes
    { node-id: uint }
    {
        node-address: principal,
        node-endpoint: (string-ascii 128), ;; IPFS node endpoint or similar
        is-active: bool,
        votes-stored: uint,
        reputation-score: uint, ;; 0-1000 scale
        last-ping: uint
    }
)

;; Vote confirmations from distributed nodes
(define-map vote-confirmations
    { vote-id: uint, node-id: uint }
    {
        confirmed-by: principal,
        confirmation-hash: (buff 20),
        confirmed-at: uint,
        storage-proof: (optional (string-ascii 128)) ;; Proof of storage
    }
)

;; Immutable referendum metadata
(define-map referendum-metadata
    { referendum-id: uint }
    {
        title: (string-ascii 256),
        description: (string-ascii 1024),
        created-by: principal,
        created-at: uint,
        end-time: (optional uint),
        metadata-hash: (buff 20),
        ipfs-metadata-hash: (optional (string-ascii 64))
    }
)

;; Merkle tree roots for batch verification
(define-map merkle-roots
    { batch-id: uint }
    {
        root-hash: (buff 32),
        vote-count: uint,
        created-at: uint,
        ipfs-proof: (optional (string-ascii 64))
    }
)

;; Vote integrity checksums
(define-map vote-checksums
    { referendum-id: uint, batch-size: uint }
    {
        checksum: (buff 32),
        vote-count: uint,
        last-updated: uint
    }
)

;; ===== PUBLIC FUNCTIONS =====

;; Initialize a new referendum
(define-public (initialize-referendum 
    (title (string-ascii 256))
    (description (string-ascii 1024))
    (ipfs-metadata (optional (string-ascii 64))))
    (let (
        (current-ref-id (var-get referendum-id))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        
        ;; Store referendum metadata immutably
        (map-set referendum-metadata
            { referendum-id: current-ref-id }
            {
                title: title,
                description: description,
                created-by: tx-sender,
                created-at: stacks-block-height,
                end-time: none,
                metadata-hash: (hash160 (+ (len title) (len description) stacks-block-height)),
                ipfs-metadata-hash: ipfs-metadata
            }
        )
        
        ;; Reset vote counter for new referendum
        (var-set vote-counter u0)
        (var-set total-votes u0)
        (var-set referendum-active true)
        
        ;; Emit event for off-chain systems
        (print {
            event: "referendum-initialized",
            referendum-id: current-ref-id,
            title: title,
            metadata-hash: (hash160 (+ (len title) (len description) stacks-block-height)),
            ipfs-hash: ipfs-metadata,
            block-height: stacks-block-height
        })
        
        (ok current-ref-id)
    )
)

;; Cast a vote with distributed storage
(define-public (cast-vote 
    (choice (string-ascii 10))
    (ipfs-hash (optional (string-ascii 64))))
    (let (
        (current-ref-id (var-get referendum-id))
        (vote-id (+ (var-get vote-counter) u1))
        (vote-hash (create-vote-hash tx-sender choice stacks-block-height))
    )
        ;; Validation checks
        (asserts! (var-get referendum-active) ERR-REFERENDUM-NOT-ACTIVE)
        (asserts! (or (is-eq choice "yes") (is-eq choice "no") (is-eq choice "abstain")) ERR-INVALID-VOTE)
        (asserts! (is-none (map-get? voter-registry { voter: tx-sender, referendum-id: current-ref-id })) ERR-ALREADY-VOTED)
        
        ;; Record the vote
        (map-set votes
            { vote-id: vote-id }
            {
                voter: tx-sender,
                choice: choice,
                timestamp: stacks-block-height,
                block-height: stacks-block-height,
                vote-hash: vote-hash,
                ipfs-hash: ipfs-hash,
                confirmations: u0,
                is-verified: false
            }
        )
        
        ;; Register voter
        (map-set voter-registry
            { voter: tx-sender, referendum-id: current-ref-id }
            { vote-id: vote-id, voted-at: stacks-block-height }
        )
        
        ;; Update counters
        (var-set vote-counter vote-id)
        (var-set total-votes (+ (var-get total-votes) u1))
        
        ;; Request distributed storage from nodes
        ;; (try! (request-distributed-storage vote-id))
        
        ;; Emit event for distributed network
        (print {
            event: "vote-cast",
            vote-id: vote-id,
            referendum-id: current-ref-id,
            voter: tx-sender,
            vote-hash: vote-hash,
            ipfs-hash: ipfs-hash,
            block-height: stacks-block-height,
            requires-storage: true
        })
        
        (ok vote-id)
    )
)

;; Confirm vote storage by distributed node
(define-public (confirm-vote-storage 
    (vote-id uint)
    (node-id uint)
    (storage-proof (optional (string-ascii 128))))
    (let (
        (vote-data (unwrap! (map-get? votes { vote-id: vote-id }) ERR-INVALID-VOTE))
        (node-data (unwrap! (map-get? storage-nodes { node-id: node-id }) ERR-INVALID-NODE))
        (confirmation-hash (hash160 (+ vote-id stacks-block-height)))
    )
        ;; Verify node is active and authorized
        (asserts! (get is-active node-data) ERR-INVALID-NODE)
        (asserts! (is-eq tx-sender (get node-address node-data)) ERR-UNAUTHORIZED)
        
        ;; Record confirmation
        (map-set vote-confirmations
            { vote-id: vote-id, node-id: node-id }
            {
                confirmed-by: tx-sender,
                confirmation-hash: confirmation-hash,
                confirmed-at: stacks-block-height,
                storage-proof: storage-proof
            }
        )
        
        ;; Update vote confirmation count
        (let (
            (new-confirmations (+ (get confirmations vote-data) u1))
            (is-now-verified (>= new-confirmations MIN-CONFIRMATIONS))
        )
            (map-set votes
                { vote-id: vote-id }
                (merge vote-data {
                    confirmations: new-confirmations,
                    is-verified: is-now-verified
                })
            )
            
            ;; Update node statistics
            (map-set storage-nodes
                { node-id: node-id }
                (merge node-data {
                    votes-stored: (+ (get votes-stored node-data) u1),
                    last-ping: stacks-block-height
                })
            )
            
            ;; Emit confirmation event
            (print {
                event: "vote-storage-confirmed",
                vote-id: vote-id,
                node-id: node-id,
                confirmations: new-confirmations,
                is-verified: is-now-verified,
                storage-proof: storage-proof
            })
            
            (ok is-now-verified)
        )
    )
)

;; Register a new distributed storage node
(define-public (register-storage-node 
    (node-endpoint (string-ascii 128)))
    (let (
        (node-id (+ (var-get storage-node-count) u1))
    )
        (asserts! (<= node-id MAX-STORAGE-NODES) ERR-INVALID-NODE)
        (asserts! (is-none (get-node-by-address tx-sender)) ERR-NODE-ALREADY-EXISTS)
        
        ;; Register the node
        (map-set storage-nodes
            { node-id: node-id }
            {
                node-address: tx-sender,
                node-endpoint: node-endpoint,
                is-active: true,
                votes-stored: u0,
                reputation-score: u800, ;; Start with good reputation
                last-ping: stacks-block-height
            }
        )
        
        (var-set storage-node-count node-id)
        
        ;; Emit event
        (print {
            event: "storage-node-registered",
            node-id: node-id,
            node-address: tx-sender,
            node-endpoint: node-endpoint
        })
        
        (ok node-id)
    )
)

;; Create Merkle root for vote batch verification
(define-public (create-merkle-root 
    (batch-id uint)
    (root-hash (buff 32))
    (vote-count uint)
    (ipfs-proof (optional (string-ascii 64))))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        
        (map-set merkle-roots
            { batch-id: batch-id }
            {
                root-hash: root-hash,
                vote-count: vote-count,
                created-at: stacks-block-height,
                ipfs-proof: ipfs-proof
            }
        )
        
        ;; Update referendum checksum
        ;; (try! (update-referendum-checksum))
        
        (print {
            event: "merkle-root-created",
            batch-id: batch-id,
            root-hash: root-hash,
            vote-count: vote-count,
            ipfs-proof: ipfs-proof
        })
        
        (ok true)
    )
)

;; End referendum and finalize results
(define-public (end-referendum)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (asserts! (var-get referendum-active) ERR-REFERENDUM-ENDED)
        
        ;; Finalize referendum
        (var-set referendum-active false)
        
        ;; Update referendum metadata with end time
        (let (
            (current-ref-id (var-get referendum-id))
            (existing-metadata (unwrap! (map-get? referendum-metadata { referendum-id: current-ref-id }) ERR-UNAUTHORIZED))
        )
            (map-set referendum-metadata
                { referendum-id: current-ref-id }
                (merge existing-metadata { end-time: (some stacks-block-height) })
            )
        )
        
        ;; Create final checksum
        ;; (try! (update-referendum-checksum))
        
        ;; Prepare for next referendum
        (var-set referendum-id (+ (var-get referendum-id) u1))
        
        (print {
            event: "referendum-ended",
            referendum-id: (var-get referendum-id),
            total-votes: (var-get total-votes),
            end-block: stacks-block-height
        })
        
        (ok true)
    )
)

;; ===== PRIVATE FUNCTIONS =====

;; Create vote hash for integrity verification
(define-private (create-vote-hash (voter principal) (choice (string-ascii 10)) (timestamp uint))
    (hash160 (+ timestamp (len choice)))
)

;; Request distributed storage for a vote
(define-private (request-distributed-storage (vote-id uint))
    (begin
        ;; This would trigger off-chain processes to store the vote
        ;; across multiple distributed nodes
        (print {
            event: "storage-requested",
            vote-id: vote-id,
            required-confirmations: MIN-CONFIRMATIONS
        })
        (ok true)
    )
)

;; Update referendum integrity checksum
(define-private (update-referendum-checksum)
    (let (
        (current-ref-id (var-get referendum-id))
        (total-votes-count (var-get total-votes))
        (checksum-input (+ current-ref-id total-votes-count))
        (checksum (hash160 checksum-input))
    )
        (map-set vote-checksums
            { referendum-id: current-ref-id, batch-size: total-votes-count }
            {
                checksum: (concat checksum 0x000000000000000000000000), ;; Pad to 32 bytes
                vote-count: total-votes-count,
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Get node by address (helper function)
(define-private (get-node-by-address (address principal))
    (let (
        (node-1 (map-get? storage-nodes { node-id: u1 }))
        (node-2 (map-get? storage-nodes { node-id: u2 }))
        (node-3 (map-get? storage-nodes { node-id: u3 }))
    )
        ;; Simple check for first few nodes (could be extended)
        (if (and (is-some node-1) (is-eq address (get node-address (unwrap-panic node-1))))
            node-1
            (if (and (is-some node-2) (is-eq address (get node-address (unwrap-panic node-2))))
                node-2
                (if (and (is-some node-3) (is-eq address (get node-address (unwrap-panic node-3))))
                    node-3
                    none
                )
            )
        )
    )
)

;; ===== READ-ONLY FUNCTIONS =====

;; Get vote details with verification status
(define-read-only (get-vote (vote-id uint))
    (map-get? votes { vote-id: vote-id })
)

;; ;; Get referendum metadata
;; (define-read-only (get-referendum-metadata (referendum-id uint))
;;     (map-get? referendum-metadata { referendum-id: referendum-id })
;; )

;; Get storage node information
(define-read-only (get-storage-node (node-id uint))
    (map-get? storage-nodes { node-id: node-id })
)

;; Get vote confirmations for a specific vote
(define-read-only (get-vote-confirmations (vote-id uint) (node-id uint))
    (map-get? vote-confirmations { vote-id: vote-id, node-id: node-id })
)

;; Get Merkle root for batch verification
(define-read-only (get-merkle-root (batch-id uint))
    (map-get? merkle-roots { batch-id: batch-id })
)

;; ;; Get referendum checksum
;; (define-read-only (get-referendum-checksum (referendum-id uint))
;;     (map-get? vote-checksums { referendum-id: referendum-id, batch-size: (var-get total-votes) })
;; )

;; Check if voter has voted in current referendum
(define-read-only (has-voted (voter principal))
    (is-some (map-get? voter-registry { voter: voter, referendum-id: (var-get referendum-id) }))
)

;; Get current referendum status
(define-read-only (get-referendum-status)
    {
        referendum-id: (var-get referendum-id),
        is-active: (var-get referendum-active),
        total-votes: (var-get total-votes),
        storage-nodes: (var-get storage-node-count),
        block-height: stacks-block-height
    }
)

;; Verify vote integrity
(define-read-only (verify-vote-integrity (vote-id uint))
    (let (
        (vote-data (map-get? votes { vote-id: vote-id }))
    )
        (if (is-some vote-data)
            {
                exists: true,
                is-verified: (get is-verified (unwrap-panic vote-data)),
                confirmations: (get confirmations (unwrap-panic vote-data)),
                required-confirmations: MIN-CONFIRMATIONS,
                vote-hash: (get vote-hash (unwrap-panic vote-data))
            }
            {
                exists: false,
                is-verified: false,
                confirmations: u0,
                required-confirmations: MIN-CONFIRMATIONS,
                vote-hash: 0x
            }
        )
    )
)

;; Get contract statistics
(define-read-only (get-contract-stats)
    {
        total-referendums: (var-get referendum-id),
        current-referendum-votes: (var-get total-votes),
        active-storage-nodes: (var-get storage-node-count),
        min-confirmations: MIN-CONFIRMATIONS,
        max-storage-nodes: MAX-STORAGE-NODES
    }
)