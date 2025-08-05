;; Digital Loyalty Points System
;; A comprehensive rewards system for businesses to issue, manage, and redeem loyalty points

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_INSUFFICIENT_BALANCE (err u402))
(define-constant ERR_INVALID_AMOUNT (err u403))
(define-constant ERR_BUSINESS_NOT_REGISTERED (err u404))
(define-constant ERR_REWARD_NOT_FOUND (err u405))
(define-constant ERR_INSUFFICIENT_POINTS (err u406))
(define-constant ERR_TRANSFER_TO_SELF (err u407))
(define-constant ERR_BUSINESS_ALREADY_REGISTERED (err u408))
(define-constant ERR_INVALID_RATE (err u409))

;; Data Variables
(define-data-var contract-owner principal CONTRACT_OWNER)
(define-data-var platform-fee-rate uint u25) ;; 0.25% (25 basis points)

;; Data Maps

;; Business registry with their point conversion rates and metadata
(define-map businesses
  principal
  {
    name: (string-ascii 64),
    conversion-rate: uint, ;; Points earned per STX spent (scaled by 1000)
    is-active: bool,
    total-points-issued: uint,
    registered-at: uint
  }
)

;; User balances per business
(define-map user-balances
  { user: principal, business: principal }
  uint
)

;; User total points across all businesses
(define-map user-total-points
  principal
  uint
)

;; Reward catalog for businesses
(define-map rewards
  { business: principal, reward-id: uint }
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    points-required: uint,
    is-active: bool,
    redeemed-count: uint,
    created-at: uint
  }
)

;; Next reward ID for each business
(define-map next-reward-id principal uint)

;; Transaction history for auditing
(define-map transaction-history
  uint
  {
    transaction-type: (string-ascii 16), ;; "earn", "redeem", "transfer"
    user: principal,
    business: (optional principal),
    amount: uint,
    block-height: uint,
    timestamp: uint
  }
)

(define-data-var next-transaction-id uint u1)

;; Read-only functions

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-business-info (business principal))
  (map-get? businesses business)
)

(define-read-only (get-user-balance (user principal) (business principal))
  (default-to u0 (map-get? user-balances { user: user, business: business }))
)

(define-read-only (get-user-total-points (user principal))
  (default-to u0 (map-get? user-total-points user))
)

(define-read-only (get-reward (business principal) (reward-id uint))
  (map-get? rewards { business: business, reward-id: reward-id })
)

(define-read-only (get-next-reward-id (business principal))
  (default-to u1 (map-get? next-reward-id business))
)

(define-read-only (get-transaction (transaction-id uint))
  (map-get? transaction-history transaction-id)
)

(define-read-only (is-business-registered (business principal))
  (is-some (map-get? businesses business))
)

;; Private functions

(define-private (record-transaction (tx-type (string-ascii 16)) (user principal) (business (optional principal)) (amount uint))
  (let ((tx-id (var-get next-transaction-id)))
    (map-set transaction-history tx-id
      {
        transaction-type: tx-type,
        user: user,
        business: business,
        amount: amount,
        block-height: stacks-block-height,
        timestamp: stacks-block-height ;; Using block height as timestamp proxy
      }
    )
    (var-set next-transaction-id (+ tx-id u1))
    tx-id
  )
)

(define-private (update-user-balance (user principal) (business principal) (new-balance uint))
  (begin
    (map-set user-balances { user: user, business: business } new-balance)
    (let ((current-total (get-user-total-points user))
          (current-business-balance (get-user-balance user business))
          (balance-diff (if (>= new-balance current-business-balance)
                           (- new-balance current-business-balance)
                           (- current-business-balance new-balance))))
      (if (>= new-balance current-business-balance)
          (map-set user-total-points user (+ current-total balance-diff))
          (map-set user-total-points user (- current-total balance-diff))
      )
    )
    true
  )
)

;; Administrative functions

(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (<= new-rate u1000) ERR_INVALID_RATE) ;; Max 10%
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

;; Business management functions

(define-public (register-business (name (string-ascii 64)) (conversion-rate uint))
  (begin
    (asserts! (> conversion-rate u0) ERR_INVALID_RATE)
    (asserts! (is-none (map-get? businesses tx-sender)) ERR_BUSINESS_ALREADY_REGISTERED)
    (map-set businesses tx-sender
      {
        name: name,
        conversion-rate: conversion-rate,
        is-active: true,
        total-points-issued: u0,
        registered-at: stacks-block-height
      }
    )
    (map-set next-reward-id tx-sender u1)
    (ok true)
  )
)

(define-public (update-business-status (business principal) (is-active bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (is-some (map-get? businesses business)) ERR_BUSINESS_NOT_REGISTERED)
    (map-set businesses business
      (merge (unwrap-panic (map-get? businesses business)) { is-active: is-active })
    )
    (ok true)
  )
)

(define-public (update-conversion-rate (new-rate uint))
  (begin
    (asserts! (> new-rate u0) ERR_INVALID_RATE)
    (asserts! (is-some (map-get? businesses tx-sender)) ERR_BUSINESS_NOT_REGISTERED)
    (map-set businesses tx-sender
      (merge (unwrap-panic (map-get? businesses tx-sender)) { conversion-rate: new-rate })
    )
    (ok true)
  )
)

;; Point management functions

(define-public (issue-points (customer principal) (points uint))
  (let ((business-info (unwrap! (map-get? businesses tx-sender) ERR_BUSINESS_NOT_REGISTERED)))
    (asserts! (get is-active business-info) ERR_UNAUTHORIZED)
    (asserts! (> points u0) ERR_INVALID_AMOUNT)

    (let ((current-balance (get-user-balance customer tx-sender))
          (new-balance (+ current-balance points)))

      ;; Update business total points issued
      (map-set businesses tx-sender
        (merge business-info { total-points-issued: (+ (get total-points-issued business-info) points) })
      )

      ;; Update user balance
      (update-user-balance customer tx-sender new-balance)

      ;; Record transaction
      (record-transaction "earn" customer (some tx-sender) points)

      (ok new-balance)
    )
  )
)

(define-public (transfer-points (recipient principal) (business principal) (amount uint))
  (begin
    (asserts! (not (is-eq tx-sender recipient)) ERR_TRANSFER_TO_SELF)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-some (map-get? businesses business)) ERR_BUSINESS_NOT_REGISTERED)

    (let ((sender-balance (get-user-balance tx-sender business))
          (recipient-balance (get-user-balance recipient business)))

      (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_POINTS)

      ;; Update balances
      (update-user-balance tx-sender business (- sender-balance amount))
      (update-user-balance recipient business (+ recipient-balance amount))

      ;; Record transaction
      (record-transaction "transfer" tx-sender (some business) amount)

      (ok true)
    )
  )
)

;; Reward management functions

(define-public (create-reward (name (string-ascii 64)) (description (string-ascii 256)) (points-required uint))
  (begin
    (asserts! (is-some (map-get? businesses tx-sender)) ERR_BUSINESS_NOT_REGISTERED)
    (asserts! (> points-required u0) ERR_INVALID_AMOUNT)

    (let ((reward-id (get-next-reward-id tx-sender)))
      (map-set rewards { business: tx-sender, reward-id: reward-id }
        {
          name: name,
          description: description,
          points-required: points-required,
          is-active: true,
          redeemed-count: u0,
          created-at: stacks-block-height
        }
      )
      (map-set next-reward-id tx-sender (+ reward-id u1))
      (ok reward-id)
    )
  )
)

(define-public (update-reward-status (reward-id uint) (is-active bool))
  (begin
    (asserts! (is-some (map-get? businesses tx-sender)) ERR_BUSINESS_NOT_REGISTERED)
    (asserts! (is-some (map-get? rewards { business: tx-sender, reward-id: reward-id })) ERR_REWARD_NOT_FOUND)

    (map-set rewards { business: tx-sender, reward-id: reward-id }
      (merge (unwrap-panic (map-get? rewards { business: tx-sender, reward-id: reward-id }))
             { is-active: is-active })
    )
    (ok true)
  )
)

(define-public (redeem-reward (business principal) (reward-id uint))
  (let ((reward-info (unwrap! (map-get? rewards { business: business, reward-id: reward-id }) ERR_REWARD_NOT_FOUND))
        (user-balance (get-user-balance tx-sender business)))

    (asserts! (get is-active reward-info) ERR_REWARD_NOT_FOUND)
    (asserts! (>= user-balance (get points-required reward-info)) ERR_INSUFFICIENT_POINTS)

    ;; Deduct points from user
    (update-user-balance tx-sender business (- user-balance (get points-required reward-info)))

    ;; Update reward redemption count
    (map-set rewards { business: business, reward-id: reward-id }
      (merge reward-info { redeemed-count: (+ (get redeemed-count reward-info) u1) })
    )

    ;; Record transaction
    (record-transaction "redeem" tx-sender (some business) (get points-required reward-info))

    (ok true)
  )
)

;; Bulk operations for efficiency

(define-public (bulk-issue-points (customers (list 50 { customer: principal, points: uint })))
  (begin
    (asserts! (is-some (map-get? businesses tx-sender)) ERR_BUSINESS_NOT_REGISTERED)
    (let ((business-info (unwrap-panic (map-get? businesses tx-sender))))
      (asserts! (get is-active business-info) ERR_UNAUTHORIZED)
      (ok (map process-bulk-issuance customers))
    )
  )
)

(define-private (process-bulk-issuance (entry { customer: principal, points: uint }))
  (let ((customer (get customer entry))
        (points (get points entry)))
    (if (> points u0)
        (let ((current-balance (get-user-balance customer tx-sender))
              (new-balance (+ current-balance points)))
          (update-user-balance customer tx-sender new-balance)
          (record-transaction "earn" customer (some tx-sender) points)
          true
        )
        false
    )
  )
)

;; Analytics functions

(define-read-only (get-business-analytics (business principal))
  (match (map-get? businesses business)
    business-info
    (ok {
      total-points-issued: (get total-points-issued business-info),
      conversion-rate: (get conversion-rate business-info),
      is-active: (get is-active business-info),
      registered-at: (get registered-at business-info)
    })
    ERR_BUSINESS_NOT_REGISTERED
  )
)

(define-read-only (get-user-analytics (user principal))
  (ok {
    total-points: (get-user-total-points user),
    balances-summary: "Call get-user-balance for specific businesses"
  })
)
