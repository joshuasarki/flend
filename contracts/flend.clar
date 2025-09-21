;; P2P Lending Contract with improved validations and error handling

(define-map loans uint { 
  borrower: principal, 
  lender: (optional principal), 
  principal: uint, 
  collateral: uint, 
  due-height: uint, 
  repaid: bool 
})

;; Error codes
(define-constant ERR-LOAN-NOT-FOUND (err u1))
(define-constant ERR-TRANSFER-FAILED (err u2))
(define-constant ERR-LOAN-EXISTS (err u3))
(define-constant ERR-INVALID-PRINCIPAL (err u4))
(define-constant ERR-INVALID-DUE (err u5))
(define-constant ERR-ALREADY-FUNDED (err u6))
(define-constant ERR-SELF-FUNDING (err u7))
(define-constant ERR-LOAN-NOT-FUNDED (err u8))
(define-constant ERR-ALREADY-REPAID (err u9))
(define-constant ERR-NOT-BORROWER (err u10))

(define-public (create-loan (id uint) (principal uint) (collateral uint) (due uint))
  (begin
    ;; Validate loan doesn't already exist
    (asserts! (is-none (map-get? loans id)) ERR-LOAN-EXISTS)
    ;; Validate non-zero principal
    (asserts! (> principal u0) ERR-INVALID-PRINCIPAL)
    ;; Validate reasonable due period
    (asserts! (> due u0) ERR-INVALID-DUE)
    
    ;; borrower must transfer collateral off-chain or to contract
    (map-set loans id { 
      borrower: tx-sender, 
      lender: none, 
      principal: principal, 
      collateral: collateral, 
      due-height: (+ stacks-block-height due), 
      repaid: false 
    })
    (ok true)))

(define-public (fund-loan (id uint))
  (let ((loan-data (map-get? loans id)))
    (match loan-data
      val
        (begin
          ;; Validate loan hasn't been funded yet
          (asserts! (is-none (get lender val)) ERR-ALREADY-FUNDED)
          ;; Prevent self-funding
          (asserts! (not (is-eq tx-sender (get borrower val))) ERR-SELF-FUNDING)
          
          ;; Validate transfer succeeds before updating loan state
          (match (stx-transfer? (get principal val) tx-sender (get borrower val))
            success
              (begin
                (map-set loans id { 
                  borrower: (get borrower val), 
                  lender: (some tx-sender), 
                  principal: (get principal val), 
                  collateral: (get collateral val), 
                  due-height: (get due-height val), 
                  repaid: false 
                })
                (ok true))
            error ERR-TRANSFER-FAILED))
      ERR-LOAN-NOT-FOUND)))

(define-public (repay-loan (id uint))
  (let ((loan-data (map-get? loans id)))
    (match loan-data
      val
        (begin
          ;; Validate loan is funded (has a lender)
          (asserts! (is-some (get lender val)) ERR-LOAN-NOT-FUNDED)
          ;; Validate loan hasn't been repaid yet
          (asserts! (not (get repaid val)) ERR-ALREADY-REPAID)
          ;; Validate only borrower can repay
          (asserts! (is-eq tx-sender (get borrower val)) ERR-NOT-BORROWER)
          
          ;; Get the lender from the optional
          (let ((lender-principal (unwrap-panic (get lender val))))
            ;; Transfer principal amount from borrower back to lender
            (match (stx-transfer? (get principal val) tx-sender lender-principal)
              success
                (begin
                  ;; Mark loan as repaid
                  (map-set loans id { 
                    borrower: (get borrower val), 
                    lender: (get lender val), 
                    principal: (get principal val), 
                    collateral: (get collateral val), 
                    due-height: (get due-height val), 
                    repaid: true 
                  })
                  (ok true))
              error ERR-TRANSFER-FAILED)))
      ERR-LOAN-NOT-FOUND)))

;; Read-only function to get loan details
(define-read-only (get-loan (id uint))
  (map-get? loans id))

;; Read-only function to check if loan is overdue
(define-read-only (is-loan-overdue (id uint))
  (let ((loan-data (map-get? loans id)))
    (match loan-data
      val
        (and 
          (is-some (get lender val))  ;; loan is funded
          (not (get repaid val))      ;; loan is not repaid
          (>= stacks-block-height (get due-height val))) ;; past due date
      false)))