;; Enhanced P2P Lending Contract with Interest and Collateral Management

(define-map loans uint { 
  borrower: principal, 
  lender: (optional principal), 
  principal: uint, 
  collateral: uint, 
  due-height: uint, 
  repaid: bool,
  interest-rate: uint,  ;; Interest rate in basis points (e.g., 500 = 5%)
  amount-repaid: uint,  ;; Track partial repayments
  created-height: uint  ;; When loan was created for interest calculation
})

;; Collateral vault to hold borrower's collateral
(define-map collateral-vault uint { 
  borrower: principal,
  amount: uint,
  locked: bool,
  loan-id: uint
})

;; Liquidation settings
(define-map liquidation-settings uint {
  grace-period: uint,  ;; Blocks after due date before liquidation
  liquidation-penalty: uint  ;; Penalty in basis points
})

;; Original error codes
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

;; New error codes for enhanced functionality
(define-constant ERR-INVALID-INTEREST-RATE (err u11))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u12))
(define-constant ERR-COLLATERAL-EXISTS (err u20))
(define-constant ERR-COLLATERAL-NOT-FOUND (err u21))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u22))
(define-constant ERR-COLLATERAL-LOCKED (err u23))
(define-constant ERR-NOT-LIQUIDATABLE (err u24))
(define-constant ERR-UNAUTHORIZED-LIQUIDATION (err u25))

;; Constants
(define-constant BASIS-POINTS u10000) ;; 100% = 10000 basis points
(define-constant MAX-INTEREST-RATE u2000) ;; Max 20% interest rate
(define-constant DEFAULT-GRACE-PERIOD u144) ;; ~1 day in blocks
(define-constant DEFAULT-LIQUIDATION-PENALTY u1000) ;; 10% penalty
(define-constant MIN-COLLATERAL-RATIO u12000) ;; 120% collateralization ratio
(define-constant BLOCKS-PER-DAY u144) ;; Approximate blocks per day

;; Helper function to get minimum of two values
(define-private (min-value (a uint) (b uint))
  (if (<= a b) a b))

;; Original create-loan function (backward compatible)
(define-public (create-loan (id uint) (principal uint) (collateral uint) (due uint))
  (create-loan-with-interest id principal collateral due u0))

;; Enhanced loan creation with interest rate
(define-public (create-loan-with-interest (id uint) (principal uint) (collateral uint) (due uint) (interest-rate uint))
  (begin
    ;; Validate loan doesn't already exist
    (asserts! (is-none (map-get? loans id)) ERR-LOAN-EXISTS)
    ;; Validate non-zero principal
    (asserts! (> principal u0) ERR-INVALID-PRINCIPAL)
    ;; Validate reasonable due period
    (asserts! (> due u0) ERR-INVALID-DUE)
    ;; Validate reasonable interest rate (0-20%)
    (asserts! (<= interest-rate MAX-INTEREST-RATE) ERR-INVALID-INTEREST-RATE)
    
    (map-set loans id { 
      borrower: tx-sender, 
      lender: none, 
      principal: principal, 
      collateral: collateral, 
      due-height: (+ stacks-block-height due), 
      repaid: false,
      interest-rate: interest-rate,
      amount-repaid: u0,
      created-height: stacks-block-height
    })
    (ok true)))

;; Create loan with on-chain collateral deposit
(define-public (create-loan-with-collateral (id uint) (principal uint) (collateral-amount uint) (due uint) (interest-rate uint))
  (begin
    ;; Validate collateral ratio (collateral must be at least 120% of principal)
    (asserts! (>= (* collateral-amount u100) (* principal MIN-COLLATERAL-RATIO)) ERR-INSUFFICIENT-COLLATERAL)
    ;; Validate collateral vault doesn't exist
    (asserts! (is-none (map-get? collateral-vault id)) ERR-COLLATERAL-EXISTS)
    
    ;; Transfer collateral to contract
    (match (stx-transfer? collateral-amount tx-sender (as-contract tx-sender))
      success
        (begin
          ;; Store collateral in vault
          (map-set collateral-vault id {
            borrower: tx-sender,
            amount: collateral-amount,
            locked: true,
            loan-id: id
          })
          
          ;; Set liquidation settings
          (map-set liquidation-settings id {
            grace-period: DEFAULT-GRACE-PERIOD,
            liquidation-penalty: DEFAULT-LIQUIDATION-PENALTY
          })
          
          ;; Create the loan
          (create-loan-with-interest id principal collateral-amount due interest-rate))
      error ERR-TRANSFER-FAILED)))

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
                  repaid: false,
                  interest-rate: (get interest-rate val),
                  amount-repaid: (get amount-repaid val),
                  created-height: (get created-height val)
                })
                (ok true))
            error ERR-TRANSFER-FAILED))
      ERR-LOAN-NOT-FOUND)))

;; Original repay-loan function (backward compatible - pays full amount)
(define-public (repay-loan (id uint))
  (let ((loan-data (map-get? loans id)))
    (match loan-data
      val
        (let ((total-owed (calculate-total-owed id)))
          (repay-loan-amount id total-owed))
      ERR-LOAN-NOT-FOUND)))

;; Enhanced repayment with partial payment support
(define-public (repay-loan-amount (id uint) (amount uint))
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
          ;; Validate minimum payment amount
          (asserts! (> amount u0) ERR-INSUFFICIENT-PAYMENT)
          
          (let ((lender-principal (unwrap-panic (get lender val)))
                (total-owed (calculate-total-owed id))
                (new-amount-repaid (+ (get amount-repaid val) amount))
                (is-fully-repaid (>= new-amount-repaid total-owed)))
            
            ;; Transfer payment from borrower to lender
            (match (stx-transfer? amount tx-sender lender-principal)
              success
                (begin
                  ;; Update loan with new repayment amount
                  (map-set loans id { 
                    borrower: (get borrower val), 
                    lender: (get lender val), 
                    principal: (get principal val), 
                    collateral: (get collateral val), 
                    due-height: (get due-height val), 
                    repaid: is-fully-repaid,
                    interest-rate: (get interest-rate val),
                    amount-repaid: new-amount-repaid,
                    created-height: (get created-height val)
                  })
                  (ok is-fully-repaid))
              error ERR-TRANSFER-FAILED)))
      ERR-LOAN-NOT-FOUND)))

;; Release collateral when loan is fully repaid
(define-public (release-collateral (id uint))
  (let ((collateral-data (map-get? collateral-vault id))
        (loan-data (map-get? loans id)))
    (match collateral-data
      collateral-val
        (match loan-data
          loan-val
            (begin
              ;; Validate loan is fully repaid
              (asserts! (get repaid loan-val) ERR-ALREADY-REPAID)
              ;; Validate caller is the borrower
              (asserts! (is-eq tx-sender (get borrower collateral-val)) ERR-NOT-BORROWER)
              
              ;; Transfer collateral back to borrower
              (match (as-contract (stx-transfer? (get amount collateral-val) tx-sender (get borrower collateral-val)))
                success
                  (begin
                    ;; Remove collateral from vault
                    (map-delete collateral-vault id)
                    (map-delete liquidation-settings id)
                    (ok true))
                error ERR-TRANSFER-FAILED))
          ERR-LOAN-NOT-FOUND)
      ERR-COLLATERAL-NOT-FOUND)))

;; Liquidate overdue loan
(define-public (liquidate-loan (id uint))
  (let ((loan-data (map-get? loans id))
        (collateral-data (map-get? collateral-vault id))
        (liquidation-data (map-get? liquidation-settings id)))
    (match loan-data
      loan-val
        (match collateral-data
          collateral-val
            (match liquidation-data
              liquidation-val
                (begin
                  ;; Validate loan is funded and not repaid
                  (asserts! (is-some (get lender loan-val)) ERR-LOAN-NOT-FUNDED)
                  (asserts! (not (get repaid loan-val)) ERR-ALREADY-REPAID)
                  
                  ;; Validate loan is past grace period
                  (asserts! (>= stacks-block-height 
                              (+ (get due-height loan-val) (get grace-period liquidation-val))) 
                           ERR-NOT-LIQUIDATABLE)
                  
                  ;; Only lender can liquidate
                  (asserts! (is-eq tx-sender (unwrap-panic (get lender loan-val))) ERR-UNAUTHORIZED-LIQUIDATION)
                  
                  (let ((lender-principal (unwrap-panic (get lender loan-val)))
                        (total-owed (calculate-total-owed id))
                        (penalty-amount (/ (* (get amount collateral-val) (get liquidation-penalty liquidation-val)) BASIS-POINTS))
                        (collateral-to-lender (min-value (get amount collateral-val) (+ total-owed penalty-amount))))
                    
                    ;; Transfer collateral to lender (up to amount owed + penalty)
                    (match (as-contract (stx-transfer? collateral-to-lender tx-sender lender-principal))
                      success
                        (begin
                          ;; If there's remaining collateral, return to borrower
                          (if (> (get amount collateral-val) collateral-to-lender)
                              ;; Handle the remaining collateral transfer properly
                              (match (as-contract (stx-transfer? (- (get amount collateral-val) collateral-to-lender) 
                                                               tx-sender (get borrower collateral-val)))
                                remaining-success
                                  (begin
                                    ;; Mark loan as repaid and clean up
                                    (map-set loans id (merge loan-val { repaid: true }))
                                    (map-delete collateral-vault id)
                                    (map-delete liquidation-settings id)
                                    (ok true))
                                remaining-error ERR-TRANSFER-FAILED)
                              ;; No remaining collateral, proceed with cleanup
                              (begin
                                ;; Mark loan as repaid and clean up
                                (map-set loans id (merge loan-val { repaid: true }))
                                (map-delete collateral-vault id)
                                (map-delete liquidation-settings id)
                                (ok true))))
                      error ERR-TRANSFER-FAILED)))
              ERR-COLLATERAL-NOT-FOUND)
          ERR-COLLATERAL-NOT-FOUND)
      ERR-LOAN-NOT-FOUND)))

;; Calculate total amount owed (principal + interest)
(define-read-only (calculate-total-owed (id uint))
  (match (map-get? loans id)
    loan-data
      (let ((principal (get principal loan-data))
            (interest-rate (get interest-rate loan-data))
            (blocks-elapsed (- stacks-block-height (get created-height loan-data))))
        ;; Simple interest calculation: principal + (principal * rate * time / basis-points)
        ;; Time is calculated in days (blocks / BLOCKS_PER_DAY)
        (if (is-eq interest-rate u0)
            principal  ;; No interest if rate is 0
            (+ principal 
               (/ (* (* principal interest-rate) blocks-elapsed) 
                  (* BASIS-POINTS BLOCKS-PER-DAY)))))
    u0))

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

;; Get remaining balance owed
(define-read-only (get-remaining-balance (id uint))
  (match (map-get? loans id)
    loan-data
      (let ((total-owed (calculate-total-owed id))
            (amount-repaid (get amount-repaid loan-data)))
        (if (>= amount-repaid total-owed)
            u0
            (- total-owed amount-repaid)))
    u0))

;; Check if loan is liquidatable
(define-read-only (is-liquidatable (id uint))
  (let ((loan-data (map-get? loans id))
        (liquidation-data (map-get? liquidation-settings id)))
    (match loan-data
      loan-val
        (match liquidation-data
          liquidation-val
            (and 
              (is-some (get lender loan-val))  ;; loan is funded
              (not (get repaid loan-val))      ;; loan is not repaid
              (>= stacks-block-height 
                  (+ (get due-height loan-val) (get grace-period liquidation-val)))) ;; past grace period
          false)
      false)))

;; Get collateral information
(define-read-only (get-collateral-info (id uint))
  (map-get? collateral-vault id))

;; Get liquidation settings
(define-read-only (get-liquidation-settings (id uint))
  (map-get? liquidation-settings id))
