# P2P Lending Smart Contract

A Clarity smart contract implementing peer-to-peer lending functionality on the Stacks blockchain.

## Features

- **Loan Creation**: Borrowers can create loan requests with:
  - Principal amount (in STX)
  - Collateral amount
  - Due date (in block height)
  - Unique loan ID

- **Loan Funding**: Lenders can fund existing loan requests
  - Automatic STX transfer to borrower
  - Prevention of self-funding
  - Single lender per loan

- **Loan Repayment**: 
  - Borrower-only repayment
  - Automatic STX transfer to lender
  - Loan status tracking

## Core Data Structure

```clarity
loans: {
  borrower: principal,
  lender: (optional principal),
  principal: uint,
  collateral: uint,
  due-height: uint,
  repaid: bool
}
```

## Helper Functions

- `get-loan`: Retrieve loan details
- `is-loan-overdue`: Check loan status against current block height

## Error Handling

Comprehensive error codes for common scenarios:
- Invalid loan parameters
- Unauthorized actions
- Transfer failures
- Duplicate loans
- Invalid state transitions

## Notes

- Collateral handling is mentioned as off-chain
- All monetary transactions use STX token
- Uses Clarity's built-in block height for loan duration
