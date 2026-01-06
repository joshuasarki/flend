import { describe, it, expect } from "vitest";
import { Cl, ClarityType } from "@stacks/transactions";

const contractName = "flend";
const BASIS_POINTS = 10000n;
const BLOCKS_PER_DAY = 144n;

const accounts = simnet.getAccounts();
const borrower = accounts.get("wallet_1")!;
const lender = accounts.get("wallet_2")!;
const principalValue = (cv: any) => cv.value;

const toUInt = (cv: any) => BigInt(cv.value);
const toBool = (cv: any) => cv.type === "true";
const optPrincipal = (cv: any) =>
  cv.type === "some" && cv.value ? cv.value.value : null;

const mineUntilHeight = (targetHeight: number) => {
  while (simnet.blockHeight < targetHeight) {
    simnet.mineBlock([]);
  }
};

const getLoan = (id: number) => {
  const { result } = simnet.callReadOnlyFn(
    contractName,
    "get-loan",
    [Cl.uint(id)],
    borrower
  );
  expect(result.type).toBe(ClarityType.OptionalSome);
  return (result as any).value.value;
};

const getLiquidationSettings = (id: number) => {
  const { result } = simnet.callReadOnlyFn(
    contractName,
    "get-liquidation-settings",
    [Cl.uint(id)],
    borrower
  );
  expect(result.type).toBe(ClarityType.OptionalSome);
  return (result as any).value.value;
};

const computeAccrual = (
  outstanding: bigint,
  rate: bigint,
  lastHeight: bigint,
  targetHeight: bigint
) => {
  if (outstanding === 0n || rate === 0n || targetHeight <= lastHeight) return 0n;
  return (
    (outstanding * rate * (targetHeight - lastHeight)) /
    (BASIS_POINTS * BLOCKS_PER_DAY)
  );
};

describe("flend contract core flows", () => {
  it("creates and funds a collateralised loan", () => {
    const create = simnet.callPublicFn(
      contractName,
      "create-loan-with-collateral",
      [Cl.uint(1), Cl.uint(1_000_000), Cl.uint(1_500_000), Cl.uint(10), Cl.uint(0)],
      borrower
    );
    expect(create.result).toBeOk(Cl.bool(true));

    const fund = simnet.callPublicFn(
      contractName,
      "fund-loan",
      [Cl.uint(1)],
      lender
    );
    expect(fund.result).toBeOk(Cl.bool(true));

    const loan = getLoan(1);
    expect(optPrincipal(loan["lender"])).toBe(lender);
    expect(optPrincipal(loan["lender"])).not.toBeNull();
    expect(principalValue(loan["borrower"])).toBe(borrower);
    expect(toUInt(loan["outstanding-principal"])).toBe(1_000_000n);
    expect(toBool(loan["repaid"])).toBe(false);

    const { result: totalOwed } = simnet.callReadOnlyFn(
      contractName,
      "calculate-total-owed",
      [Cl.uint(1)],
      borrower
    );
    expect(totalOwed).toBeUint(1_000_000);
  });

  it("applies interest accrual and repays interest before principal", () => {
    const loanId = 2;
    const principal = 1_000_000n;
    const rate = 500n; // 5%

    const create = simnet.callPublicFn(
      contractName,
      "create-loan-with-collateral",
      [Cl.uint(loanId), Cl.uint(principal), Cl.uint(principal * 12n / 10n), Cl.uint(10), Cl.uint(rate)],
      borrower
    );
    expect(create.result).toBeOk(Cl.bool(true));
    const fund = simnet.callPublicFn(
      contractName,
      "fund-loan",
      [Cl.uint(loanId)],
      lender
    );
    expect(fund.result).toBeOk(Cl.bool(true));

    const before = getLoan(loanId);
    const lastAccrued = toUInt(before["last-accrued-height"]);
    const targetHeight = Number(lastAccrued + 144n);
    mineUntilHeight(targetHeight - 1); // next transaction will execute at targetHeight

    const payment = 200_000n;
    const repay = simnet.callPublicFn(
      contractName,
      "repay-loan-amount",
      [Cl.uint(loanId), Cl.uint(payment)],
      borrower
    );
    expect(repay.result).toBeOk(Cl.bool(false)); // not fully repaid yet

    const after = getLoan(loanId);
    const blocksElapsed =
      toUInt(after["last-accrued-height"]) - lastAccrued;
    const accrued = computeAccrual(
      toUInt(before["outstanding-principal"]),
      rate,
      lastAccrued,
      lastAccrued + blocksElapsed
    );

    const paymentToInterest = payment < accrued ? payment : accrued;
    const remainingAfterInterest = payment - paymentToInterest;
    const principalPaid =
      remainingAfterInterest > toUInt(before["outstanding-principal"])
        ? toUInt(before["outstanding-principal"])
        : remainingAfterInterest;

    const expectedAccrued = accrued - paymentToInterest;
    const expectedOutstanding =
      toUInt(before["outstanding-principal"]) - principalPaid;

    expect(toUInt(after["accrued-interest"])).toBe(expectedAccrued);
    expect(toUInt(after["outstanding-principal"])).toBe(expectedOutstanding);
    expect(toUInt(after["amount-repaid"])).toBe(payment);
    expect(toBool(after["repaid"])).toBe(false);
  });

  it("fully repays and releases collateral", () => {
    const loanId = 3;
    const create = simnet.callPublicFn(
      contractName,
      "create-loan-with-collateral",
      [Cl.uint(loanId), Cl.uint(500_000), Cl.uint(700_000), Cl.uint(5), Cl.uint(0)],
      borrower
    );
    expect(create.result).toBeOk(Cl.bool(true));

    const fund = simnet.callPublicFn(
      contractName,
      "fund-loan",
      [Cl.uint(loanId)],
      lender
    );
    expect(fund.result).toBeOk(Cl.bool(true));

    const repay = simnet.callPublicFn(
      contractName,
      "repay-loan",
      [Cl.uint(loanId)],
      borrower
    );
    expect(repay.result).toBeOk(Cl.bool(true));

    const release = simnet.callPublicFn(
      contractName,
      "release-collateral",
      [Cl.uint(loanId)],
      borrower
    );
    expect(release.result).toBeOk(Cl.bool(true));

    const { result: collateralInfo } = simnet.callReadOnlyFn(
      contractName,
      "get-collateral-info",
      [Cl.uint(loanId)],
      borrower
    );
    expect(collateralInfo).toBeNone();

    const after = getLoan(loanId);
    expect(toBool(after["repaid"])).toBe(true);
    expect(toUInt(after["outstanding-principal"])).toBe(0n);
    expect(toUInt(after["accrued-interest"])).toBe(0n);
  });

  it("liquidates after grace period and cleans up state", () => {
    const loanId = 4;
    const create = simnet.callPublicFn(
      contractName,
      "create-loan-with-collateral",
      [Cl.uint(loanId), Cl.uint(1_000_000), Cl.uint(1_400_000), Cl.uint(1), Cl.uint(0)],
      borrower
    );
    expect(create.result).toBeOk(Cl.bool(true));

    const fund = simnet.callPublicFn(
      contractName,
      "fund-loan",
      [Cl.uint(loanId)],
      lender
    );
    expect(fund.result).toBeOk(Cl.bool(true));

    const loan = getLoan(loanId);
    const settings = getLiquidationSettings(loanId);
    const dueHeight = Number(toUInt(loan["due-height"]));
    const gracePeriod = Number(toUInt(settings["grace-period"]));
    const targetHeight = dueHeight + gracePeriod;

    // Not liquidatable before grace period expires
    const { result: liquidatableEarly } = simnet.callReadOnlyFn(
      contractName,
      "is-liquidatable",
      [Cl.uint(loanId)],
      lender
    );
    expect(liquidatableEarly).toBeBool(false);

    mineUntilHeight(targetHeight);
    const liquidate = simnet.callPublicFn(
      contractName,
      "liquidate-loan",
      [Cl.uint(loanId)],
      lender
    );
    expect(liquidate.result).toBeOk(Cl.bool(true));

    const { result: collateralAfter } = simnet.callReadOnlyFn(
      contractName,
      "get-collateral-info",
      [Cl.uint(loanId)],
      borrower
    );
    expect(collateralAfter).toBeNone();

    const repaidLoan = getLoan(loanId);
    expect(toBool(repaidLoan["repaid"])).toBe(true);
    expect(toUInt(repaidLoan["outstanding-principal"])).toBe(0n);
    expect(toUInt(repaidLoan["accrued-interest"])).toBe(0n);

    const { result: liquidatableAfter } = simnet.callReadOnlyFn(
      contractName,
      "is-liquidatable",
      [Cl.uint(loanId)],
      lender
    );
    expect(liquidatableAfter).toBeBool(false);
  });

  it("rejects undercollateralised creation", () => {
    const create = simnet.callPublicFn(
      contractName,
      "create-loan-with-collateral",
      [Cl.uint(5), Cl.uint(1_000_000), Cl.uint(900_000), Cl.uint(10), Cl.uint(100)],
      borrower
    );
    expect(create.result).toBeErr(Cl.uint(22)); // ERR-INSUFFICIENT-COLLATERAL
  });
});
