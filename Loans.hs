module Loans where

data Loan = Loan {
  balance :: Double
, apr     :: Double
, monthly :: Double
  } deriving (Show, Eq)

data RepaymentStrategy = Strategy {
  paymentsPerYear :: Int
, fractionOfMonthly :: Double
  } deriving (Show, Eq)

defaultRepaymentStrategy = Strategy 12 1


sampleMortgage = Loan 250000 0.05 1342.05
