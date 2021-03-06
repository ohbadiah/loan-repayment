module Sim where

import Loans

data SimProgress = Sim {
  elapsedT      :: Double
, principalPaid :: Double
, interestPaid  :: Double
, totalPayments :: Int
} deriving Eq

type SimResult = Either String SimProgress

simDelta :: SimProgress -> Double -> Double -> Double -> SimProgress
simDelta (Sim t p i n) dt dp di = Sim {
  elapsedT      = t + dt
, principalPaid = p + dp
, interestPaid  = i + di
, totalPayments = n + 1
  }

initialSimProgress = Sim 0 0 0 0

paydown :: RepaymentStrategy -> Loan -> SimResult
paydown repaymentStrategy loan = step repaymentStrategy loan initialSimProgress

defaultPaydown :: Loan -> SimResult
defaultPaydown = paydown defaultRepaymentStrategy

step :: RepaymentStrategy -> Loan -> SimProgress -> SimResult
step  s@(Strategy perYear fractionOfMonthly) l@(Loan balance apr monthly) sim
  | balance <= 0.01       = Right sim
  | (elapsedT sim) >= 100 = Left "That better not take more than a century to repay."
  | otherwise             = step s (l {balance = balance'}) sim' where
      dt                = 1 / (fromIntegral perYear)
      compoundedBalance = pert l dt
      payment           = monthly * fractionOfMonthly
      balance'          = compoundedBalance - payment

      -- Bookkeeping
      interestPortion   = compoundedBalance - balance
      principalPortion  = payment - interestPortion
      sim'              = simDelta sim dt principalPortion interestPortion

pert :: Loan -> Double -> Double
pert (Loan principal apr _) years = principal * (exp $ apr * years)


-- This should be done with a module that gives you formatting strings. Cause, yeah, without that...
instance Show SimProgress where
  show (Sim t principalPaid interestPaid n) = unlines [timeDesc, paymentBreakdown] where
    timeDesc = "The loan will be paid down in " ++ (formatMonths . months) t ++ "."
    paymentBreakdown = let
      totalPaid = principalPaid + interestPaid
      principalProportion = principalPaid / totalPaid
      interestProportion = interestPaid / totalPaid in
      unwords ["The composition of the payoff was ",
               (show (100 * principalProportion)), "% principal, and",
               (show (100 * interestProportion)), "% interest."]

    formatMonths :: Int -> String
    formatMonths n = let y = show $ n `div` 12
                         m = show $ n `mod` 12 in
                     y ++ " years, " ++ m ++ " months"

    months :: (RealFrac a) => a -> Int
    months t = round $ t * 12
