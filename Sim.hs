module Sim where

import Loans

months :: (RealFrac a) => a -> Int
months t = round $ t * 12

data SimProgress = Sim {
  elapsedT :: Double
, principalPaid :: Double
, interestPaid :: Double
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


formatMonths :: Int -> String
formatMonths n = let y = show $ n `div` 12
                     m = show $ n `mod` 12 in
                         y ++ " years, " ++ m ++ " months"

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


-- This should be done with a module that gives you formatting strings. Cause, yeah, wtihout that...
instance Show SimProgress where
  show (Sim t principalPaid interestPaid n) = unlines [timeDesc, paymentBreakdown, payments] where
    timeDesc = "The loan will be paid down in " ++ (formatMonths . months) t ++ "."
    paymentBreakdown = let
      totalPaid = principalPaid + interestPaid
      principalProportion = principalPaid / totalPaid
      interestProportion = interestPaid / totalPaid in
      unwords ["It took a total of $",
               (show totalPaid),
               ",\n", (show principalPaid), "(", (show (100 * principalProportion)), "%) of which was principal, and ",
               "\n", (show interestPaid), "(", (show (100 * interestProportion)), "%) of which was interest."]
    payments = "That's a total of " ++ (show n) ++ " payments.\n\n"
