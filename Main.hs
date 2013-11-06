module Main where

import Loans
import Sim

main ::IO ()
main = putStrLn $ show $ defaultPaydown sampleMortgage
