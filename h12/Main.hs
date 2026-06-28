module Main where

import Risk ( Battlefield(Battlefield), succesProb )
import Control.Monad.Random ( evalRandIO )

main :: IO ()
main = do
  res <- evalRandIO (succesProb (Battlefield 100 100))
  print res
