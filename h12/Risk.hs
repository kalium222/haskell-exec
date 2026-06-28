{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
    ( replicateM,
      MonadRandom(getRandom),
      Rand,
      Random(randomR, random),
      StdGen )
import Data.List (sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}

-- Ex.2
battle :: Battlefield -> Rand StdGen Battlefield
battle bf =
  let att = min 3 (attackers bf - 1)
      def = min 2 (defenders bf)
      desc = sortBy (flip compare)
      solve att_list def_list =
        let pairs = zip (desc att_list) (desc def_list)
            att_win = length $ filter (uncurry (>)) pairs
            def_win = length pairs - att_win
         in Battlefield (attackers bf - def_win) (defenders bf - att_win)
   in liftA2 solve (replicateM att die) (replicateM def die)

-- Ex.3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield att def)
  | att < 2 || def <= 0 = return bf
  | otherwise = battle bf >>= invade

-- Ex.4
succesProb :: Battlefield -> Rand StdGen Double
succesProb bf = compute_rate <$> (replicateM 1000 (invade bf))
  where
    destroy_rate = filter ((== 0) . defenders)
    compute_rate l = (fromIntegral $ length $ destroy_rate l) / (fromIntegral $ length l) :: Double
