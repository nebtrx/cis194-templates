{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Num, Ord, Show)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom


----------------------------------------------------------------------
-- Risk
----------------------------------------------------------------------

type Army =
  Int


data Battlefield =
  Battlefield
    { attackers :: Army
    , defenders :: Army
    }


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield a d) = do
       aDiceM <- replicateM (min 3 (a-1)) die
       dDiceM <- replicateM (min 2 d) die
       return $ outcome  b (fmap unDV  aDiceM) (fmap unDV dDiceM)

outcome :: Battlefield -> [Int] -> [Int] -> Battlefield
outcome (Battlefield a d) aDice dDice =  Battlefield (a - aDeads) ( d - dDeads )
        where
              results = zip (sort aDice) (sort dDice)
              aDeads =  foldr (\(x, y) acc -> if x > y
                                                 then acc + 1
                                                 else acc) 0 results
              dDeads = length results  - aDeads



----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

invade :: Battlefield -> Rand StdGen Battlefield
invade b = do
       -- thanks to JC for pointing out in class
       battleResult@ (Battlefield a d) <- battle b
       if d > 0 || a > 1
          then invade b
          else return battleResult


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
            results <- replicateM 1000 $ invade b
            return  (fromInteger  ( foldr (\(Battlefield _ d ) acc -> if d == 0
                                                                         then acc + 1
                                                                         else acc ) 0 results) / 1000.0)
