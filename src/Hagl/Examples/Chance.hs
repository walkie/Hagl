{-# LANGUAGE TupleSections #-}

-- | Some simple games of chance.
module Hagl.Examples.Chance where

import Hagl


--
-- * Dice rolling
--

-- | Simulates a die roll.  A single player game that just rewards the
--   player with a random value from 1 to 6.
die :: Extensive Int
die = chance (map (1,) [1..6]) (map edge [1..6])
  where edge a = (a, pays [fromIntegral a])

-- | Roll a die `n` times and sum the result.
roll n = execGame die ["Total" ::: undefined] (times n >> printScore)


--
-- * Coin flipping
--

-- | A coin is either heads or tails.
data Coin = H | T deriving (Eq, Show)

-- | Simulates flipping a coin.  The result is passed to the argument
--   function, which builds the subsequent game tree.
flipCoin :: (Coin -> Extensive Coin) -> Extensive Coin
flipCoin f = chance [(1,H),(1,T)] [(H, f H), (T, f T)]

-- | Have the player predict which side will come up.  The selection is
--   passed to the argument function, which builds the subsequent game tree.
callCoin :: (Coin -> Extensive Coin) -> Extensive Coin
callCoin f = decision 1 [(H, f H), (T, f T)]

-- | Build a payoff from the player's prediction and the actual flip.  If
--   correct, the player gets 1 point, else -1.
calledIt :: Coin -> Coin -> Extensive Coin
calledIt a b = pays [if a == b then 1 else -1]

-- | Build the simple coin game of predicting which side will come up, then
--   flipping it.
coinGame = callCoin (flipCoin . calledIt)
