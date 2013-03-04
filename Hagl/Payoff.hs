-- | The outcome of a game is a payoff.  This module provides a simple
--   representation of payoffs, smart constructors for building typical
--   payoffs, and functions for pretty printing them.
module Hagl.Payoff where

import Hagl.Lists

--
-- * Representation
--

-- | Payoffs are represented as a list of `Float` values
--   where each value corresponds to a particular player.  While the type
--   of payoffs could be generalized, this representation supports both
--   cardinal and ordinal payoffs while being easy to work with.
type Payoff = ByPlayer Float


--
-- * Smart constructors
--

-- | Payoff where all players out of n score payoff a, except player p, who scores b.
allBut :: Int -> PlayerID -> Float -> Float -> Payoff
allBut n p a b = ByPlayer $ replicate (p-1) a ++ b : replicate (n-p) a

-- | Add two payoffs.
addPayoffs :: Payoff -> Payoff -> Payoff
addPayoffs (ByPlayer as) (ByPlayer bs) = ByPlayer (zipWith (+) as bs)

-- | Zero-sum payoff where player w wins (scoring n-1) 
--   and all other players lose (scoring -1).
winner :: Int -> PlayerID -> Payoff
winner n w = allBut n w (-1) (fromIntegral n - 1)

-- | Zero-sum payoff where player w loses (scoring -n+1)
--   and all other players, out of np, win (scoring 1).
loser :: Int -> PlayerID -> Payoff
loser n l = allBut n l 1 (1 - fromIntegral n)

-- | Zero-sum payoff where all players tie.  Each player scores 0.
tie :: Int -> Payoff
tie n = ByPlayer (replicate n 0)


--
-- * Pretty printing
--

-- | Pretty print floats as integers, when possible.
showFloat :: Float -> String
showFloat f | f == fromIntegral i = show i
            | otherwise           = show f
  where i = floor f

-- | String representation of a Payoff.
showPayoff :: Payoff -> String
showPayoff (ByPlayer vs) = showSeq (map showFloat vs)

-- | Bracketed string representation of a Payoff.
showPayoffAsList :: Payoff -> String
showPayoffAsList p = "[" ++ showPayoff p ++ "]"
