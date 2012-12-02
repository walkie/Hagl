-- | Payoffs are represented as a list of `Float` values
--   where each value corresponds to a particular player.  While the type
--   of payoffs could be generalized, this representation supports both
--   cardinal and ordinal payoffs while being easy to work with.
module Hagl.Base.Payoff where

import Hagl.Base.List

--
-- * Payoffs
--

-- | Payoffs are awarded at the end of a game. A point value for each player.
type Payoff = ByPlayer Float

-- | Payoff where all players out of n score payoff a, except player p, who scores b.
allBut :: Int -> PlayerID -> Float -> Float -> Payoff
allBut n p a b = ByPlayer $ replicate (p-1) a ++ b : replicate (n-p) a

-- | Add two payoffs.
addPayoffs :: Payoff -> Payoff -> Payoff
addPayoffs = dzipWith (+)

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
