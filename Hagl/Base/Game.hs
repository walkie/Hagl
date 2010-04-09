{-# LANGUAGE TypeFamilies #-}

module Hagl.Base.Game where

import Hagl.Base.List


---------------------
-- Game Definition --
---------------------

class Game g where
  type Move g
  type State g
  gameTree :: g -> Int -> GameTree (State g) (Move g)


----------------
-- Game Trees --
----------------

type PlayerIx  = Int
type Payoff    = ByPlayer Float
type Edge s mv = (mv, GameTree s mv)

data GameTree s mv = GameTree s (Node s mv) deriving Eq

data Node s mv = Internal (Decision mv) [Edge s mv] -- internal node
               | Payoff Payoff                      -- terminating payoff
               deriving Eq

data Decision mv = Decision PlayerIx -- decision made by a player
                 | Chance (Dist mv)  -- decision based on a random distribution
                 deriving Eq


--------------------------
-- Game Tree Navigation --
--------------------------

treeNode :: GameTree s mv -> Node s mv
treeNode (GameTree _ n) = n

treeState :: GameTree s mv -> s
treeState (GameTree s _) = s

edges :: GameTree s mv -> [Edge s mv]
edges = e . treeNode
  where e (Internal _ es) = es
        e _               = []

playerIx :: GameTree s mv -> Maybe PlayerIx
playerIx = ix . treeNode
  where ix (Internal (Decision p) _) = Just p
        ix _                         = Nothing


-----------------------
-- Game Construction --
-----------------------

-- | The next player index out of n players.
nextPlayer :: Int -> PlayerIx -> PlayerIx
nextPlayer n p | p >= n    = 1
               | otherwise = p + 1

-- | Payoff where all players out of n score payoff a, except player p, who scores b.
allBut :: Int -> PlayerIx -> Float -> Float -> Payoff
allBut n p a b = ByPlayer $ replicate (p-1) a ++ b : replicate (n-p) a

--
-- Zero-sum payoffs
--

-- | Zero-sum payoff where player w wins (scoring n-1) 
--   and all other players lose (scoring -1).
winner :: Int -> PlayerIx -> Payoff
winner n w = allBut n w (-1) (fromIntegral n - 1)

-- | Zero-sum payoff where player w loses (scoring -n+1)
--   and all other players, out of np, win (scoring 1).
loser :: Int -> PlayerIx -> Payoff
loser n l = allBut n l 1 (1 - fromIntegral n)

-- | Zero-sum payoff where all players tie.  Each player scores 0.
tie :: Int -> Payoff
tie n = ByPlayer (replicate n 0)
