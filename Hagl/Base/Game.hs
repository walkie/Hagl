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

treeNode :: GameTree s mv -> Node s mv
treeNode (GameTree _ n) = n

treeState :: GameTree s mv -> s
treeState (GameTree s _) = s

edges :: GameTree s mv -> [Edge s mv]
edges (GameTree _ (Internal _ es)) = es
edges _                            = []
