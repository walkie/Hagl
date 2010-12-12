module Hagl.Minimax (minimax) where

import Control.Monad (liftM2)
import Data.List     (elemIndex)
import Data.Maybe    (fromJust)

import Hagl.Base     hiding (movesFrom)
import Hagl.GameTree


-- Minimax strategy.
minimax :: Game g => Strategy s g
minimax = liftM2 minimax' myIx location

-- Minimax algorithm with alpha-beta pruning. Only defined for games with
-- perfect information and no Chance nodes.
minimax' :: PlayerIx -> GameTree s mv -> mv
minimax' me t = movesFrom t !! fromJust (elemIndex (maximum vals) vals)
  where inf  = 1/0 :: Float
        vals = map (val me (-inf) inf . treeNode) (children t)

-- (used by minimax')
val :: PlayerIx -> Float -> Float -> Node s mv -> Float
val me _ _   (Payoff vs)                            = forPlayer me vs
val me a b n@(Internal (Decision p) es) | a >= b    = ifMe a  b
                                        | otherwise = ifMe a' b'
  where ifMe a b   = if (me == p) then a else b
        mm (a,b) n = let v = val me a b n
                     in ifMe (max a v, b) (a, min b v)
        (a',b')    = foldl mm (a,b) (map (treeNode . snd) es)
