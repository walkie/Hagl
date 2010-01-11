{-# LANGUAGE PatternGuards #-}

module Hagl.GameTree where

import Data.List  (intersperse)
import qualified Data.Tree as DT (Tree(..), drawTree)

import Hagl.List

----------------
-- Game Trees --
----------------

type PlayerIx  = Int
type Payoff    = ByPlayer Float
type Edge s mv = (mv, GameTree s mv)

data GameTree s mv = Node s (Node mv) [Edge s mv] -- internal node
                   | Payoff Payoff                -- terminating payoff
                   deriving Eq

data Node mv = Decision PlayerIx -- decision made by a player
             | Chance (Dist mv)  -- random move from distribution
             deriving Eq

-- The moves available from a node.
movesFrom :: GameTree s mv -> [mv]
movesFrom (Node _ _ es) = map fst es
movesFrom _             = []

-- The immediate children of a node.
children :: GameTree s mv -> [GameTree s mv]
children (Node _ _ es) = map snd es
children _             = []

-- Get a particular child node by following the edge labeled with mv.
child :: Eq mv => mv -> GameTree s mv -> GameTree s mv
child mv (Node _ _ es) | Just t <- lookup mv es = t
child _ _ = error "GameTree.child: invalid move"

----------------
-- Traversing --
----------------

-- Nodes in BFS order.
bfs :: GameTree s mv -> [GameTree s mv]
bfs t = bfs' [t]
  where bfs' [] = []
        bfs' ns = ns ++ bfs' (concatMap children ns)

-- Nodes in DFS order.
dfs :: GameTree s mv -> [GameTree s mv]
dfs t = t : concatMap dfs (children t)

-- The highest numbered player in this finite game tree.
maxPlayer :: GameTree s mv -> Int
maxPlayer t = foldl1 max $ map player (dfs t)
  where player (Node _ (Decision p) _) = p
        player _                       = 0

---------------------
-- Pretty Printing --
---------------------

showFloat :: Float -> String
showFloat f | f == fromIntegral i = show i
            | otherwise           = show f
  where i = floor f

showPayoff :: Payoff -> String
showPayoff = concat . intersperse "," . map showFloat . toList

showPayoffAsList :: Payoff -> String
showPayoffAsList p = "[" ++ showPayoff p ++ "]"

drawTree :: Show mv => GameTree s mv -> String
drawTree = condense . DT.drawTree . tree ""
  where condense = unlines . filter empty . lines
        empty    = not . all (\c -> c == ' ' || c == '|')
        tree s (Payoff p)    = DT.Node (showPayoffAsList p) []
        tree s (Node _ n es) = DT.Node (s ++ show n)
                                       [tree (show m ++ " -> ") t | (m,t) <- es]

instance Show mv => Show (Node mv) where
  show (Decision p) = "Player " ++ show p
  show (Chance d)   = "Chance " ++ show d

instance Show mv => Show (GameTree s mv) where
  show = drawTree
