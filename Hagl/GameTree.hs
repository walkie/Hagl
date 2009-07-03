{-# LANGUAGE FlexibleContexts #-}
module Hagl.GameTree where

import Data.Maybe (fromMaybe)
import Data.Tree  (Tree(..), drawTree)

import Hagl.Core

----------------
-- Game Trees --
----------------

type Edge mv = (mv, GameTree mv)

data GameTree mv = Decision PlayerIx [Edge mv] -- decision made by a player
                 | Chance (Dist (Edge mv))     -- random move from distribution
                 | Payoff Payoff               -- terminating payoff
                 deriving Eq

-- The moves available from a node.
movesFrom :: GameTree mv -> [mv]
movesFrom (Decision _ es) = [m | (m,_) <- es]
movesFrom (Chance d) = [m | (_,(m,_)) <- d]
movesFrom _ = []

-- The immediate children of a node.
children :: GameTree mv -> [GameTree mv]
children (Decision _ es) = [n | (_,n) <- es]
children (Chance d) = [n | (_,(_,n)) <- d]
children _ = []

doMove :: (Eq mv, Show mv) => mv -> GameTree mv -> GameTree mv
doMove m t = case t of
    Decision _ es -> look es
    Chance d      -> look (map snd d)
  where look = fromMaybe (error ("move not found: " ++ show m)) . lookup m

-- Nodes in BFS order.
bfs :: GameTree mv -> [GameTree mv]
bfs t = let b [] = []
            b ns = ns ++ b (concatMap children ns)
        in b [t]

-- Nodes in DFS order.
dfs :: GameTree mv -> [GameTree mv]
dfs t = t : concatMap dfs (children t)

-- The highest numbered player in this finite game tree.
maxPlayer :: GameTree mv -> Int
maxPlayer t = foldl1 max $ map player (dfs t)
  where player (Decision p _) = p
        player _ = 0

---------------
-- Instances --
---------------

instance Show mv => Show (GameTree mv) where
  show t = condense (drawTree (tree "" t))
    where str (Decision p es) = "Player " ++ show p
          str (Chance d) = "Chance"
          str (Payoff p) = showPayoffAsList p
          sub (Decision p es) = [tree (show m ++ " -> ") t | (m,t) <- es]
          sub (Chance d) = [tree (show i ++ " * " ++ show m ++ " -> ") t | (i,(m,t)) <- d]
          sub (Payoff _) = []
          tree pre t = Node (pre ++ str t) (sub t)
          condense s = let empty = not . all (\c -> c == ' ' || c == '|')
                       in unlines $ filter empty $ lines s
