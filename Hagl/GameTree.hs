{-# LANGUAGE PatternGuards #-}

--
-- Functions for working with and traversing game trees.
--
module Hagl.GameTree where

import Hagl.Base

-- The moves available from a node.
movesFrom :: GameTree s mv -> [mv]
movesFrom = map fst . edges

-- The immediate children of a node.
children :: GameTree s mv -> [GameTree s mv]
children = map snd . edges

-- Get a particular child node by following the edge labeled with mv.
child :: Eq mv => mv -> GameTree s mv -> GameTree s mv
child mv t | Just t' <- lookup mv (edges t) = t'
child _  _ = error "GameTree.child: invalid move"

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
  where player (GameTree _ (Internal (Decision p) _)) = p
        player _                                      = 0
