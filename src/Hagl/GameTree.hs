{-# LANGUAGE FlexibleInstances #-}

-- | Each node in a game tree consists of the current game state and an
--   action to perform (e.g. a player must make a decision). Edges between
--   nodes are determined by moves. For @Finite@ game trees, the edges are
--   given explicitly as a list. For @NonFinite@ game trees, the edges are
--   defined implicitly by a partial function.
module Hagl.GameTree where

import qualified Data.Tree as DT (Tree(..), drawTree)

import Hagl.List
import Hagl.Payoff


--
-- * Representation
--

-- ** Nodes and actions

-- | A node in a game tree consists of the current game state and the
--   action to perform. The type parameter @e@ indicates the type of
--   outgoing edge from @Decision@ and @Chance@ nodes, and is typically
--   either @Finite@ or @NonFinite@.
data GameTree e s mv = GameTree {
     treeState  :: s
   , treeAction :: GameAction e s mv
}

-- | The action to perform at a given node.
data GameAction e s mv
   = Decision PlayerID (e s mv)  -- ^ A decision by the indicated player.
   | Chance (Dist mv)  (e s mv)  -- ^ A move based on a probability distribution.
                                 --   This corresponds to moves by Chance or
                                 --   Nature in game theory texts.
   | Payoff Payoff               -- ^ A terminating payoff node.

instance Show mv => Show (GameAction e s mv) where
  show (Decision p _) = "Player " ++ show p
  show (Chance d _)   = "Chance " ++ show d
  show (Payoff p)     = showPayoffAsList p


-- ** Edges

-- | Game trees differ in how they represent the outbound edges from a node,
--   which associate moves with subtrees.
class Edges e where
  
  -- | Get the subtree associated with a given move and state.
  followEdge :: Eq mv => e s mv -> s -> mv -> Maybe (GameTree e s mv)

-- | The edges in a finite game tree associate a finite set of discrete moves
--   with subtrees using an association list.
newtype Finite s mv = Finite [(mv, GameTree Finite s mv)]
  deriving Show

instance Edges Finite where
  followEdge (Finite es) _ mv = lookup mv es

-- | The edges in a non-finite game tree are a partial function from a
--   potentially infinite domain of moves to subtrees.
newtype NonFinite s mv = NonFinite (s -> mv -> Maybe (GameTree NonFinite s mv))

instance Edges NonFinite where
  followEdge (NonFinite f) = f


--
-- * Finite game trees
--

-- | The outgoing edges from a node in a finite game tree.
treeEdges :: GameTree Finite s mv -> [(mv, GameTree Finite s mv)]
treeEdges t = case treeAction t of
    Decision _ (Finite es) -> es
    Chance   _ (Finite es) -> es
    _ -> []

-- | The available moves from a node in a finite game tree.
treeMoves :: GameTree Finite s mv -> [mv]
treeMoves = map fst . treeEdges

-- | The immediate children of a node in a finite game tree.
children :: GameTree Finite s mv -> [GameTree Finite s mv]
children = map snd . treeEdges

-- | Game tree nodes in BFS order.
bfs :: GameTree Finite s mv -> [GameTree Finite s mv]
bfs t = bfs' [t]
  where bfs' [] = []
        bfs' ns = ns ++ bfs' (concatMap children ns)

-- | Game tree nodes in DFS order.
dfs :: GameTree Finite s mv -> [GameTree Finite s mv]
dfs t = t : concatMap dfs (children t)

-- | The highest numbered player in this game tree.
maxPlayer :: GameTree Finite s mv -> Int
maxPlayer t = maximum $ map playerID (dfs t)
  where
    playerID t | Decision p _ <- treeAction t = p
               | otherwise = 0


--
-- * Pretty printing
--

-- | A nice string representation of a finite game tree.
drawTree :: Show mv => GameTree Finite s mv -> String
drawTree = condense . DT.drawTree . tree ""
  where
    condense = unlines . filter empty . lines
    empty    = not . all (\c -> c == ' ' || c == '|')
    tree s t = DT.Node (s ++ show (treeAction t))
      [tree (show m ++ " -> ") t | (m,t) <- treeEdges t]

instance Show mv => Show (GameTree Finite s mv) where
  show = drawTree
