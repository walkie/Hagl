{-# LANGUAGE TypeFamilies #-}

-- | A representation of games as trees. Each node has an associated state,
--   action, and outbound edges.
module Hagl.GameTree where

import Data.Maybe (fromMaybe)
import qualified Data.Tree as DT (Tree(..), drawTree)

import Hagl.Lists
import Hagl.Game

--
-- * Representation
--

-- | An edge represents a single transition from one location in a game tree
--   to another, via a move.
type Edge s mv = (mv, GameTree s mv)

-- | A location in a game tree consists of an action to perform
--   and a discrete set of outbound edges.
data GameTree s mv = GameTree {
  nodeState :: s,           -- ^ The game state associated with this location.
  action    :: Action mv,   -- ^ The action to perform at this location.
  edges     :: [Edge s mv]  -- ^ The outbound edges.
} deriving Eq

-- Instances

instance Eq mv => Game (GameTree s mv) where
  type State (GameTree s mv) = (s,[Edge s mv])
  type Move  (GameTree s mv) = mv
  start (GameTree s a es) = ((s,es),a)
  transition _ ((_,es),_) mv
    | Just (GameTree s a es') <- lookup mv es = ((s,es'),a)
    | otherwise = error "GameTree: invalid move!"

instance Eq mv => DiscreteGame (GameTree s mv) where
  movesFrom _ ((_,es),_) = map fst es


--
-- * Generating game trees
--

-- | Generate the game tree for a discrete game.
gameTree :: DiscreteGame g => g -> GameTree (State g) (Move g)
gameTree g = gameTreeFrom g (start g)

-- | Generate the game tree from a particular location in a discrete game.
gameTreeFrom :: DiscreteGame g => g -> Node (State g) (Move g) -> GameTree (State g) (Move g)
gameTreeFrom g n@(s,a) = GameTree s a [(m, gameTreeFrom g (transition g n m)) | m <- movesFrom g n]

-- | Build a tree for a state-based game.
stateGameTree :: (s -> PlayerID) -- ^ Whose turn is it?
              -> (s -> Bool)     -- ^ Is the game over?
              -> (s -> [mv])     -- ^ Available moves.
              -> (s -> mv -> s)  -- ^ Execute a move and return the new state.
              -> (s -> Payoff)   -- ^ Payoff for this (final) state.
              -> s               -- ^ The current state.
              -> GameTree s mv
stateGameTree who end moves exec pay init = tree init
  where tree s | end s     = GameTree s (Payoff (pay s)) []
               | otherwise = GameTree s (Decision (who s)) [(m, tree (exec s m)) | m <- moves s]


--
-- * Simple queries
--

-- | Get the PlayerID corresponding to a decision node.
playerID :: GameTree s mv -> Maybe PlayerID
playerID (GameTree _ (Decision p) _) = Just p
playerID _                           = Nothing

-- | The highest numbered player in this finite game tree.
maxPlayer :: GameTree s mv -> Int
maxPlayer t = foldl1 max $ map (fromMaybe 0 . playerID) (dfs t)

-- | The immediate children of a node.
children :: GameTree s mv -> [GameTree s mv]
children = map snd . edges

-- | Get a particular child node by following the edge labeled with mv.
child :: Eq mv => mv -> GameTree s mv -> GameTree s mv
child mv t | Just t' <- lookup mv (edges t) = t'
child _  _ = error "GameTree.child: invalid move"


--
-- * Traversals
--

-- | Nodes in BFS order.
bfs :: GameTree s mv -> [GameTree s mv]
bfs t = bfs' [t]
  where bfs' [] = []
        bfs' ns = ns ++ bfs' (concatMap children ns)

-- | Nodes in DFS order.
dfs :: GameTree s mv -> [GameTree s mv]
dfs t = t : concatMap dfs (children t)


--
-- * Pretty printing
--

-- | A nice string representation of a game tree.
drawTree :: Show mv => GameTree s mv -> String
drawTree = condense . DT.drawTree . tree ""
  where
    condense = unlines . filter empty . lines
    empty    = not . all (\c -> c == ' ' || c == '|')
    tree s t@(GameTree _ a _) = DT.Node (s ++ show a)
                                [tree (show m ++ " -> ") t | (m,t) <- edges t]

instance Show mv => Show (GameTree s mv) where
  show = drawTree
