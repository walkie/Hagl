{-# LANGUAGE PatternGuards, TypeFamilies #-}

-- | A representation of games as trees. Each node has an associated state,
--   action, and outbound edges.
module Hagl.GameTree where

import Data.Maybe (fromMaybe)
import qualified Data.Tree as DT (Tree(..), drawTree)

import Hagl.Lists
import Hagl.Payoff
import Hagl.Game

--
-- * Representation
--

-- | A location in a game tree consists of a node and some representation of
--   outbound edges.
class GameTree t where
  -- | The state and action of this location.
  treeNode :: t s mv -> Node s mv
  -- | An implicit representation of the outbound edges.
  treeMove :: Eq mv => t s mv -> mv -> Maybe (t s mv)

-- | The state at a given tree node.
treeState :: GameTree t => t s mv -> s
treeState = nodeState . treeNode

-- | The action at a given tree node.
treeAction :: GameTree t => t s mv -> Action mv
treeAction = nodeAction . treeNode

-- | An edge represents a single transition from one location in a game tree
--   to another, via a move.
type Edge s mv = (mv, Discrete s mv)

-- | The move assicated with an edge.
edgeMove :: Edge s mv -> mv
edgeMove = fst

-- | The destination of an edge.
edgeDest :: Edge s mv -> Discrete s mv
edgeDest = snd

-- | A discrete game tree provides a finite list of outbound edges from
--   each location, providing a discrete and finite list of available moves.
data Discrete s mv = Discrete {
  -- | The game state and action associated with this location.
  dtreeNode  :: Node s mv,
  -- | The outbound edges.
  dtreeEdges :: [Edge s mv]
} deriving Eq

-- | The edges of a continuous game tree are defined by a transition function
--   from moves to children, supporting a potentially infinite or continuous
--   set of available moves.
data Continuous s mv = Continuous {
  -- | The game state and action associated with this location.
  ctreeNode :: Node s mv,
  -- | The transition function.
  ctreeMove :: mv -> Maybe (Continuous s mv)
}

instance GameTree Discrete where
  treeNode = dtreeNode
  treeMove t m = lookup m (dtreeEdges t)

instance GameTree Continuous where
  treeNode = ctreeNode
  treeMove = ctreeMove


--
-- * Generating game trees
--

-- | Generate the game tree for a discrete game.
gameTree :: DiscreteGame g => g -> Discrete (State g) (Move g)
gameTree g = gameTreeFrom g (start g)

-- | Generate the game tree from a particular location in a discrete game.
gameTreeFrom :: DiscreteGame g => g -> Node (State g) (Move g) -> Discrete (State g) (Move g)
gameTreeFrom g n = Discrete n [(m, gameTreeFrom g (transition g n m)) | m <- movesFrom g n]

-- | Build a tree for a state-based game.
stateGameTree :: (s -> PlayerID) -- ^ Whose turn is it?
              -> (s -> Bool)     -- ^ Is the game over?
              -> (s -> [mv])     -- ^ Available moves.
              -> (s -> mv -> s)  -- ^ Execute a move and return the new state.
              -> (s -> Payoff)   -- ^ Payoff for this (final) state.
              -> s               -- ^ The current state.
              -> Discrete s mv
stateGameTree who end moves exec pay = tree
  where tree s | end s     = Discrete (s, Payoff (pay s)) []
               | otherwise = Discrete (s, Decision (who s)) [(m, tree (exec s m)) | m <- moves s]


--
-- * Simple queries
--

-- | Get the PlayerID corresponding to a decision node.
playerID :: GameTree t => t s mv -> Maybe PlayerID
playerID t = case treeAction t of
               Decision p -> Just p
               _          -> Nothing

-- | The highest numbered player in this finite game tree.
maxPlayer :: Discrete s mv -> Int
maxPlayer t = maximum $ map (fromMaybe 0 . playerID) (dfs t)

-- | The immediate children of a node.
children :: Discrete s mv -> [Discrete s mv]
children = map edgeDest . dtreeEdges

-- | Get a particular child node by following the edge labeled with mv.
child :: Eq mv => mv -> Discrete s mv -> Discrete s mv
child mv t | Just t' <- lookup mv (dtreeEdges t) = t'
child _  _ = error "GameTree.child: invalid move"


--
-- * Traversals
--

-- | Nodes in BFS order.
bfs :: Discrete s mv -> [Discrete s mv]
bfs t = bfs' [t]
  where bfs' [] = []
        bfs' ns = ns ++ bfs' (concatMap children ns)

-- | Nodes in DFS order.
dfs :: Discrete s mv -> [Discrete s mv]
dfs t = t : concatMap dfs (children t)


--
-- * Pretty printing
--

-- | A nice string representation of a game tree.
drawTree :: Show mv => Discrete s mv -> String
drawTree = condense . DT.drawTree . tree ""
  where
    condense = unlines . filter empty . lines
    empty    = not . all (\c -> c == ' ' || c == '|')
    tree s t@(Discrete (_,a) _) = DT.Node (s ++ show a)
                                  [tree (show m ++ " -> ") t | (m,t) <- dtreeEdges t]

instance Show mv => Show (Discrete s mv) where
  show = drawTree
