{-# LANGUAGE FlexibleContexts,
             FlexibleInstances,
             PatternGuards,
             TypeFamilies,
             UndecidableInstances #-}

-- | All games in Hagl can be reduced to one of two kinds of game trees.
--   Each node in a game tree consists of a game state and an action to
--   perform (e.g. a player must make a decision).  Edges between nodes
--   are determined by moves.  For `Discrete` game trees, the edges are
--   given explicitly as a list.  For `Continuous` game trees, the edges
--   are defined implicitly by a function.  Games end when a `Payoff`
--   node is reached.
module Hagl.Game where

import Data.Maybe (fromMaybe)
import qualified Data.Tree as DT (Tree(..), drawTree)

import Hagl.Lists
import Hagl.Payoff

--
-- * Games
--

-- | A game has several associated types describing whether the game
--   is discrete or continuous, what type of state it maintains, and
--   the type of moves on edges.  The function `gameTree` is used
--   to get the game tree representation of this game, which is used
--   internally by Hagl for execution and analysis.
class GameTree (TreeType g) => Game g where

  -- The type of the corresponding game tree--either `Discrete` or `Continuous`.
  type TreeType g :: * -> * -> *
  
  -- | The type of state maintained throughout the game (use @()@ for stateless games).
  type State g

  -- | The type of moves that may be played during the game.
  type Move g
  
  -- | A representation of this game as a game tree.
  gameTree :: g -> (TreeType g) (State g) (Move g)

-- | Captures all games whose `TreeType` is `Discrete`.  Do not instantiate 
--   this class directly!
class (Game g, TreeType g ~ Discrete) => DiscreteGame g
instance (Game g, TreeType g ~ Discrete) => DiscreteGame g

-- | Captures all games whose `TreeType` is `Continuous`.  Do not instantiate 
--   this class directly!
class (Game g, TreeType g ~ Continuous) => ContinuousGame g
instance (Game g, TreeType g ~ Continuous) => ContinuousGame g


--
-- * Nodes and actions
--

-- | A node consists of a game state and an action to perform.
type Node s mv = (s, Action mv)

-- | The action to perform at a given node.
data Action mv =
    -- | A decision by the indicated player.
    Decision PlayerID
    -- | A move based on a probability distribution. This corresponds to
    --   moves by Chance or Nature in game theory texts.
  | Chance (Dist mv)
    -- | A terminating payoff node.
  | Payoff Payoff
  deriving Eq

-- | The state at a given node.
nodeState :: Node s mv -> s
nodeState = fst

-- | The action at a given node.
nodeAction :: Node s mv -> Action mv
nodeAction = snd

instance Show mv => Show (Action mv) where
  show (Decision p) = "Player " ++ show p
  show (Chance d)   = "Chance " ++ show d
  show (Payoff p)   = showPayoffAsList p


--
-- * Game trees
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

-- | Get the PlayerID corresponding to a decision node.
playerID :: GameTree t => t s mv -> Maybe PlayerID
playerID t = case treeAction t of
               Decision p -> Just p
               _          -> Nothing


-- ** Discrete game trees
--

-- | A discrete game tree provides a finite list of outbound edges from
--   each location, providing a discrete and finite list of available moves.
data Discrete s mv = Discrete {
  -- | The game state and action associated with this location.
  dtreeNode  :: Node s mv,
  -- | The outbound edges.
  dtreeEdges :: [Edge s mv]
} deriving Eq

-- | An edge represents a single transition from one location in a discrete
--   game tree to another, via a move.
type Edge s mv = (mv, Discrete s mv)

-- | The move assicated with an edge.
edgeMove :: Edge s mv -> mv
edgeMove = fst

-- | The destination of an edge.
edgeDest :: Edge s mv -> Discrete s mv
edgeDest = snd

-- | The available moves from a given location.
movesFrom :: Discrete s mv -> [mv]
movesFrom = map edgeMove . dtreeEdges

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

instance GameTree Discrete where
  treeNode = dtreeNode
  treeMove t m = lookup m (dtreeEdges t)

instance Game (Discrete s mv) where
  type TreeType (Discrete s mv) = Discrete
  type State (Discrete s mv) = s
  type Move (Discrete s mv) = mv
  gameTree = id


-- ** Continuous game trees
--

-- | The edges of a continuous game tree are defined by a transition function
--   from moves to children, supporting a potentially infinite or continuous
--   set of available moves.
data Continuous s mv = Continuous {
  -- | The game state and action associated with this location.
  ctreeNode :: Node s mv,
  -- | The transition function.
  ctreeMove :: mv -> Maybe (Continuous s mv)
}

instance GameTree Continuous where
  treeNode = ctreeNode
  treeMove = ctreeMove

instance Game (Continuous s mv) where
  type TreeType (Continuous s mv) = Continuous
  type State (Continuous s mv) = s
  type Move (Continuous s mv) = mv
  gameTree = id


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
instance Show mv => Show (Continuous s mv) where
  show = show . treeAction
