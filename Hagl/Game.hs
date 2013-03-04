{-# LANGUAGE TypeFamilies #-}

-- | This module describes an abstract representation of games in terms
--   of transitions between nodes.  Each node consists of a game state
--   and an action to perform (e.g. a player must make a decision).
--   Transitions are determined by moves and games end when a payoff
--   node is reached.
module Hagl.Game where

import Hagl.Lists
import Hagl.Payoff

--
-- * Game type class
--

-- | The most general class of games. Movement between nodes is captured
--   by a transition function. This supports discrete and continuous, 
--   finite and infinite games.
--
--   The `Hagl.Game.GameGraph` data type
--   provides a corresponding explicit representation of game graphs.
class Game g where
  
  -- | The type of state maintained throughout the game (use @()@ for stateless games).
  type State g

  -- | The type of moves that may be played during the game.
  type Move g
  
  -- | The initial node.
  start :: g -> Node (State g) (Move g)
  
  -- | The transition function.
  transition :: g -> Node (State g) (Move g) -> Move g -> Node (State g) (Move g)

-- | Get the initial state of a game.
startState :: Game g => g -> State g
startState = nodeState . start

-- | Get the initial action of a game.
startAction :: Game g => g -> Action (Move g)
startAction = nodeAction . start


-- 
-- * Discrete games
--

-- | Discrete games have a discrete set of moves available at each node.
--   Note that discrete games may still be inifinite.
--
--   The explicit representation of a discrete game is as a
--   `Hagl.GameTree.GameTree`.
class Game g => DiscreteGame g where
  
  -- | The available moves from a given node.
  movesFrom :: g -> Node (State g) (Move g) -> [Move g]


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
-- * Game graphs
--

-- | An explicit representation of a game graph.
data GameGraph s mv = GameGraph (Node s mv) (mv -> GameGraph s mv)

-- | The game state and action associated with this location.
graphNode :: GameGraph s mv -> Node s mv
graphNode (GameGraph n _) = n
    
-- | The transition function from this location to another.
graphTransition :: GameGraph s mv -> mv -> GameGraph s mv
graphTransition (GameGraph _ t) = t

-- Game instance
instance Game (GameGraph s mv) where
  type State (GameGraph s mv) = (s, mv -> GameGraph s mv)
  type Move  (GameGraph s mv) = mv
  start (GameGraph (s,a) t) = ((s,t),a)
  transition _ ((_,t),a) mv = start (t mv)
