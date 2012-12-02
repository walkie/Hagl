{-# LANGUAGE TypeFamilies #-}

-- | This module describes an abstract representation of games in terms
--   of transitions between nodes.  Each node consists of a game state
--   and an action to perform (e.g. a player must make a decision).
--   Transitions are determined by moves and games end when a payoff
--   node is reached.
module Hagl.Base.Game where

import Hagl.Base.List
import Hagl.Base.Payoff

--
-- * Abstract Game Representation
--

-- | A node consists of a game state and an action to perform.
type Node s mv = (s, Action mv)

-- | The action to perform at a given node.
data Action mv =
    Decision PlayerID -- ^ A decision by the indicated player.
  | Chance (Dist mv)  -- ^ A move based on a probability distribution.
  | Payoff Payoff     -- ^ A terminating payoff node.
  deriving Eq


-- | The most general class of games. Movement between nodes is captured
--   by a transition function. This supports discrete and continuous, 
--   finite and infinite games.
class Game g where
  
  -- | The type of state maintained throughout the game (use @()@ for stateless games).
  type State g

  -- | The type of moves that may be played during the game.
  type Move g
  
  -- | The initial node.
  start :: g -> Node (State g) (Move g)
  
  -- | The transition function.
  transition :: g -> Node (State g) (Move g) -> Move g -> Node (State g) (Move g)


--
-- * Discrete Games
--

-- | Discrete games have a discrete set of moves available at each node.
--   Note that discrete games may still be inifinite.
class Game g => DiscreteGame g where
  
  -- | The available moves from a given node.
  movesFrom :: g -> Node (State g) (Move g) -> [Move g]
