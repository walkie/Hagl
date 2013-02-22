{-# LANGUAGE TypeFamilies #-}

-- | This module describes an abstract representation of games in terms
--   of transitions between nodes.  Each node consists of a game state
--   and an action to perform (e.g. a player must make a decision).
--   Transitions are determined by moves and games end when a payoff
--   node is reached.
module Hagl.Game where

import Data.List (intersperse)

import Hagl.Lists

--
-- * Game graphs
--

-- | A node consists of a game state and an action to perform.
type Node s mv = (s, Action mv)

-- | The action to perform at a given node.
data Action mv =
    Decision PlayerID -- ^ A decision by the indicated player.
  | Chance (Dist mv)  -- ^ A move based on a probability distribution.
  | Payoff Payoff     -- ^ A terminating payoff node.
  deriving Eq

-- | The state at a given node.
nodeState :: Node s mv -> s
nodeState = fst

-- | The action at a given node.
nodeAction :: Node s mv -> Action mv
nodeAction = snd

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

-- | An explicit representation of a game graph.
data GameGraph s mv = GameGraph {
    -- | The game state and action associated with this location.
  graphNode       :: Node s mv,
    -- | The transition function from this location to another.
  graphTransition :: mv -> GameGraph s mv
}

-- Game instance
instance Game (GameGraph s mv) where
  type State (GameGraph s mv) = (s, mv -> GameGraph s mv)
  type Move  (GameGraph s mv) = mv
  start (GameGraph (s,a) t) = ((s,t),a)
  transition _ ((_,t),a) mv = start (t mv)


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


-- * Payoffs
--

-- | Payoffs are represented as a list of `Float` values
--   where each value corresponds to a particular player.  While the type
--   of payoffs could be generalized, this representation supports both
--   cardinal and ordinal payoffs while being easy to work with.
type Payoff = ByPlayer Float


-- ** Smart constructors
--

-- | Payoff where all players out of n score payoff a, except player p, who scores b.
allBut :: Int -> PlayerID -> Float -> Float -> Payoff
allBut n p a b = ByPlayer $ replicate (p-1) a ++ b : replicate (n-p) a

-- | Add two payoffs.
addPayoffs :: Payoff -> Payoff -> Payoff
addPayoffs (ByPlayer as) (ByPlayer bs) = ByPlayer (zipWith (+) as bs)

-- | Zero-sum payoff where player w wins (scoring n-1) 
--   and all other players lose (scoring -1).
winner :: Int -> PlayerID -> Payoff
winner n w = allBut n w (-1) (fromIntegral n - 1)

-- | Zero-sum payoff where player w loses (scoring -n+1)
--   and all other players, out of np, win (scoring 1).
loser :: Int -> PlayerID -> Payoff
loser n l = allBut n l 1 (1 - fromIntegral n)

-- | Zero-sum payoff where all players tie.  Each player scores 0.
tie :: Int -> Payoff
tie n = ByPlayer (replicate n 0)


-- ** Pretty printing
--

-- | Concatenate a sequence of elements, separated by commas.
showSeq :: [String] -> String
showSeq = concat . intersperse ","

-- | Pretty print floats as integers, when possible.
showFloat :: Float -> String
showFloat f | f == fromIntegral i = show i
            | otherwise           = show f
  where i = floor f

-- | String representation of a Payoff.
showPayoff :: Payoff -> String
showPayoff (ByPlayer vs) = showSeq (map showFloat vs)

-- | Bracketed string representation of a Payoff.
showPayoffAsList :: Payoff -> String
showPayoffAsList p = "[" ++ showPayoff p ++ "]"

instance Show mv => Show (Action mv) where
  show (Decision p) = "Player " ++ show p
  show (Chance d)   = "Chance " ++ show d
  show (Payoff p)   = showPayoffAsList p
