{-# LANGUAGE TypeFamilies #-}

module Hagl.Base.Game where

import Hagl.Base.List


--
-- * Game Classes
--

-- | The most general class of games.  Instances must provide a translation
--   into the generic game tree representation.
class Game g where
  
  -- | The type of state maintained throughout the game (use @()@ for stateless games).
  type State g

  -- | The type of moves that may be played during the game.
  type Move g
  
  -- | The type of payoffs awarded at the end of the game.
  type Payoff g

  start :: g -> Node (State g) (Move g)
  
  -- | Get the game tree representing this game.
  gameTree :: g -> Int -> GameTree (State g) (Move g)


-- | Finite games have a finite number of moves available at each point
--   in the game tree.  Note that finite games may still be infinitely long.
class Game g => FiniteGame g where
  
  -- | The moves that are available from the given position in the tree.
  movesFrom :: g -> GameTree (State g) (Move g) -> [Move g]


--
-- * Game Trees
--

-- | Payoffs are awarded at the end of a game. A point value for each player.
type Payoff = ByPlayer Float

-- | Each position in the game tree is represented by a `Node` and has an 
--   associated state.
data GameTree s mv = GameTree s (Node s mv) deriving Eq

-- | A game tree node may be either an internal node representing a decision
--   by a player or by fate, or it can be a payoff node, which ends the game.
data Node s mv =
    Internal (Decision mv) (mv -> GameTree s mv) -- ^ Internal node.
  | Payoff Payoff                                -- ^ Terminating payoff.
  deriving Eq

-- | A decision node represents an internal node in a game tree and
--   is either a decision by a player or a move of chance based on
--   a probability distribution.
data Decision mv =
    Decision PlayerID -- ^ Decision made by a player.
  | Chance (Dist mv)  -- ^ Decision based on a distribution.
  deriving Eq

instance Game (GameTree s mv) where
  type Move (GameTree s mv) = mv
  type State (GameTree s mv) = s
  gameTree g _ = g


--
-- ** Game Tree Navigation
--

treeNode :: GameTree s mv -> Node s mv
treeNode (GameTree _ n) = n

treeState :: GameTree s mv -> s
treeState (GameTree s _) = s

edges :: GameTree s mv -> [Edge s mv]
edges = e . treeNode
  where e (Internal _ es) = es
        e _               = []

movesFrom :: GameTree s mv -> [mv]
movesFrom = map fst . edges

playerIx :: GameTree s mv -> Maybe PlayerID
playerIx = ix . treeNode
  where ix (Internal (Decision p) _) = Just p
        ix _                         = Nothing


--
-- * Smart Constructors
--

-- | Decision node for a stateless game tree.
decision :: PlayerID -> [Edge () mv] -> GameTree () mv
decision p = GameTree () . Internal (Decision p)

-- | Chance node for a stateless game tree.
chance :: Dist mv -> [Edge () mv] -> GameTree () mv
chance d = GameTree () . Internal (Chance d)

-- | Payoff node for a stateless game tree.
payoff :: [Float] -> GameTree () mv
payoff = GameTree () . Payoff . fromList

-- | Build a stateless game tree in which multiple players choose in turn.
decisions :: [(PlayerID,[mv])] -> ([mv] -> GameTree () mv) -> GameTree () mv
decisions ps f = d ps []
  where d []          ns = f (reverse ns)
        d ((p,ms):ps) ns = decision p [(m, d ps (m:ns)) | m <- ms]

-- | Build a tree for a state-based game.
stateGameTree :: (s -> PlayerID) -- ^ Whose turn is it?
              -> (s -> Bool)     -- ^ Is the game over?
              -> (s -> [mv])     -- ^ Available moves.
              -> (s -> mv -> s)  -- ^ Execute a move and return the new state.
              -> (s -> Payoff)   -- ^ Payoff for this (final) state.
              -> s               -- ^ The current state.
              -> GameTree s mv
stateGameTree who end moves exec pay init = tree init
  where tree s | end s     = GameTree s $ Payoff (pay s)
               | otherwise = GameTree s $ Internal (Decision (who s)) [(m, tree (exec s m)) | m <- moves s]

--
-- * Constructing Payoffs
--

-- | The next player index out of n players.
nextPlayer :: Int -> PlayerID -> PlayerID
nextPlayer n p | p >= n    = 1
               | otherwise = p + 1

-- | Payoff where all players out of n score payoff a, except player p, who scores b.
allBut :: Int -> PlayerID -> Float -> Float -> Payoff
allBut n p a b = ByPlayer $ replicate (p-1) a ++ b : replicate (n-p) a

-- | Add two payoffs.
addPayoffs :: Payoff -> Payoff -> Payoff
addPayoffs = dzipWith (+)

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
