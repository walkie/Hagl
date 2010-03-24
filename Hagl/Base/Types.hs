{-# LANGUAGE ExistentialQuantification, 
             FlexibleInstances, 
             FunctionalDependencies,
             MultiParamTypeClasses, 
             TypeFamilies #-}

-- This module contains stuff that almost every Hagl file will need to import.
module Hagl.Base.Types where

import Control.Monad.State hiding (State)
import Data.Function (on)

import Hagl.Base.List

----------------
-- Game Trees --
----------------

type PlayerIx  = Int
type Payoff    = ByPlayer Float
type Edge s mv = (mv, GameTree s mv)

data GameTree s mv = GameTree s (Node s mv) deriving Eq

data Node s mv = Internal (Decision mv) [Edge s mv] -- internal node
               | Payoff Payoff                      -- terminating payoff
               deriving Eq

data Decision mv = Decision PlayerIx -- decision made by a player
                 | Chance (Dist mv)  -- decision based on a random distribution
                 deriving Eq

treeNode :: GameTree s mv -> Node s mv
treeNode (GameTree _ n) = n

treeState :: GameTree s mv -> s
treeState (GameTree s _) = s

---------------------
-- Game Definition --
---------------------

class Game g where
  type Move g
  type State g
  gameTree  :: g -> Int -> GameTree (State g) (Move g)

---------------------
-- Execution State --
---------------------

-- Transcripts
type Moved mv      = (Maybe PlayerIx, mv)
type Transcript mv = [Moved mv]

moved :: Decision mv -> mv -> Moved mv
moved (Decision p) mv = (Just p,  mv)
moved (Chance   _) mv = (Nothing, mv)

-- Game Execution State
data Exec g = Exec {
  _game       :: g,                           -- game definition
  _players    :: ByPlayer (Player g),         -- players active in game
  _location   :: GameTree (State g) (Move g), -- the current location in the game tree
  _transcript :: Transcript (Move g),         -- moves played so far (newest at head)
  _numMoves   :: ByPlayer Int                 -- number of moves played by each player
}

initExec :: Game g => g -> [Player g] -> Exec g
initExec g ps = Exec g (ByPlayer ps) (gameTree g np) [] ms
  where ms = ByPlayer (replicate np 0)
        np = length ps

-------------
-- Players --
-------------

type Name = String

data Player g = forall s. Player Name s (Strategy s g)
              | Name ::: Strategy () g

infix 0 :::

name :: Player g -> Name
name (Player n _ _) = n
name (n ::: _)      = n

instance Show (Player g) where
  show = name
instance Eq (Player g) where
  (==) = (==) `on` name
instance Ord (Player g) where
  compare = compare `on` name

-----------------------------------
-- Execution and Strategy Monads --
-----------------------------------

data ExecM g a = ExecM { unE :: StateT (Exec g) IO a }
data StratM s g a = StratM { unS :: StateT s (ExecM g) a }
type Strategy s g = StratM s g (Move g)

class (Game g, Monad m, MonadIO m) => GameM m g | m -> g where
  getExec :: m (Exec g)

evalGame :: Game g => g -> [Player g] -> ExecM g a -> IO a
evalGame g ps f = evalStateT (unE f) (initExec g ps)

execGame :: Game g => g -> [Player g] -> ExecM g a -> IO (Exec g)
execGame g ps f = execStateT (unE f) (initExec g ps)

runStrategy :: Player g -> ExecM g (Move g, Player g)
runStrategy p@(n ::: m) = do 
    mv <- evalStateT (unS m) ()
    return (mv, p)
runStrategy (Player n s f) = do 
    (m, s') <- runStateT (unS f) s
    return (m, Player n s' f)

---------------
-- Instances --
---------------

-- ExecM instances
instance Monad (ExecM g) where
  return = ExecM . return
  (ExecM x) >>= f = ExecM (x >>= unE . f)

instance MonadState (Exec g) (ExecM g) where
  get = ExecM get
  put = ExecM . put

instance MonadIO (ExecM g) where
  liftIO = ExecM . liftIO

instance Game g => GameM (ExecM g) g where
  getExec = ExecM get

-- StratM instances
instance Monad (StratM s g) where
  return = StratM . return
  (StratM x) >>= f = StratM (x >>= unS . f)

instance MonadState s (StratM s g) where
  get = StratM get
  put = StratM . put

instance MonadIO (StratM s g) where
  liftIO = StratM . liftIO

instance Game g => GameM (StratM s g) g where
  getExec = StratM (lift getExec)
