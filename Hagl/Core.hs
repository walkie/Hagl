{-# OPTIONS_GHC -fglasgow-exts #-}

-- This module contains stuff that almost every Hagl file will need to import.
module Hagl.Core where

import Control.Monad.State hiding (State)
import Data.Function (on)

import Hagl.List

---------------------
-- Game Definition --
---------------------

type PlayerIx = Int
type Payoff = ByPlayer Float

class Game g where
  type Move g
  type State g
  initState :: g -> State g
  runGame   :: ExecM g Payoff

---------------------
-- Execution State --
---------------------

-- Game Execution State
data Exec g = Exec {
  _game       :: g,              -- game definition
  _players    :: [Player g],     -- players active in game
  _gameState  :: State g,        -- the current state of the game
  _playerIx   :: Maybe PlayerIx, -- the index of the currently deciding player
  _transcript :: Transcript g,   -- moves so far this iteration (newest at head)
  _history    :: History g,      -- a summary of each iteration
  _numMoves   :: ByPlayer Int    -- number of moves played by each player
}

initExec :: Game g => g -> [Player g] -> Exec g
initExec g ps = Exec g ps (initState g) Nothing [] (ByGame []) ms
  where ms = ByPlayer (replicate (length ps) 0)

-- History
type Moved g      = (Maybe PlayerIx, Move g)
type Transcript g = [Moved g]

type History g = ByGame (Transcript g, Summary g)
type Summary g = (ByPlayer (ByTurn (Move g)), Payoff)

_transcripts :: History g -> ByGame (Transcript g)
_transcripts = fmap fst

_summaries :: History g -> ByGame (Summary g)
_summaries = fmap snd

_moves :: Summary g -> ByPlayer (ByTurn (Move g))
_moves = fst

_payoff :: Summary g -> ByPlayer Float
_payoff = snd

-------------
-- Players --
-------------

type Name = String

data Player g = forall s. Player Name s (Strategy g s)
              | Name ::: Strategy g ()

infix 0 :::

name :: Player g -> Name
name (Player n _ _) = n
name (n ::: _) = n

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
data StratM g s a = StratM { unS :: StateT s (ExecM g) a }
type Strategy g s = StratM g s (Move g)

class (Monad m, MonadIO m) => GameM m g | m -> g where
  getExec :: m (Exec g)

update :: MonadState s m => (s -> s) -> m s
update f = modify f >> get

evalGame :: Game g => g -> [Player g] -> ExecM g a -> IO a
evalGame g ps f = evalStateT (unE f) (initExec g ps)

execGame :: Game g => g -> [Player g] -> ExecM g a -> IO (Exec g)
execGame g ps f = execStateT (unE f) (initExec g ps)

runStrategy :: Player g -> ExecM g (Move g, Player g)
runStrategy (Player n s f) = do (m, s') <- runStateT (unS f) s
                                return (m, Player n s' f)

-- ExecM instances
instance Monad (ExecM g) where
  return = ExecM . return
  (ExecM x) >>= f = ExecM (x >>= unE . f)

instance MonadState (Exec g) (ExecM g) where
  get = ExecM get
  put = ExecM . put

instance MonadIO (ExecM g) where
  liftIO = ExecM . liftIO

instance GameM (ExecM g) g where
  getExec = ExecM get

-- StratM instances
instance Monad (StratM g s) where
  return = StratM . return
  (StratM x) >>= f = StratM (x >>= unS . f)

instance MonadState s (StratM g s) where
  get = StratM get
  put = StratM . put

instance MonadIO (StratM g s) where
  liftIO = StratM . liftIO

instance GameM (StratM g s) g where
  getExec = StratM (lift getExec)

debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn
