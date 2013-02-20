{-# LANGUAGE ExistentialQuantification, 
             FlexibleInstances, 
             FunctionalDependencies,
             MultiParamTypeClasses #-}

-- | This module defines the game execution and player strategy monads.
module Hagl.Monad where

import Control.Monad.State hiding (State)
import Data.Function (on)

import Hagl.Lists
import Hagl.Game


--
-- * Execution State
--

-- | A record of a single move event.
type MoveEvent mv   = (Maybe PlayerID, mv)

-- | A transcript is a list of move events.
type Transcript mv  = [MoveEvent mv]

-- | A summary of all the moves made by each player during the game.
type MoveSummary mv = ByPlayer (ByTurn mv)

-- | Create a move event from an action and the move made.
moveEvent :: Action mv -> mv -> MoveEvent mv
moveEvent (Decision p) mv = (Just p,  mv)
moveEvent (Chance   _) mv = (Nothing, mv)

-- | Produce a move summary from a transcript.
summarize :: Int -> Transcript mv -> MoveSummary mv
summarize np t = ByPlayer [ByTurn [mv | (mi,mv) <- t, mi == Just p] | p <- [1..np]]

-- | The state of a game execution.
data Exec g = Exec {
  _game        :: g,                       -- ^ Game definition.
  _players     :: ByPlayer (Player g),     -- ^ Players active in the game.
  _location    :: Node (State g) (Move g), -- ^ The current location in the game graph.
  _numMoves    :: ByPlayer Int,            -- ^ Number of moves played by each player
  _transcript  :: Transcript (Move g),     -- ^ Moves played so far (newest at head)
  _finalPayoff :: Maybe Payoff             -- ^ Final payoff, once the game is completed
}

-- | Initial game execution state.
initExec :: Game g => g -> [Player g] -> Exec g
initExec g ps = Exec g (ByPlayer ps) (start g) ms [] Nothing
  where ms = ByPlayer (replicate np 0)
        np = length ps


--
-- * Players
--

-- | The name of the player.
type Name = String

-- | Each player has a name and strategy, and may optionally maintain
--   their own personal state.
data Player g = forall s. Player Name s (Strategy s g)
              | Name ::: Strategy () g

infix 0 :::

-- | Get the name of a player.
name :: Player g -> Name
name (Player n _ _) = n
name (n ::: _)      = n

instance Show (Player g) where
  show = name
instance Eq (Player g) where
  (==) = (==) `on` name
instance Ord (Player g) where
  compare = compare `on` name


--
-- * Execution and Strategy Monads
--

-- | The game execution monad.  A state monad transformer that maintains the
--   game execution state.
data ExecM g a = ExecM  { unE :: StateT (Exec g) IO a }

-- | The strategy monad.  A state monad transformer that maintains the personal
--   state of the player who is playing this strategy, and wraps the game
--   execution monad.  This gives strategies access to the game execution state.
data StratM s g a = StratM { unS :: StateT s (ExecM g) a }

-- | A strategy is a computation in the strategy monad that produces a move.
type Strategy s g = StratM s g (Move g)

-- | Similar to MonadIO for monads that wrap IO, this type class captures all
--   monads that wrap the game execution monad, providing uniform access to the
--   game execution state.
class (Game g, Monad m, MonadIO m) => GameM m g | m -> g where
  getExec :: m (Exec g)

-- | Evaluate a command in the game monad, for a given game and players,
--   returning the result.
evalGame :: Game g => g -> [Player g] -> ExecM g a -> IO a
evalGame g ps f = evalStateT (unE f) (initExec g ps)

-- | Execute a command in the game monad, for a given game and players,
--   returning the execution state.
execGame :: Game g => g -> [Player g] -> ExecM g a -> IO (Exec g)
execGame g ps f = execStateT (unE f) (initExec g ps)

-- | Run the strategy associated with a particular player, producing a move
--   and an updated player state.
runStrategy :: Player g -> ExecM g (Move g, Player g)
runStrategy p@(n ::: m) = do 
    mv <- evalStateT (unS m) ()
    return (mv, p)
runStrategy (Player n s f) = do 
    (mv, s') <- runStateT (unS f) s
    return (mv, Player n s' f)


--
-- * Instances
--

instance Monad (ExecM g) where
  return = ExecM . return
  (ExecM x) >>= f = ExecM (x >>= unE . f)
instance Monad (StratM s g) where
  return = StratM . return
  (StratM x) >>= f = StratM (x >>= unS . f)

instance MonadState (Exec g) (ExecM g) where
  get = ExecM get
  put = ExecM . put
instance MonadState s (StratM s g) where
  get = StratM get
  put = StratM . put

instance MonadIO (ExecM g) where
  liftIO = ExecM . liftIO
instance MonadIO (StratM s g) where
  liftIO = StratM . liftIO

instance Game g => GameM (ExecM g) g where
  getExec = ExecM get
instance Game g => GameM (StratM s g) g where
  getExec = StratM (lift getExec)
