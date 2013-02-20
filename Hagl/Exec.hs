{-# LANGUAGE ExistentialQuantification, 
             FlexibleContexts, 
             FlexibleInstances, 
             FunctionalDependencies,
             MultiParamTypeClasses #-}

-- | This module defines the game execution and player strategy monads,
--   and provides functions for executing games.
module Hagl.Exec where

import Control.Monad.State hiding (State)

import Control.Monad (liftM,liftM2)
import Data.Function (on)
import Data.Maybe    (fromMaybe,isJust)

import Hagl.Lists
import Hagl.Game


--
-- * Game Execution
--

-- ** Game Execution State
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


-- ** Game Execution Monad
--

-- | The game execution monad.  A state monad transformer that maintains the
--   game execution state.
data ExecM g a = ExecM  { unE :: StateT (Exec g) IO a }

-- | This type class captures all monads that wrap the game execution monad,
--   providing uniform access to the game execution state.  It is similar to
--   MonadIO for the IO monad.
class (Game g, Monad m, MonadIO m) => GameM m g | m -> g where
  getExec :: m (Exec g)

-- | Evaluate a command with the given game and players in the game
--   execution monad, returning the result.
evalGame :: Game g => g -> [Player g] -> ExecM g a -> IO a
evalGame g ps f = evalStateT (unE f) (initExec g ps)

-- | Execute a command with the given game and players in the game
--   execution monad, returning the execution state.
execGame :: Game g => g -> [Player g] -> ExecM g a -> IO (Exec g)
execGame g ps f = execStateT (unE f) (initExec g ps)


-- ** Execution State Accessors
--

-- | The game being played.
game :: GameM m g => m g
game = liftM _game getExec

-- | The players playing.
players :: GameM m g => m (ByPlayer (Player g))
players = liftM _players getExec

-- | Current location in the game graph.
location :: GameM m g => m (Node (State g) (Move g))
location = liftM _location getExec

-- | The current game state.
state :: GameM m g => m (State g)
state = liftM fst location

-- | Currently available moves.
availMoves :: (DiscreteGame g, GameM m g) => m [Move g]
availMoves = liftM2 movesFrom game location

-- | Transcript of all moves so far.
transcript :: GameM m g => m (Transcript (Move g))
transcript = liftM _transcript getExec

-- | The final payoff, if the game is complete.
finalPayoff :: GameM m g => m (Maybe Payoff)
finalPayoff = liftM _finalPayoff getExec
  
-- | Is the game complete?
isComplete :: GameM m g => m Bool
isComplete = liftM isJust finalPayoff

-- | Transcript of moves so far, separated by player.
move :: GameM m g => m (MoveSummary (Move g))
move = liftM2 summarize numPlayers transcript

-- | The total number of moves each player has played.
numMoves :: GameM m g => m (ByPlayer Int)
numMoves = liftM _numMoves getExec

-- | The index of the currently active player.
myIx :: GameM m g => m PlayerID
myIx = do (_,a) <- location
          case a of
            Decision p -> return p
            _ -> error "Internal error: myIx on non-decision node!"

-- | The currently active player.
me :: GameM m g => m (Player g)
me = liftM2 forPlayer myIx players

-- | The number of players playing the game.
numPlayers :: GameM m g => m Int
numPlayers = liftM (length . toAssocList) players


-- ** Executing Games
--

-- | Process one node in the game tree.
step :: (Game g, Eq (Move g)) => ExecM g (Maybe Payoff)
step = location >>= processNode
  where
    processNode (s,a) = case a of
      Decision i -> performMove a (decide i)
      Chance d   -> performMove a (fromDist d)
      Payoff p   -> givePayoff p
    
    performMove a getMove = do
      m <- getMove
      e <- getExec
      put e { _location   = transition (_game e) (_location e) m,
              _transcript = moveEvent a m : _transcript e,
              _numMoves   = inc a (_numMoves e) }
      return Nothing
    
    inc (Decision p) ns = setForPlayer p (forPlayer p ns + 1) ns
    inc _            ns = ns

    decide i = do
      p <- liftM (forPlayer i) players
      (m,p') <- runStrategy p
      e <- getExec
      put e { _players = setForPlayer i p (_players e) }
      return m
    
    givePayoff p = do
      e <- getExec
      put e { _finalPayoff = Just p }
      return (Just p)
      

-- | Run the game to completion.
finish :: (Game g, Eq (Move g)) => ExecM g Payoff
finish = step >>= maybe finish return

-- | Execute a game with some given players, returning the payoff.
runGame :: (Game g, Eq (Move g)) => g -> [Player g] -> IO Payoff
runGame g ps = evalGame g ps finish


--
-- * Players and Strategies
--

-- ** Players
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


-- * Strategy Monad
--

-- | The strategy monad.  A state monad transformer that maintains the personal
--   state of the player who is playing this strategy, and wraps the game
--   execution monad.  This gives strategies access to the game execution state.
data StratM s g a = StratM { unS :: StateT s (ExecM g) a }

-- | A strategy is a computation in the strategy monad that produces a move.
type Strategy s g = StratM s g (Move g)

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
-- Instances
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
