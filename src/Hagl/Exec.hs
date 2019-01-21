{-# LANGUAGE
      ExistentialQuantification, 
      FlexibleInstances, 
      FunctionalDependencies
  #-}

-- | At a high level, this module defines the execution of games and
--   strategies in Hagl.  The goal is that end-users should be able to write
--   strategies and execute games without understanding the gory monadic
--   details.  For many examples, see "Hagl.Examples".
--
--   Now, about the gory details...  This module defines two monad
--   transformers, `ExecM` and `StratM`.  `ExecM` maintains the current
--   execution state of the game, while `StratM` adds an additional state
--   that is local to each strategy.  Each strategy may define it's own
--   type of state, and strategies cannot affect the state of other strategies.
--
--   At the center of the monad onion is the `IO` monad, allowing both game
--   execution and strategies to do things like print output, get random
--   numbers, and look up the price of tea in Shanghai.
module Hagl.Exec where

import Control.Monad.State hiding (State)

import Control.Monad (liftM,liftM2)
import Data.Function (on)

import Hagl.List
import Hagl.Payoff
import Hagl.Game
import Hagl.History


--
-- * Game execution monad
--

-- | The game execution monad.  A state monad transformer that maintains the
--   game execution state.
newtype ExecM g a = ExecM  { unE :: StateT (Exec g) IO a }
  deriving (Applicative, Functor, Monad)

-- | This type class captures all monads that wrap the game execution monad,
--   providing uniform access to the game execution state.  It is similar to
--   `MonadIO` for the `IO` monad.
class (Game g, Monad m, MonadIO m) => GameM m g | m -> g where
  getExec :: m (Exec g)

-- | Execute a game with some given players, returning the payoff.
runGame :: (Game g, Eq (Move g)) => g -> [Player g] -> IO Payoff
runGame g ps = evalGame g ps finish

-- | Evaluate a command with the given game and players in the game
--   execution monad, returning the result.
evalGame :: Game g => g -> [Player g] -> ExecM g a -> IO a
evalGame g ps f = evalStateT (unE f) (initExec g ps)

-- | Execute a command with the given game and players in the game
--   execution monad, returning the execution state.
execGame :: Game g => g -> [Player g] -> ExecM g a -> IO (Exec g)
execGame g ps f = execStateT (unE f) (initExec g ps)


--
-- * Strategy monad
--

-- | The strategy monad.  A state monad transformer that maintains the personal
--   state of the player who is playing this strategy, and wraps the game
--   execution monad.  This gives strategies access to the game execution state.
newtype StratM s g a = StratM { unS :: StateT s (ExecM g) a }
  deriving (Applicative, Functor, Monad)

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

-- | Modify a state and return it. Handy in some strategies.
update :: MonadState s m => (s -> s) -> m s
update f = modify f >> get


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
-- * Game execution state
--

-- | The state of a game execution.
data Exec g = Exec {
  _game        :: g,                               -- ^ Game definition.
  _players     :: ByPlayer (Player g),             -- ^ Players active in the game.
  _location    :: (TreeType g) (State g) (Move g), -- ^ The current location in the game tree.
  _transcript  :: Transcript (Move g),             -- ^ Transcript of the current iteration.
  _history     :: History (Move g),                -- ^ History of all completed iterations.
  _numMoves    :: ByPlayer Int,                    -- ^ Total number of moves played by each player.
  _gameNumber  :: Int                              -- ^ The current iteration number.
}

-- | Initial game execution state.
initExec :: Game g => g -> [Player g] -> Exec g
initExec g ps = Exec g (ByPlayer ps) (gameTree g) [] (ByGame []) ms 1
  where ms = ByPlayer (replicate (length ps) 0)


--
-- ** Execution state accessors
--

-- | The game being played.
game :: GameM m g => m g
game = liftM _game getExec


-- *** About players

-- | The players playing.
players :: GameM m g => m (ByPlayer (Player g))
players = liftM _players getExec

-- | The number of players playing the game.
numPlaying :: GameM m g => m Int
numPlaying = liftM dlength players

-- | The index of the currently active player.
myPlayerID :: GameM m g => m PlayerID
myPlayerID = do
  a <- liftM treeAction location
  case a of
    Decision p -> return p
    _ -> error "Internal error: myPlayerID on non-decision node!"

-- | The currently active player.
me :: GameM m g => m (Player g)
me = liftM2 forPlayer myPlayerID players


-- *** About current iteration

-- | Current location in the game graph.
location :: GameM m g => m ((TreeType g) (State g) (Move g))
location = liftM _location getExec

-- | The current game state.
gameState :: GameM m g => m (State g)
gameState = liftM treeState location

-- | Currently available moves.
availMoves :: (DiscreteGame g, GameM m g) => m [Move g]
availMoves = liftM movesFrom location

-- | The current iteration number (i.e. completed iterations +1).
gameNumber :: GameM m g => m Int
gameNumber = liftM _gameNumber getExec

-- | Transcript of moves so far this iteration.
transcript :: GameM m g => m (Transcript (Move g))
transcript = liftM _transcript getExec

-- | Summary of moves so far this iteration, by player. -- TODO rename (movesThisGame?)
move :: GameM m g => m (MoveSummary (Move g))
move = liftM2 summarize numPlaying transcript

-- | Are we at the start of a new game iteration?
isNewGame :: GameM m g => m Bool
isNewGame = liftM null transcript


-- *** About all iterations

-- | The number of completed game iterations.
numCompleted :: GameM m g => m Int
numCompleted = liftM (subtract 1) gameNumber

-- | Historical record of all game iterations.
history :: GameM m g => m (History (Move g))
history = do t  <- transcript
             ms <- liftM (`summarize` t) numPlaying
             h  <- liftM _history getExec
             return (addForNewGame (t,(ms,Nothing)) h)

-- | Transcript of each iteration, including the current one.
transcripts :: GameM m g => m (ByGame (Transcript (Move g)))
transcripts = liftM _transcripts history

-- | Summary of each iteration, including the current one.
summaries :: GameM m g => m (ByGame (Summary (Move g)))
summaries = liftM _summaries history

-- | Payoff of each iteration.  The payoff of the current game is undefined.
payoffs :: GameM m g => m (ByGame Payoff)
payoffs = liftM (fmap _payoff) summaries

-- | Current score.  The sum of previous iterations' payoffs.
score :: GameM m g => m Payoff
score = liftM _score history

-- | The total number of moves each player has played.
numMoves :: GameM m g => m (ByPlayer Int)
numMoves = liftM _numMoves getExec

-- | Summary of the moves of each iteration, including the current one.
moves :: GameM m g => m (ByGame (MoveSummary (Move g)))
moves = liftM (fmap _moveSummary) summaries

-- | The first move of every iteration, including the current one 
--   (which may be undefined for some players).
firstMove :: GameM m g => m (ByGame (ByPlayer (Move g)))
firstMove = liftM ((fmap . fmap) (first . everyTurn)) moves
  where first (a:_) = a
        first _     = error "firstMove: No moves played."

-- | The only move of every iteration, including the current one 
--   (which may be undefined for some players).
onlyMove :: GameM m g => m (ByGame (ByPlayer (Move g)))
onlyMove = liftM ((fmap . fmap) (only . everyTurn)) moves
  where only [a] = a
        only []  = error "onlyMove: No moves played."
        only _   = error "onlyMove: Multiple moves played."


--
-- * Executing games
--

-- | Process one node in the game tree.
step :: (Game g, Eq (Move g)) => ExecM g (Maybe Payoff)
step = location >>= processLocation
  where
    processLocation l = case treeAction l of
      Decision i -> decide i   >>= performMove l
      Chance d   -> fromDist d >>= performMove l
      Payoff p   -> givePayoff p
    
    performMove l m = do
      e <- getExec
      let a = treeAction l
      case treeMove l m of
        Just l' -> put e { _location   = l',
                           _transcript = moveEvent a m : _transcript e,
                           _numMoves   = inc a (_numMoves e) }
        Nothing -> fail "step: illegal move!"
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
      let t = _transcript e
      put e { _location   = gameTree (_game e),
              _transcript = [],
              _history    = addForNewGame (t, (summarize (dlength p) t, Just p)) (_history e),
              _gameNumber = _gameNumber e + 1 }
      return (Just p)


-- | Run the current game iteration to completion, returning the payoff.
finish :: (Game g, Eq (Move g)) => ExecM g Payoff
finish = step >>= maybe finish return

-- | Execute a single game iteration, returning the payoff.  (This is the same
--   as finish.)
once :: (Game g, Eq (Move g)) => ExecM g Payoff
once = finish

-- | Execute n game iterations, returning the cumulative score.
times :: (Game g, Eq (Move g)) => Int -> ExecM g Payoff
times n = numPlaying >>= go n . tie
  where go n p | n <= 0    = return p
               | otherwise = once >>= go (n-1) . addPayoffs p


--
-- Instances
--

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
