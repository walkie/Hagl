{-# LANGUAGE
      ExistentialQuantification,
      FlexibleInstances,
      FunctionalDependencies
  #-}

-- | This module defines a generic interface for the execution of games and
--   strategies in Hagl.
--
--   This module defines two monads:
--   in @GameM@ and @StrategyMonad@. @ExecM@ maintains the current
--   execution state of the game, while @StratM@ adds an additional state
--   that is local to each strategy. Each strategy may define its own
--   type of state and strategies cannot affect the state of other strategies.
--
--   At the center of the monad onion is the @IO@ monad, allowing both game
--   execution and strategies to do things like print output, get random
--   numbers, and look up the price of tea in Shanghai.
module Hagl.Game where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader,ReaderT,ask,runReaderT)
import Control.Monad.State (MonadState,StateT,get,put,runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad (liftM,liftM2)
import Data.Function (on)

import Hagl.History
import Hagl.List
import Hagl.Payoff


--
-- * Games
--

-- | All games can be reduced to a game tree. This type class's associated
--   types capture the relevant type parameters of that tree.
class Game g where

  -- | The type of state maintained throughout the game
  --   (use @()@ for stateless games).
  type State g

  -- | The type of moves that may be played during the game.
  type Move g

  -- | The initial state of the game.
  initState :: g -> State g

  -- | Run this game.
  runGame :: g -> GameM g Payoff


--
-- * Players
--

-- | A player name.
type Name = String

-- | Each player has a name and strategy, and may optionally maintain
--   their own personal state.
data Player g
   = forall s. Player Name s (Strategy s g)  -- ^ A stateful player.
   | Name ::: Strategy () g                  -- ^ A stateless player.

infix 0 :::

instance Show (Player g) where
  show = name
instance Eq (Player g) where
  (==) = (==) `on` name
instance Ord (Player g) where
  compare = compare `on` name

-- | The name of a player.
name :: Player g -> Name
name (Player n _ _) = n
name (n ::: _)      = n


--
-- * Game monads
--

-- | A type class for the game execution monad in the MTL style. This includes
--   both the concrete game execution monad @GameM@ and the strategy execution
--   monad @StratM@.
class Monad m => MonadGame g m | m -> g where

  -- | Get the game that is currently being played.
  game :: m g

  -- | Get the current game execution state.
  getExec :: m (Exec g)

  -- | Set the current game execution state.
  putExec :: Exec g -> m ()


-- ** Game execution monad

-- | The game execution monad.
newtype GameM g a = GameM
  { unGameM :: ReaderT g (StateT (Exec g) IO) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader g
    , MonadState (Exec g)
    )

instance MonadGame g (GameM g) where
  game = ask
  getExec = get
  putExec = put

-- | Execute a game execution action with the given game and players,
--   returning the result and the final execution state.
runGameM :: Game g => g -> [Player g] -> GameM g a -> IO (a, Exec g)
runGameM g ps (GameM m) = runStateT (runReaderT m g) (initExec g ps)

-- | Execute a game execution action with the given game and players,
--   returning the resulting value.
evalGameM :: Game g => g -> [Player g] -> GameM g a -> IO a
evalGameM g ps = fmap fst . runGameM g ps

-- | Execute a game execution action with the given game and players,
--   returning the final execution state.
execGameM :: Game g => g -> [Player g] -> GameM g a -> IO (Exec g)
execGameM g ps = fmap snd . runGameM g ps


-- ** Strategy monad

-- | The strategy monad. Wraps the game execution monad with an additional
--   state, specific to the player who is playing this strategy.
newtype StratM s g a = StratM
  { unStratM :: StateT s (GameM g) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader g
    , MonadState s
    )

instance MonadGame g (StratM s g) where
  game = ask
  getExec = StratM (lift getExec)
  putExec = StratM . lift . putExec

-- | A strategy is an action in the strategy monad that yields a move.
type Strategy s g = StratM s g (Move g)

-- | Run the strategy associated with a particular player, producing a move
--   and an updated player state.
runStrategy :: Player g -> GameM g (Move g, Player g)
runStrategy p@(_ ::: m) = do
    (mv, _) <- runStateT (unStratM m) ()
    return (mv, p)
runStrategy (Player n s m) = do
    (mv, s') <- runStateT (unStratM m) s
    return (mv, Player n s' m)


--
-- * Game execution state
--

-- | The state of a game execution.
data Exec g = Exec
  { _players    :: ByPlayer (Player g)  -- ^ Players active in the game.
  , _gameState  :: State g              -- ^ The current state of the game.
  , _playerID   :: Maybe PlayerID       -- ^ Index of the currently active player.
  , _transcript :: Transcript (Move g)  -- ^ Transcript of the current iteration.
  , _history    :: History (Move g)     -- ^ History of all completed iterations.
  , _numMoves   :: ByPlayer Int         -- ^ Total number of moves played by each player.
  , _gameNumber :: Int                  -- ^ The current iteration number.
  }

-- | Initial game execution state.
initExec :: Game g => g -> [Player g] -> Exec g
initExec g ps = Exec (ByPlayer ps) (initState g) Nothing [] (ByGame []) ms 1
  where
    ms = ByPlayer (replicate (length ps) 0)


-- ** Execution state accessors

-- | The players playing.
allPlayers :: MonadGame g m => m (ByPlayer (Player g))
allPlayers = liftM _players getExec

-- | The number of players playing the game.
numPlaying :: MonadGame g m => m Int
numPlaying = liftM dlength allPlayers

-- | The index of the currently active player.
playerID :: MonadGame g m => m PlayerID
playerID = do
    mi <- liftM _playerID getExec
    case mi of
      Just i -> return i
      _ -> error "playerID: no active player"

-- | The currently active player.
me :: MonadGame g m => m (Player g)
me = liftM2 forPlayer playerID allPlayers

-- | The current game state.
gameState :: MonadGame g m => m (State g)
gameState = liftM _gameState getExec

-- | Currently available moves.
-- availMoves :: (FiniteGame g, GameM m g) => m [Move g]
-- availMoves = liftM treeMoves location

-- | Transcript of moves so far this iteration.
transcript :: MonadGame g m => m (Transcript (Move g))
transcript = liftM _transcript getExec

-- | Are we at the start of a new game iteration?
isNewGame :: MonadGame g m => m Bool
isNewGame = liftM null transcript

-- | Historical record of all game iterations.
history :: MonadGame g m => m (History (Move g))
history = do
    t  <- transcript
    ms <- liftM (`summarize` t) numPlaying
    h  <- liftM _history getExec
    return (addForNewGame (t,(ms,Nothing)) h)

-- | Transcript of each iteration, including the current one.
transcripts :: MonadGame g m => m (ByGame (Transcript (Move g)))
transcripts = liftM _transcripts history

-- | Summary of each iteration, including the current one.
summaries :: MonadGame g m => m (ByGame (Summary (Move g)))
summaries = liftM _summaries history

-- | Payoff of each iteration.  The payoff of the current game is undefined.
payoffs :: MonadGame g m => m (ByGame Payoff)
payoffs = liftM (fmap _payoff) summaries

-- | Current score.  The sum of previous iterations' payoffs.
score :: MonadGame g m => m Payoff
score = liftM _score history

-- | Summary of the moves of each iteration, including the current one.
moves :: MonadGame g m => m (ByGame (MoveSummary (Move g)))
moves = liftM (fmap _moveSummary) summaries

-- | Summary of moves so far this iteration, by player.
movesThisGame :: MonadGame g m => m (MoveSummary (Move g))
movesThisGame = liftM2 summarize numPlaying transcript

-- | The first move of every iteration, including the current one
--   (which may be undefined for some players).
firstMove :: MonadGame g m => m (ByGame (ByPlayer (Move g)))
firstMove = liftM ((fmap . fmap) (first . everyTurn)) moves
  where
    first (a:_) = a
    first _     = error "firstMove: No moves played."

-- | The only move of every iteration, including the current one
--   (which may be undefined for some players).
onlyMove :: MonadGame g m => m (ByGame (ByPlayer (Move g)))
onlyMove = liftM ((fmap . fmap) (only . everyTurn)) moves
  where
    only [a] = a
    only []  = error "onlyMove: No moves played."
    only _   = error "onlyMove: Multiple moves played."

-- | The total number of moves each player has played.
numMoves :: MonadGame g m => m (ByPlayer Int)
numMoves = liftM _numMoves getExec

-- | The current iteration number (i.e. completed iterations +1).
gameNumber :: MonadGame g m => m Int
gameNumber = liftM _gameNumber getExec

-- | The number of completed game iterations.
numCompleted :: MonadGame g m => m Int
numCompleted = liftM (subtract 1) gameNumber


--
-- * Executing games
--

-- | Have the indicated player make a decision, log and return the move that
--   the player chooses.
decide :: PlayerID -> GameM g (Move g)
decide i = do
    e <- getExec
    put e { _playerID = Just i }
    let ps = _players e
    (mv,p') <- runStrategy (forPlayer i ps)
    put e { _players = setForPlayer i p' ps
          , _playerID = Nothing
          , _transcript = (Just i, mv) : _transcript e
          , _numMoves = modifyForPlayer i (+1) (_numMoves e) }
    return mv

-- | Pick a move randomly from the given distribution.
chance :: Dist (Move g) -> GameM g (Move g)
chance d = do
    e <- getExec
    mv <- fromDist d
    put e { _transcript = (Nothing, mv) : _transcript e }
    return mv

-- | Execute a single game iteration and return the payoff. Updates the
--   history and resets the game state for the next iteration.
once :: Game g => GameM g Payoff
once = do
    g <- game
    p <- runGame g
    e <- getExec
    let t = _transcript e
    put e { _gameState  = initState g
          , _transcript = []
          , _history    = addForNewGame (t, (summarize (dlength p) t, Just p)) (_history e)
          , _gameNumber = _gameNumber e + 1 }
    return p

-- | Execute n game iterations, returning the cumulative score.
-- times :: (Game g, Eq (Move g)) => Int -> ExecM g Payoff
times :: Game g => Int -> GameM g Payoff
times n = numPlaying >>= go n . tie
  where
    go n p | n <= 0    = return p
           | otherwise = once >>= go (n-1) . addPayoffs p
