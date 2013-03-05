{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}

-- | This module provides support for representing and executing
--   iterated games.
module Hagl.Iterated where

import Control.Monad (liftM)
import Data.Maybe    (fromMaybe)
import Data.List     (transpose)

import Hagl.Lists
import Hagl.Payoff
import Hagl.Game
import Hagl.Exec
import Hagl.Strategy


--
-- * Representation
--

-- | An iterated game is just a game repeated a number of times specified by
--   `Limit`.
data Iterated g = Iterated Limit g

-- | An iterated game can have either a finite or infinite number of iterations.
data Limit = Finite Int | Infinite deriving Eq

-- | Construct an infinitely iterated game from a non-iterated game.
iterated :: g -> Iterated g
iterated = Iterated Infinite

-- | Get the uniterated form of a game.
uniterated :: Iterated g -> g
uniterated (Iterated _ g) = g

-- | Get the limit of an iterated game.
limit :: Iterated g -> Limit
limit (Iterated l _) = l

-- | `Just n` if the limit is finite, `Nothing` otherwise.
limitToMaybe :: Limit -> Maybe Int
limitToMaybe (Finite i) = Just i
limitToMaybe Infinite   = Nothing

-- | Given a number of iterations, has the limit been reached?
reached :: Int -> Limit -> Bool
reached _ Infinite   = False
reached a (Finite b) = a < b

-- Show instances
--

instance Show Limit where
  show (Finite n) = "Iterated " ++ show n ++ " times"
  show Infinite   = "Iterated"

instance Show g => Show (Iterated g) where
  show (Iterated l g) = "(" ++ show l ++ ")\n" ++ show g


--
-- * Iterated game execution state
--

-- | Summary of each iteration: a summary of moves by each player, and
--   a payoff if the game is complete.
type Summary mv = (MoveSummary mv, Maybe Payoff)

-- | The execution history of an iterated game: a transcript and summary
--   of each completed game.
type History mv = ByGame (Transcript mv, Summary mv)

-- | Iterated game execution state.
data Iter s mv = Iter {
  _gameNumber     :: Int,           -- ^ The current iteration number.
  _history        :: History mv,    -- ^ History of all completed game iterations.
  _iterTranscript :: Transcript mv, -- ^ The transcript of the current iteration.
  _iterState      :: s              -- ^ The state of the current game iteration.
}

-- | Initial iterated game execution state.
initIter :: s -> Iter s mv
initIter = Iter 1 (ByGame []) []

-- | Get the transripts from a history.
_transcripts :: History mv -> ByGame (Transcript mv)
_transcripts = fmap fst

-- | Get the iteration summaries from a history.
_summaries :: History mv -> ByGame (Summary mv)
_summaries = fmap snd

-- | Get the move summaries from an iteration summary.
_moveSummary :: Summary mv -> MoveSummary mv
_moveSummary = fst

-- | Get the payoff from the summary of a completed game.
_payoff :: Summary mv -> Payoff
_payoff = fromMaybe e . snd
  where e = error "Incomplete game does not have a payoff!"

-- | Compute the current score from a history.
_score :: History mv -> Payoff
_score = ByPlayer . map sum . transpose .  -- calculate score
         map everyPlayer . everyGame .     -- convert to plain lists
         fmap _payoff . _summaries         -- get payoffs for each game


-- ** Execution state accessors
--

-- | The current iteration number (i.e. completed iterations +1).
gameNumber :: GameM m (Iterated g) => m Int
gameNumber = liftM _gameNumber gameState

-- | The number of completed game iterations.
numCompleted :: GameM m (Iterated g) => m Int
numCompleted = liftM (subtract 1) gameNumber

-- | The state of the current game iteration.
iterState :: GameM m (Iterated g) => m (State g)
iterState = liftM _iterState gameState

-- | Record of all completed game iterations.
history :: GameM m (Iterated g) => m (History (Move g))
history = liftM _history gameState

-- | Transcript for the current iteration.
iterTranscript :: GameM m (Iterated g) => m (Transcript (Move g))
iterTranscript = liftM _iterTranscript gameState

-- | Transcript of each iteration, including the current one.
transcripts :: GameM m (Iterated g) => m (ByGame (Transcript (Move g)))
transcripts = do t  <- liftM _iterTranscript gameState
                 ts <- liftM _transcripts history
                 return (addForNewGame t ts)

-- | Summary of each iteration, including the current one.
summaries :: GameM m (Iterated g) => m (ByGame (Summary (Move g)))
summaries = do t  <- liftM _iterTranscript gameState
               ms <- liftM (`summarize` t) numPlaying 
               ss <- liftM _summaries history
               return (addForNewGame (ms,Nothing) ss)

-- | Summary of the moves of each iteration, including the current one.
moves :: GameM m (Iterated g) => m (ByGame (MoveSummary (Move g)))
moves = liftM (fmap _moveSummary) summaries

-- | The first move of every iteration, including the current one 
--   (which may be undefined for some players).
firstMove :: GameM m (Iterated g) => m (ByGame (ByPlayer (Move g)))
firstMove = liftM ((fmap . fmap) (first . everyTurn)) moves
  where first (a:_) = a
        first _     = error "firstMove: No moves played."

-- | The only move of every iteration, including the current one 
--   (which may be undefined for some players).
onlyMove :: GameM m (Iterated g) => m (ByGame (ByPlayer (Move g)))
onlyMove = liftM ((fmap . fmap) (only . everyTurn)) moves
  where only [a] = a
        only []  = error "onlyMove: No moves played."
        only _   = error "onlyMove: Multiple moves played."

-- | Payoff of each iteration.  The payoff of the current game is undefined.
payoffs :: GameM m (Iterated g) => m (ByGame Payoff)
payoffs = liftM (fmap _payoff) summaries

-- | Current score.  The sum of previous iterations' payoffs.
score :: GameM m (Iterated g) => m Payoff
score = liftM _score history

-- | Are we at the start of a new game iteration?
isNewGame :: GameM m (Iterated g) => m Bool
isNewGame = liftM null iterTranscript


--
-- * Executing iterated games
--

-- | Run a non-iterated game as a finite iterated game the indicated number of
--   times and return the final score.  Like `Hagl.Exec.runGame` but on the
--   iterated version of 'g'.
runIterated :: (Game g, Eq (Move g)) => g -> Int -> [Player (Iterated g)] -> IO Payoff
runIterated g n = runGame (Iterated (Finite n) g)

-- | Like `Hagl.Exec.evalGame`, but on an infinitely iterated version of 'g'.
evalIterated :: Game g => g -> [Player (Iterated g)] -> ExecM (Iterated g) a -> IO a
evalIterated = evalGame . Iterated Infinite

-- | Like `Hagl.Exec.execGame`, but on an infinitely iterated version of 'g'.
execIterated :: Game g => g -> [Player (Iterated g)] -> ExecM (Iterated g) a -> IO (Exec (Iterated g))
execIterated = execGame . Iterated Infinite

-- | Execute a single game iteration, returning the payoff.
once :: (Game g, Eq (Move g)) => ExecM (Iterated g) Payoff
once = step >> isNewGame >>= \done ->
       if done then lastGame's payoffs else once

-- | Execute n game iterations, returning the cumulative score.
times :: (Game g, Eq (Move g)) => Int -> ExecM (Iterated g) Payoff
times n = numPlaying >>= go n . tie
  where go n p | n <= 0    = return p
               | otherwise = once >>= go (n-1) . addPayoffs p


--
-- Game instances
--

instance Game g => Game (Iterated g) where
  type TreeType (Iterated g) = TreeType g
  type Move  (Iterated g) = Move g
  type State (Iterated g) = Iter (State g) (Move g)
  gameTree = undefined
  
  {-
  start (Iterated _ g) = (initIter s, a)
    where (s,a) = start g

  transition (Iterated l g) (Iter n h t s, a) m =
      case transition g (s,a) m of
        (s',Payoff p) | reached n l -> (Iter n h' [] s', Payoff (_score h'))
                      | otherwise   -> (Iter (n+1) h' [] (startState g), startAction g)
          where h' = addForNewGame (t', (summarize (dlength p) t', Just p)) h
        (s',a') -> (Iter n h t' s', a')
    where t' = moveEvent a m : t
  -}
