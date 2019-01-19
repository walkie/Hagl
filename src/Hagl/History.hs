-- | This module provides data structures for tracking the execution of
--   games in Hagl, and simple functions for manipulating this data.  Note
--   that underscore-prefixed functions have monadic versions in "Hagl.Exec"
--   without the underscore.  Usually you want the monadic versions from
--   within strategies.
module Hagl.History where

import Data.List  (transpose)
import Data.Maybe (fromMaybe)

import Hagl.Lists
import Hagl.Payoff
import Hagl.Game


--
-- * Transcripts
--

-- | A record of a single move event.
type MoveEvent mv = (Maybe PlayerID, mv)

-- | A transcript is a list of move events.
type Transcript mv  = [MoveEvent mv]

-- | Create a move event from an action and the move made.
moveEvent :: Action mv -> mv -> MoveEvent mv
moveEvent (Decision p) mv = (Just p,  mv)
moveEvent (Chance   _) mv = (Nothing, mv)


--
-- * Move summaries
--

-- | A summary of all the moves made by each player during an iteration.
type MoveSummary mv = ByPlayer (ByTurn mv)

-- | A summary of each iteration: a summary of moves by each player, and
--   a payoff if the game is complete.
type Summary mv = (MoveSummary mv, Maybe Payoff)

-- | Get the move summaries from an iteration summary.
_moveSummary :: Summary mv -> MoveSummary mv
_moveSummary = fst

-- | Get the payoff from the summary of a completed game.
_payoff :: Summary mv -> Payoff
_payoff = fromMaybe e . snd
  where e = error "Incomplete game does not have a payoff!"

-- | Produce a move summary from a transcript.
summarize :: Int -> Transcript mv -> MoveSummary mv
summarize np t = ByPlayer [ByTurn [mv | (mi,mv) <- t, mi == Just p] | p <- [1..np]]


--
-- * Execution history
--

-- | The execution history of an iterated game: a transcript and summary
--   of each completed iteration.
type History mv = ByGame (Transcript mv, Summary mv)

-- | Get the transripts from a history.
_transcripts :: History mv -> ByGame (Transcript mv)
_transcripts = fmap fst

-- | Get the iteration summaries from a history.
_summaries :: History mv -> ByGame (Summary mv)
_summaries = fmap snd

-- | Compute the current score from a history.
_score :: History mv -> Payoff
_score = ByPlayer . map sum . transpose .   -- calculate score
         map everyPlayer . completedGames . -- convert to plain lists
         fmap _payoff . _summaries          -- get payoffs for each completed game


