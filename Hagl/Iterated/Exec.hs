{-# LANGUAGE FunctionalDependencies,
             MultiParamTypeClasses #-}

module Hagl.Iterated.Exec where

import Data.Maybe (fromMaybe)
import Data.List  (transpose)

import Hagl.Base
import Hagl.Iterated.List


---------------------
-- Execution State --
---------------------

type Summary mv = (MoveSummary mv, Maybe Payoff)
type History mv = ByGame (Transcript mv, Summary mv)

data Iter s mv = Iter {
  _iterNumber     :: Int,           -- the current iteration number
  _history        :: History mv,    -- history of all completed game iterations
  _iterTranscript :: Transcript mv, -- the transcript of the current iteration
  _iterState      :: s              -- the state of the current game iteration
}

initIter :: s -> Iter s mv
initIter = Iter 1 (ByGame []) []

_transcripts :: History mv -> ByGame (Transcript mv)
_transcripts = fmap fst

_summaries :: History mv -> ByGame (Summary mv)
_summaries = fmap snd

_moveSummary :: Summary mv -> MoveSummary mv
_moveSummary = fst

_payoff :: Summary mv -> Payoff
_payoff = fromMaybe e . snd
  where e = error "Incomplete game does not have a payoff!"

_score :: History mv -> Payoff
_score = ByPlayer . map sum . transpose . toList2 . fmap _payoff . _summaries


-- Type class to enable access to iteration state from within other GameM monads.
class IterGame i g | i -> g where
  getIter :: GameM m i => m (Iter (State g) (Move g))
