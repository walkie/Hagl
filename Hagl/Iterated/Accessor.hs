module Hagl.Iterated.Accessor where

import Control.Monad (liftM)

import Hagl.Base.Types
import Hagl.Iterated.Types

-----------------------------
-- Iterated Game Accessors --
-----------------------------

gameNumber :: (GameM m i, IterGame i g) => m Int
gameNumber = liftM _gameNumber getIter

history :: (GameM m i, IterGame i g) => m (History (Move g))
history = liftM _history getIter

gameTranscript :: (GameM m i, IterGame i g) => m (Transcript (Move g))
gameTranscript = liftM _gameTranscript getIter

gameState :: (GameM m i, IterGame i g) => m (State g)
gameState = liftM _gameState getIter

transcripts :: (GameM m i, IterGame i g) => m (ByGame (Transcript (Move g)))
transcripts = liftM _transcripts history

summaries :: (GameM m i, IterGame i g) => m (ByGame (Summary (Move g)))
summaries = liftM _summaries history

-- would be better/easiest to write this with lastGame... in Selector
summary :: (GameM m i, IterGame i g) => m (Summary (Move g))
summary = liftM (head . toList) summaries

moves :: (GameM m i, IterGame i g) => m (MoveSummary (Move g))
moves = liftM _moves summary

payoff :: (GameM m i, IterGame i g) => m Payoff
payoff = liftM _payoff summary

score :: (GameM m i, IterGame i g) => m Payoff
score = liftM _score history
