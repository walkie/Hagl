{-# LANGUAGE TupleSections #-}
module Hagl.Iterated.Accessor where

import Control.Monad (liftM)

import Hagl.Base
import Hagl.Iterated.List
import Hagl.Iterated.Game
import Hagl.Iterated.Exec

-----------------------------
-- Iterated Game Accessors --
-----------------------------

-- | The current iteration number (i.e. completed iterations +1).
gameNumber :: (GameM m i, IterGame i g) => m Int
gameNumber = liftM _gameNumber getIter

-- | The state of the current game iteration.
gameState :: (GameM m i, IterGame i g) => m (State g)
gameState = liftM _gameState getIter

-- | Record of all completed game iterations.
history :: (GameM m i, IterGame i g) => m (History (Move g))
history = liftM _history getIter

-- | Transcript of each iteration, including the current one.
gameTranscript :: (GameM m i, IterGame i g) => m (ByGame (Transcript (Move g)))
gameTranscript = do t  <- liftM _gameTranscript getIter
                    ts <- liftM _transcripts history
                    return (t `dcons` ts)

-- | Summary of each iteration, including the current one.
summary :: (GameM m i, IterGame i g) => m (ByGame (Summary (Move g)))
summary = do t  <- liftM _gameTranscript getIter
             ms <- liftM (flip summarize t) numPlayers 
             ss <- liftM _summaries history
             return ((ms,Nothing) `dcons` ss)

-- | Summary of the moves of each iteration, including the current one.
moves :: (GameM m i, IterGame i g) => m (ByGame (MoveSummary (Move g)))
moves = liftM (fmap _moveSummary) summary

-- | The first move of every iteration, including the current one 
--   (which may be undefined for some players).
onlyMove :: (GameM m i, IterGame i g) => m (ByGame (ByPlayer (Move g)))
onlyMove = liftM ((fmap . fmap) (head . toList)) moves

-- | Payoff of each iteration.  The payoff of the current game is undefined.
payoff :: (GameM m i, IterGame i g) => m (ByGame Payoff)
payoff = liftM (fmap _payoff) summary

-- | Current score.  The sum of previous iterations' payoffs.
score :: (GameM m i, IterGame i g) => m Payoff
score = liftM _score history
