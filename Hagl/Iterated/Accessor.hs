{-# LANGUAGE FlexibleContexts #-}

module Hagl.Iterated.Accessor where

import Control.Monad (liftM)

import Hagl.Base hiding (payoff,transcript)
import Hagl.Iterated.List
import Hagl.Iterated.Game

-----------------------------
-- Iterated Game Accessors --
-----------------------------

-- | The current iteration number (i.e. completed iterations +1).
gameNumber :: GameM m (Iterated g) => m Int
gameNumber = liftM _gameNumber state

-- | The state of the current game iteration.
gameState :: GameM m (Iterated g) => m (State g)
gameState = liftM _gameState state

-- | Record of all completed game iterations.
history :: GameM m (Iterated g) => m (History (Move g))
history = liftM _history state

-- | Transcript of each iteration, including the current one.
transcript :: GameM m (Iterated g) => m (ByGame (Transcript (Move g)))
transcript = do t  <- liftM _gameTranscript state
                ts <- liftM _transcripts history
                return (t `dcons` ts)

-- | Summary of each iteration, including the current one.
summary :: GameM m (Iterated g) => m (ByGame (Summary (Move g)))
summary = do t  <- liftM _gameTranscript state
             ms <- liftM (flip summarize t) numPlayers 
             ss <- liftM _summaries history
             return ((ms,Nothing) `dcons` ss)

-- | Summary of the moves of each iteration, including the current one.
moves :: GameM m (Iterated g) => m (ByGame (MoveSummary (Move g)))
moves = liftM (fmap _moveSummary) summary

-- | The first move of every iteration, including the current one 
--   (which may be undefined for some players).
onlyMove :: GameM m (Iterated g) => m (ByGame (ByPlayer (Move g)))
onlyMove = liftM ((fmap . fmap) (head . toList)) moves

-- | Payoff of each iteration.  The payoff of the current game is undefined.
payoff :: GameM m (Iterated g) => m (ByGame Payoff)
payoff = liftM (fmap _payoff) summary

-- | Current score.  The sum of previous iterations' payoffs.
score :: GameM m (Iterated g) => m Payoff
score = liftM _score history
