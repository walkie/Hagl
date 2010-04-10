{-# LANGUAGE FlexibleContexts #-}

module Hagl.Iterated.Execution where

import Hagl.Base
import Hagl.Iterated.Game
import Hagl.Iterated.Accessor

-----------------------------
-- Iterated Game Execution --
-----------------------------

-- | Execute a single game iteration, returning the payoff.
once :: (Game g, Eq (Move g)) => ExecM (Iterated g) Payoff
once = step >> gamePayoff >>= maybe once return

-- | Execute n game iterations, returning the cummulative score.
times :: (Game g, Eq (Move g)) => Int -> ExecM (Iterated g) Payoff
times n = numPlayers >>= go n . tie
  where go n p | n <= 0    = return p
               | otherwise = once >>= go (n-1) . addPayoffs p
