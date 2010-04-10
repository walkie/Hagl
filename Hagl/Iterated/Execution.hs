{-# LANGUAGE FlexibleContexts #-}

module Hagl.Iterated.Execution where

import Hagl.Base
import Hagl.Iterated.Game
import Hagl.Iterated.Accessor

-----------------------------
-- Iterated Game Execution --
-----------------------------

-- | Execute a single game iteration.
once :: (Game g, Eq (Move g)) => ExecM (Iterated g) Payoff
once = step >> gamePayoff >>= maybe once return

-- | Execute n game iterations.
times :: (Game g, Eq (Move g)) => Int -> ExecM (Iterated g) ()
times 0 = return ()
times n = once >> times (n-1)
