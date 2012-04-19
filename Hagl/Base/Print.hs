{-# LANGUAGE FlexibleContexts #-}
module Hagl.Base.Print where

import Control.Applicative ((<*>))
import Control.Monad       (ap,liftM,liftM2)
import Control.Monad.Trans (MonadIO(..))

import Hagl.Base.Game
import Hagl.Base.List
import Hagl.Base.Monad
import Hagl.Base.Accessor
import Hagl.Base.Pretty
import Hagl.Base.Selector
import Hagl.Base.Util

------------------------
-- Printing Functions --
------------------------

print :: (MonadIO m, Show a) => m a -> m ()
print = (>>= printStr . show)

printLn :: (MonadIO m, Show a) => m a -> m ()
printLn = (>>= printStrLn . show)

printStr :: MonadIO m => String -> m ()
printStr = liftIO . putStr

printStrLn :: MonadIO m => String -> m ()
printStrLn = liftIO . putStrLn

-- | Print the transcript of this game, and the payoff if the game is complete.
printTranscript :: (GameM m g, Show (Move g)) => m ()
printTranscript = do
    ps <- players
    t  <- transcript
    fp <- finalPayoff
    printStr (showTranscript ps t)
    printStr (showPayoffLine fp)

{-
-- | Print summary of the last game.
printSummary :: (GameM m g, Show (Move g)) => m ()
printSummary = numGames >>= printSummaryOfGame

-- Print summary of every completed game.
printSummaries :: (GameM m g, Show (Move g)) => m ()
printSummaries = numGames >>= \n -> mapM_ printSummaryOfGame [1..n]

-- Print the summary of the indicated game.
printSummaryOfGame :: (GameM m g, Show (Move g)) => Int -> m ()
printSummaryOfGame n = 
    do (mss,pay) <- forGameM n summaries
       ps <- players
       printStrLn $ "Summary of Game "++show n++":"
       printStr $ unlines ["  "++show p++" moves: "++show (reverse ms) 
                          | (p,ms) <- zip (toList ps) (toList2 mss)]
       printMaybePayoff pay
    
printScore :: (GameM m g, Show (Move g)) => m ()
printScore = do printStrLn "Score:"
                printStr =<< liftM2 scoreString players score
-}

-----------------------
-- Utility functions --
-----------------------

printMaybePayoff :: GameM m g => Maybe Payoff -> m ()
printMaybePayoff Nothing  = return ()
printMaybePayoff (Just p) = printStrLn $ "  Payoff: " ++ show (toList p)
    
-- Generate a string showing a set of players' scores.
scoreString :: ByPlayer (Player g) -> Payoff -> String 
scoreString (ByPlayer ps) (ByPlayer vs) = 
    unlines ["  "++show p++": "++showFloat v | (p,v) <- zip ps vs]
