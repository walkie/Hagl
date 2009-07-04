{-# LANGUAGE FlexibleContexts #-}
module Hagl.Print where

import Control.Monad       (liftM, liftM2)
import Control.Monad.Trans (MonadIO(..))

import Hagl.Core
import Hagl.Accessor

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

-- Print transcript of the last game.
printTranscript :: (GameM m g, Show (Move g)) => m ()
printTranscript = numGames >>= printTranscriptOfGame

-- Print transcripts of completed games.
printTranscripts :: (GameM m g, Show (Move g)) => m ()
printTranscripts = do n <- numGames
                      mapM_ printTranscriptOfGame [1..n]

-- Print Transcript of the given game.
printTranscriptOfGame :: (GameM m g, Show (Move g)) => Int -> m ()
printTranscriptOfGame n = do
    printStrLn $ "Game " ++ show n ++ ":"
    -- print the transcript
    t  <- transcripts `forGameM` n
    ps <- players
    let mv (Just i,  m) = "  " ++ show (ps `forPlayer` i) ++ "'s move: " ++ show m
        mv (Nothing, m) = "  Chance: " ++ show m
     in (printStr . unlines . map mv) (reverse t)
    -- maybe print the payoff
    p <- payoff `forGameM` n
    this <- gameNumber
    if this == n then return ()
                 else printStrLn $ "  Payoff: " ++ showPayoffAsList p

-- Print summary of the last game.
printSummary :: (GameM m g, Show (Move g)) => m ()
printSummary = numGames >>= printSummaryOfGame

-- Print summary of every completed game.
printSummaries :: (GameM m g, Show (Move g)) => m ()
printSummaries = numGames >>= \n -> mapM_ printSummaryOfGame [1..n]

-- Print the summary of the indicated game.
printSummaryOfGame :: (GameM m g, Show (Move g)) => Int -> m ()
printSummaryOfGame n = 
    do (mss,pay) <- summaries `forGameM` n
       ps <- players
       printStrLn $ "Summary of Game "++show n++":"
       printStr $ unlines ["  "++show p++" moves: "++show (reverse ms) 
                          | (p,ms) <- zip (toList ps) (toList2 mss)]
       printMaybePayoff pay
    
printScore :: (GameM m g, Show (Move g)) => m ()
printScore = do printStrLn "Score:"
                printStr =<< liftM2 scoreString players score

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
