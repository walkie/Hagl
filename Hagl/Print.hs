{-# LANGUAGE FlexibleContexts #-}

-- | This module collects several function for converting Hagl values to
--   strings and printing out the current state of an execution.  Note that
--   some other pretty printing functions are located in the modules they are
--   specific to.
module Hagl.Print where

import Control.Monad (liftM,liftM2)
import Control.Monad.IO.Class

import Hagl.Lists
import Hagl.Payoff
import Hagl.Game
import Hagl.Exec
import Hagl.Iterated

--
-- * Generic printing functions
--

-- | Print a value from within a `MonadIO` monad.
print :: (MonadIO m, Show a) => m a -> m ()
print = (>>= printStr . show)

-- | Print a value + newline from within a `MonadIO` monad.
printLn :: (MonadIO m, Show a) => m a -> m ()
printLn = (>>= printStrLn . show)

-- | Print a string from within a `MonadIO` monad.
printStr :: MonadIO m => String -> m ()
printStr = liftIO . putStr

-- | Print a string + newline from within a `MonadIO` monad.
printStrLn :: MonadIO m => String -> m ()
printStrLn = liftIO . putStrLn


--
-- * Pure showing functions
--

-- | String representation of a transcript.
showTranscript :: (Game g, Show (Move g)) =>
  ByPlayer (Player g) -> Transcript (Move g) -> String
showTranscript ps t = (unlines . map mv . reverse) t
  where mv (Just i,  m) = "  " ++ show (forPlayer i ps) ++ "'s move: " ++ show m
        mv (Nothing, m) = "  Chance: " ++ show m

-- | String representation of a move summary.
showMoveSummary :: (Game g, Show (Move g)) =>
  ByPlayer (Player g) -> MoveSummary (Move g) -> String
showMoveSummary ps mss = (unlines . map row)
                         (zip (everyPlayer ps) (map everyTurn (everyPlayer mss)))
  where row (p,ms) = "  " ++ show p ++ " moves: " ++ showSeq (reverse (map show ms))

-- | Generate a string showing a set of players' scores.
scoreString :: ByPlayer (Player g) -> Payoff -> String 
scoreString (ByPlayer ps) (ByPlayer vs) = 
    unlines ["  "++show p++": "++showFloat v | (p,v) <- zip ps vs]


--
-- * Printing game execution state
--

-- | Print a payoff or nothing.
printMaybePayoff :: GameM m g => Maybe Payoff -> m ()
printMaybePayoff Nothing  = return ()
printMaybePayoff (Just p) = printStrLn $ "  Payoff: " ++ show (everyPlayer p)
    
-- | Print the transcript of this game, and the payoff if the game is complete.
printTranscript :: (GameM m g, Show (Move g)) => m ()
printTranscript = do
    ps <- players
    t  <- transcript
    fp <- finalPayoff
    printStr (showTranscript ps t)
    printMaybePayoff fp

-- | Print the moves from the current location.
printMovesFromHere :: (GameM m g, Show (Move g), DiscreteGame g) => m ()
printMovesFromHere = do
    g <- game
    l <- location
    printStrLn (show (movesFrom g l))


--
-- * Printing iterated game execution state
--

-- | Print transcript of the given game.
printTranscriptOfGame :: (GameM m (Iterated g), Show (Move (Iterated g))) => Int -> m ()
printTranscriptOfGame n = do
    printStrLn $ "Game " ++ show n ++ ":"
    -- print the transcript
    t  <- liftM (forGame n) transcripts
    ps <- players
    printStr (showTranscript ps t)
    -- maybe print the payoff
    p  <- liftM (forGame n) payoffs
    this <- gameNumber
    if this == n then return ()
                 else printStrLn $ "  Payoff: " ++ showPayoffAsList p

-- | Print transcripts of all completed games.
printTranscripts :: (GameM m (Iterated g), Show (Move (Iterated g))) => m ()
printTranscripts = do n <- numCompleted
                      mapM_ printTranscriptOfGame [1..n]

-- | Print summary of the last game.
printSummary :: (GameM m (Iterated g), Show (Move (Iterated g))) => m ()
printSummary = numCompleted >>= printSummaryOfGame

-- | Print summary of every completed game.
printSummaries :: (GameM m (Iterated g), Show (Move (Iterated g))) => m ()
printSummaries = numCompleted >>= \n -> mapM_ printSummaryOfGame [1..n]

-- | Print the summary of the indicated game.
printSummaryOfGame :: (GameM m (Iterated g), Show (Move (Iterated g))) => Int -> m ()
printSummaryOfGame n = 
    do (mss,pay) <- liftM (forGame n) summaries
       ps <- players
       printStrLn $ "Summary of Game "++show n++":"
       printStr $ showMoveSummary ps mss
       printMaybePayoff pay
    
-- | Print the current score.
printScore :: (GameM m (Iterated g), Show (Move (Iterated g))) => m ()
printScore = do printStrLn "Score:"
                printStr =<< liftM2 scoreString players score

