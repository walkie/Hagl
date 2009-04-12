module Game.Execution.Print where

import Control.Monad.State
import Data.List
import Game.Definition
import Game.Execution
import Game.Execution.Util
import Game.Strategy
import Game.Strategy.Accessor
import Game.Strategy.Selector

------------------------
-- Printing Functions --
------------------------

print :: (MonadIO m, Show a) => m a -> m ()
print = (>>= liftIO . putStr . show)

printLn :: (MonadIO m, Show a) => m a -> m ()
printLn = (>>= liftIO . putStrLn . show)

printStr :: MonadIO m => String -> m ()
printStr = liftIO . putStr

printStrLn :: MonadIO m => String -> m ()
printStrLn = liftIO . putStrLn

printTranscript :: (MonadIO m, GameMonad m mv, Show mv) => m ()
printTranscript = do n <- numGames
                     sequence_ $ map printTranscriptOfGame [1..n]

printTranscriptOfGame :: (MonadIO m, GameMonad m mv, Show mv) => Int -> m ()
printTranscriptOfGame n =
    do ByGame ts <- transcripts
       ps <- players
       printStrLn $ "Game "++show n++":"
       let t = reverse $ ts !! (length ts - n)
           event (DecisionEvent i m) = "  " ++ show (ps !! (i-1)) ++ "'s move: " ++ show m
           event (ChanceEvent i) = "  Chance: " ++ show i
           event (PayoffEvent vs) = "  Payoff: " ++ show vs
        in printStr $ unlines $ map event t

printSummaries :: (MonadIO m, GameMonad m mv, Show mv) => m ()
printSummaries = do n <- numGames
                    sequence_ $ map printSummaryOfGame [1..n]

printSummaryOfGame :: (MonadIO m, GameMonad m mv, Show mv) => Int -> m ()
printSummaryOfGame n = 
    do ByGame ss <- summaries
       ps <- players
       printStrLn $ "Summary of Game "++show n++":"
       let (ByPlayer mss, ByPlayer vs) = ss !! (length ss - n)
        in do printStr $ unlines ["  "++show p++" moves: "++show (reverse ms) | (p,ms) <- zip ps mss]
              printStrLn $ "  Score: "++show vs

printScore :: (MonadIO m, GameMonad m mv, Show mv) => m ()
printScore = do s <- liftM2 scoreString players (our score)
                printStrLn "Score:"
                printStr =<< liftM2 scoreString players (our score)
