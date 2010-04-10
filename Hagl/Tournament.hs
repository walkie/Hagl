
module Hagl.Tournament where

import Control.Monad (liftM)
import Data.List     (nub)

import Hagl.Base

--------------------------
-- Tournament Execution --
--------------------------

-- Tournaments results: mapping from player names to scores.
type Results = [(Name,Float)]

-- | Run a game with each successive collection of players. Aggregate the scores
--   of all Players (based on name) and return the results.
runGames :: Game g => g -> [[Player g]] -> ExecM g Payoff -> IO Results
runGames g pss run = do
    pays <- mapM (\ps -> evalGame g ps run) pss
    let paymap  = zip names $ (concat . map toList) pays
    let score n = sum [p | (m,p) <- paymap, n == m]
    return [(n, score n) | n <- nub names]
  where names = (map name . concat) pss

-- | Tournament where all combinations of players play
--   such that player 1 comes from list 1, player 2 from list 2, etc.
partite :: Game g => g -> [[Player g]] -> ExecM g Payoff -> IO Results
partite g = runGames g . cross

-- | Tournament where all orders of all players are played 
--   (including against selves).
fullRoundRobin :: Game g => g -> Int -> [Player g] -> ExecM g Payoff -> IO Results
fullRoundRobin g np ps = partite g (replicate np ps)

-- | Tournament where all unique combinations of players are played 
--   (including against selves). TODO test with np /= 2
roundRobin :: Game g => g -> Int -> [Player g] -> ExecM g Payoff -> IO Results
roundRobin g np ps = runGames g (ucross (replicate np ps))

-- | Print the results of a tournament.
printResults :: Results -> IO ()
printResults r = do
    putStrLn "Tournament Results:"
    putStr $ unlines ["  " ++ n ++ ": " ++ showFloat s | (n,s) <- r]
