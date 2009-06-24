{-# LANGUAGE FlexibleContexts #-}
module Hagl.Exec where

import Control.Monad.State
import Data.List
import Data.Function

import Hagl.Core
import Hagl.Accessor
import Hagl.Print

--------------------
-- Game Execution --
--------------------

once :: Game g => ExecM g ()
once = runGame >>= conclude

times :: Game g => Int -> ExecM g ()
times 0 = return ()
times n = once >> times (n-1)

conclude :: Game g => Payoff -> ExecM g ()
conclude p = do
    e <- getExec
    g <- game
    t <- transcript
    h <- history
    ms <- moveSummary
    put e { _gameState = initState g,
            _transcript = [],
            _history = (t, (ms, Just p)) `dcons` h }

--------------------------
-- Tournament Execution --
--------------------------

-- Run a game with each successive collection of players. Aggregate the scores
-- of all Players (based on name) and print the final scores.
-- TODO: Cleanup this function...
runGames :: (Game g, Show (Move g)) => g -> [[Player g]] -> ExecM g a -> IO ()
runGames g pss f = 
    let unique = nub $ concat pss
        run ps = evalGame g ps (f >> liftM toList score)
    in mapM run pss >>= \vss ->
         let pis = map (flip elemIndices (concat pss)) unique
             vs =  map (sum . map ((concat vss) !!)) pis
             (vs', ps') = unzip $ reverse $ sortTogether vs unique
         in do putStrLn "Final Scores:"
               putStr $ scoreString (fromList ps') (fromList vs')

-- Run a tournament where all combinations of players are played
-- where player 1 comes from list 1, player 2 from list 2, etc.
tournament :: (Game g, Show (Move g)) => g -> [[Player g]] -> ExecM g a -> IO ()
tournament g = runGames g . cross

-- Run a tournament where all orders of all players are played 
-- (including against selves).
-- TODO constant 2 should be replaced with some way to get number of players...
fullRoundRobin :: (Game g, Show (Move g)) => g -> [Player g] -> ExecM g a -> IO ()
fullRoundRobin g ps = tournament g (replicate 2 ps)

-- Run a tournament where all unique combinations of players are played 
-- (including against selves).
-- TODO constant 2 should be replaced with some way to get number of players...
roundRobin :: (Game g, Show (Move g)) => g -> [Player g] -> ExecM g a -> IO ()
roundRobin g ps = runGames g (ucross (replicate 2 ps))

-----------------------
-- Utility Functions --
-----------------------

sortTogether :: (Ord a) => [a] -> [b] -> [(a, b)]
sortTogether as bs = sortBy (compare `on` fst) (zip as bs)
