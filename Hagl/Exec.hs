module Game.Execution.Tournament where

import Control.Monad.State
import Data.List
import Game.Definition
import Game.Execution
import Game.Execution.Run
import Game.Execution.Util
import Game.Strategy
import Game.Strategy.Accessor
import Game.Strategy.Selector

-- Run a game with each successive collection of players. Aggregate the scores
-- of all Players (based on name) and print the final scores.
runGames :: (Show m) => Game m -> [[Player m]] -> GameExec m a -> IO ()
runGames g pss f = 
    let unique = nub $ concat pss
        run ps = evalGame g ps (f >> our score)
    in sequence (map run pss) >>= \vss ->
         let pis = map (flip elemIndices (concat pss)) unique
             vs =  map (sum . map ((concat vss) !!)) pis
             (vs', ps') = unzip $ reverse $ sortTogether vs unique
         in do putStrLn "Final Scores:"
               putStr $ scoreString ps' vs'

-- Run a tournament where all combinations of players are played
-- where player 1 comes from list 1, player 2 from list 2, etc.
tournament :: (Show m) => Game m -> [[Player m]] -> GameExec m a -> IO ()
tournament g pss = runGames g (allCombs pss)

-- Run a tournament where all orders of all players are played 
-- (including against selves).
fullRoundRobin :: (Show m) => Game m -> [Player m] -> GameExec m a -> IO ()
fullRoundRobin g ps = tournament g (replicate (numPlayers g) ps)

-- Run a tournament where all unique combinations of players are played 
-- (including against selves).
roundRobin :: (Show m) => Game m -> [Player m] -> GameExec m a -> IO ()
roundRobin g ps = runGames g (allUnique (replicate (numPlayers g) ps))

-----------------------
-- Utility Functions --
-----------------------

allCombs :: [[a]] -> [[a]]
allCombs (xs:xss) = [(y:ys) | y <- xs, ys <- allCombs xss]
allCombs [] = [[]]

allUnique :: (Ord a) => [[a]] -> [[a]]
allUnique = nub . map sort . allCombs

sortTogether :: (Ord a) => [a] -> [b] -> [(a, b)]
sortTogether as bs = let f (a1,_) (a2,_) = compare a1 a2
                     in sortBy f $ zip as bs
