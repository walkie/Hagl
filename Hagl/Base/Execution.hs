{-# LANGUAGE FlexibleContexts, PatternGuards #-}
module Hagl.Base.Execution (step,finish,runGame) where

import Control.Monad.State hiding (State)
--import Data.List
--import Data.Function

import Hagl.Base.Accessor
import Hagl.Base.Game
import Hagl.Base.List
import Hagl.Base.Monad

--------------------
-- Game Execution --
--------------------

-- | Process one node in the game tree.
step :: (Game g, Eq (Move g)) => ExecM g (Maybe Payoff)
step = liftM treeNode location >>= processNode

-- | Run the game to completion.
finish :: (Game g, Eq (Move g)) => ExecM g Payoff
finish = step >>= maybe finish return

-- | Execute a game with some given players, returning the payoff.
runGame :: (Game g, Eq (Move g)) => g -> [Player g] -> IO Payoff
runGame g ps = evalGame g ps finish


----------------------
-- Helper Functions --
----------------------

processNode :: (Game g, Eq (Move g)) => Node (State g) (Move g) -> ExecM g (Maybe Payoff)
processNode (Payoff p)      = return (Just p)
processNode (Internal d es) = do m <- decide d
                                 performMove m es
                                 recordMove d m
                                 return Nothing

decide :: Game g => Decision (Move g) -> ExecM g (Move g)
decide (Chance d)   = fromDist d
decide (Decision i) = do p <- liftM (forPlayer i) players
                         (m,p') <- runStrategy p
                         setPlayer i p'
                         return m

performMove :: (Game g, Eq (Move g)) => Move g -> [Edge (State g) (Move g)] -> ExecM g ()
performMove m es | Just t <- lookup m es = setLocation t
                 | otherwise             = fail "Invalid move!"

recordMove :: Game g => Decision (Move g) -> Move g -> ExecM g ()
recordMove d m = do e <- getExec
                    put e { _transcript = moved d m : _transcript e
                          , _numMoves   = numMoves' d (_numMoves e) }
  where numMoves' (Decision p) nm = setListElem (p-1) (forPlayer p nm + 1) nm
        numMoves' _            nm = nm

-- setters

setPlayer :: Game g => PlayerIx -> Player g -> ExecM g ()
setPlayer i p = do e <- getExec
                   put e { _players = setListElem (i-1) p (_players e) }

setLocation :: Game g => GameTree (State g) (Move g) -> ExecM g ()
setLocation l = do e <- getExec
                   put e { _location = l }

{-

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
-}
