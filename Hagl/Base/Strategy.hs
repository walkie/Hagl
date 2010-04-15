{-# LANGUAGE FlexibleContexts #-}

module Hagl.Base.Strategy where

import Control.Monad.Trans (liftIO)
import System.IO.Error     (isUserError)

import Hagl.Base.Game
import Hagl.Base.List
import Hagl.Base.Monad
import Hagl.Base.Accessor
import Hagl.Base.Selector
import Hagl.Base.Util

-----------------------
-- Common Strategies --
-----------------------

-- | Play a move.
play :: Move g -> Strategy s g
play = return

-- | A pure strategy. Always plays the same move.
pure :: Move g -> Strategy s g
pure = return

-- | A mixed strategy. Plays moves based on a distribution.
mixed :: Dist (Move g) -> Strategy s g
mixed = fromDist

-- | Perform some pattern of moves periodically.
periodic :: Game g => [Move g] -> Strategy s g
periodic ms = my numMoves >>= \n -> return $ ms !! mod n (length ms)

-- | Select a move randomly.
randomly :: Game g => Strategy s g
randomly = availMoves >>= randomlyFrom

-- | Play a list of initial strategies, then a primary strategy thereafter.
thereafter :: Game g => [Strategy s g] -> Strategy s g -> Strategy s g
thereafter ss s = my numMoves >>= \n -> if n < length ss then ss !! n else s

-- | Play an initial strategy for the first move, then a primary strategy thereafter.
atFirstThen :: Game g => Strategy s g -> Strategy s g -> Strategy s g
atFirstThen s = thereafter [s]

-- | A human player, who enters moves on the console.
human :: (Game g, Read (Move g)) => Strategy () g
human = me >>= liftIO . getMove . name
  where getMove n = putStr (n ++ "'s move: ") >> catch readLn (retry n)
        retry n e | isUserError e = putStrLn "Not a valid move... try again." >> getMove n
                  | otherwise     = ioError e
