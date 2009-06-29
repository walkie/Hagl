module Hagl.Strategy where

import Hagl.Core
import Hagl.Accessor (numMoves)
import Hagl.Selector (my)

-----------------------
-- Common Strategies --
-----------------------

-- Play a move.
play :: Move g -> Strategy s g
play = return

-- Construct a pure strategy. Always play the same move.
pure :: Move g -> Strategy s g
pure = return

-- Construct a mixed strategy. Play moves based on a distribution.
mixed :: [(Int, Move g)] -> Strategy s g
mixed = randomlyFrom . expandDist

-- Perform some pattern of moves periodically.
periodic :: Game g => [Move g] -> Strategy s g
periodic ms = my numMoves >>= \n -> return $ ms !! mod n (length ms)

-- Play a list of initial strategies, then a primary strategy thereafter.
thereafter :: Game g => [Strategy s g] -> Strategy s g -> Strategy s g
thereafter ss s = my numMoves >>= \n -> if n < length ss then ss !! n else s

-- Play an initial strategy for the first move, then a primary strategy thereafter.
atFirstThen :: Game g => Strategy s g -> Strategy s g -> Strategy s g
atFirstThen s = thereafter [s]
