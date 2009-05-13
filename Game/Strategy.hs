module Game.Strategy where

import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Game.Definition
import Game.Execution
import Game.Execution.Util
import Game.Lists
import Game.Strategy.Accessor
import Game.Strategy.Selector
import Game.Util

-----------------------
-- Common Strategies --
-----------------------

-- Play a move.
play :: mv -> Strategy mv s
play = return

-- Construct a pure strategy. Always play the same move.
pure :: mv -> Strategy mv s
pure = return

-- Pick a move from the list of available moves randomly.
randomly :: Eq mv => Strategy mv s
randomly = do loc <- location
              let ms = case loc of
                         Perfect t -> availMoves t 
                         Imperfect ts -> foldl1 intersect (map availMoves ts)
               in randomlyFrom ms

-- Pick a move randomly from a list.
randomlyFrom :: [mv] -> Strategy mv s
randomlyFrom as = liftM (as !!) (randomIndex as)

-- Construct a mixed strategy. Play moves based on a distribution.
mixed :: [(Int, mv)] -> Strategy mv s
mixed = randomlyFrom . expandDist

-- Perform some pattern of moves periodically.
periodic :: [mv] -> Strategy mv s
periodic ms = my numMoves >>= \n -> return $ ms !! mod n (length ms)

-- Play a list of initial strategies, then a primary strategy thereafter.
thereafter :: [Strategy mv s] -> Strategy mv s -> Strategy mv s
thereafter ss s = my numMoves >>= \n -> if n < length ss then ss !! n else s

atFirstThen :: Strategy mv s -> Strategy mv s -> Strategy mv s
atFirstThen s = thereafter [s]

-- Minimax algorithm with alpha-beta pruning. Only defined for games with
-- perfect information and no Chance nodes.
minimax :: Strategy mv s
minimax = myIx >>= \me -> location >>= \loc ->
  let isMe = (me + 1 ==)
      val alpha beta n@(Decision p _)
         | alpha >= beta = if isMe p then alpha else beta
         | otherwise =
             let mm (a,b) n = let v = val a b n
                              in if isMe p then (max a v, b)
                                           else (a, min b v)
                 (alpha', beta') = foldl mm (alpha, beta) (children n)
             in if isMe p then alpha' else beta'
      val _ _ (Payoff vs) = vs !! me
  in case loc of
       Perfect n -> 
         let vals = map (val (-infinity) infinity) (children n)
         in return $ availMoves n !! maxIndex vals

infinity :: Float
infinity = 1/0


---------------
-- Utilities --
---------------

maxIndex :: (Ord a) => [a] -> Int
maxIndex as = fromJust $ elemIndex (maximum as) as
