module Game.Strategy where

import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Game.Definition
import Game.Execution
import Game.Execution.Util
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
periodic ms = numMoves >>= \n -> return $ ms !! mod n (length ms)

-- Begin a list of strategies.
atFirst :: Strategy mv s -> [Strategy mv s] -> Strategy mv s
atFirst s ss = numMoves >>= \n -> (s:ss) !!! n

-- Next in a list of strategies.
next :: Strategy mv s -> [Strategy mv s] -> [Strategy mv s]
next = (:)

-- End a list of strategies.
finally :: Strategy mv s -> [Strategy mv s]
finally = (:[])

-- Play a strategy for the first move, then another strategy thereafter.
atFirstThen :: Strategy mv s -> Strategy mv s -> Strategy mv s
atFirstThen a b = atFirst a (finally b)

-- Play an initial move, then another strategy thereafter.
initiallyThen :: mv -> Strategy mv s -> Strategy mv s
initiallyThen a b = atFirst (return a) (finally b)

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

numMoves :: GameMonad m mv => m Int
numMoves = liftM (length . concat) (my `each` every moves)
