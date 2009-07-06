{-# LANGUAGE FlexibleContexts #-}
module Hagl.Searchable where

import Data.List  (elemIndex)
import Data.Maybe (fromJust)

import Hagl.Core
import Hagl.Accessor
import Hagl.Game
import Hagl.GameTree
import Hagl.Exec (once)

----------------------
-- Searchable Class --
----------------------

class Game g => Searchable g where
  gameTree  :: g -> State g -> GameTree (Move g)
  nextState :: g -> State g -> Move g -> State g

location :: (Searchable g, GameM m g) => m (GameTree (Move g))
location = gameTreeM

gameTreeM :: (Searchable g, GameM m g) => m (GameTree (Move g))
gameTreeM = do g <- game
               s <- gameState
               return (gameTree g s)

nextStateM :: (Searchable g, GameM m g) => Move g -> m (State g)
nextStateM m = do g <- game
                  s <- gameState
                  return (nextState g s m)

----------------
-- Strategies --
----------------

-- Pick a move from the list of available moves randomly.
randomly :: (Searchable g, Eq (Move g)) => Strategy s g
randomly = do t <- gameTreeM
              randomlyFrom (movesFrom t)

-- Minimax algorithm with alpha-beta pruning. Only defined for games with
-- perfect information and no Chance nodes.
minimax :: Searchable g => Strategy s g
minimax = myIx >>= \me -> gameTreeM >>= \t -> 
    let isMe = (me + 1 ==)
        val alpha beta n@(Decision p _)
           | alpha >= beta = if isMe p then alpha else beta
           | otherwise =
               let mm (a,b) n = let v = val a b n
                                in if isMe p then (max a v, b)
                                             else (a, min b v)
                   (alpha', beta') = foldl mm (alpha, beta) (children n)
               in if isMe p then alpha' else beta'
        val _ _ (Payoff vs) = vs `forPlayer` me
    in let vals = map (val (-infinity) infinity) (children t)
       in return $ movesFrom t !! maxIndex vals
  where infinity = 1/0 :: Float
        maxIndex as = fromJust $ elemIndex (maximum as) as

--------------------------
-- Executing Game Trees --
--------------------------

runTree :: (Searchable g, Eq (Move g), Show (Move g)) => ExecM g Payoff
runTree = step >>= maybe runTree return

step :: (Searchable g, Eq (Move g), Show (Move g)) => ExecM g (Maybe Payoff)
step = gameTreeM >>= \t -> case t of
  Decision i es ->
    do m <- decide i 
       s <- nextStateM m
       putGameState s
       return Nothing
  Chance d ->
    do m <- chance (moveDist d)
       s <- nextStateM m 
       putGameState s
       return Nothing
  Payoff p -> return (Just p)

finish :: (Searchable g, Eq (Move g), Show (Move g)) => ExecM g ()
finish = once
