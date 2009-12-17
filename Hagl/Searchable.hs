{-# LANGUAGE FlexibleContexts #-}
module Hagl.Searchable where

import Control.Monad (liftM2, liftM3)
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
gameTreeM = liftM2 gameTree game gameState

nextStateM :: (Searchable g, GameM m g) => Move g -> m (State g)
nextStateM = liftM3 nextState game gameState . return

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
    let ifMe p a b = if (me == p) then a else b
        val alpha beta n@(Decision p _)
           | alpha >= beta = ifMe p alpha beta
           | otherwise =
               let mm (a,b) n = let v = val a b n
                                in ifMe p (max a v, b) (a, min b v)
                   (alpha', beta') = foldl mm (alpha, beta) (children n)
               in ifMe p alpha' beta'
        val _ _ (Payoff vs) = forPlayer me vs
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

-----------------------
-- Library Functions -- -- for easily making games Searchable
-----------------------

-- Build a tree for a state-based game.
stateGameTree :: Game g => g                    -- ^ Game definition.
              -> (State g -> PlayerIx)          -- ^ Whose turn is it?
              -> (State g -> Bool)              -- ^ Is the game over?
              -> (State g -> [Move g])          -- ^ Available moves.
              -> (State g -> Move g -> State g) -- ^ Execute a move and return the new state.
              -> (State g -> Payoff)            -- ^ Payoff for this (final) state.
              -> State g                        -- ^ The current state.
              -> GameTree (Move g)
stateGameTree g who end moves exec pay init = tree init
  where tree s | end s     = Payoff (pay s)
               | otherwise = Decision (who s) [(m, tree (exec s m)) | m <- moves s]
