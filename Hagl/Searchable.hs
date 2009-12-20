{-# LANGUAGE FlexibleContexts #-}
module Hagl.Searchable where

import Control.Monad (liftM2, liftM3)
import Data.List     (elemIndex)
import Data.Maybe    (fromJust)

import Hagl.Core
import Hagl.Accessor
import Hagl.Game
import Hagl.GameTree
import Hagl.Exec (conclude)

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
randomly = gameTreeM >>= randomlyFrom . movesFrom

-- Minimax strategy.
minimax :: Searchable g => Strategy s g
minimax = liftM2 minimax' myIx gameTreeM

-- Minimax algorithm with alpha-beta pruning. Only defined for games with
-- perfect information and no Chance nodes.
minimax' :: PlayerIx -> GameTree mv -> mv
minimax' me t = movesFrom t !! fromJust (elemIndex (maximum vals) vals)
  where inf  = 1/0 :: Float
        vals = map (val me (-inf) inf) (children t)

-- (used by minimax')
val :: PlayerIx -> Float -> Float -> GameTree mv -> Float
val me _ _   (Payoff vs)                = forPlayer me vs
val me a b n@(Decision p _) | a >= b    = ifMe a  b
                            | otherwise = ifMe a' b'
  where ifMe a b   = if (me == p) then a else b
        mm (a,b) n = let v = val me a b n
                     in ifMe (max a v, b) (a, min b v)
        (a',b')    = foldl mm (a,b) (children n)

--------------------------
-- Executing Game Trees --
--------------------------

runTree :: (Searchable g, Eq (Move g), Show (Move g)) => ExecM g Payoff
runTree = step >>= maybe runTree return

step :: (Searchable g, Eq (Move g), Show (Move g)) => ExecM g (Maybe Payoff)
step = gameTreeM >>= \t -> case t of
    Decision i es -> perform (decide i)
    Chance   d    -> perform (chance (moveDist d))
    Payoff   p    -> return (Just p)
  where perform x = x >>= nextStateM >>= putGameState >> return Nothing

finish :: (Searchable g, Eq (Move g), Show (Move g)) => ExecM g ()
finish = runTree >>= conclude

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
