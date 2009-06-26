{-# LANGUAGE FlexibleContexts #-}
module Hagl.GameTree where

import Data.List
import Data.Maybe
import qualified Data.Tree as Tree

import Hagl.Core
import Hagl.Accessor
import Hagl.Game
import Hagl.Exec

----------------
-- Game Trees --
----------------

type Edge mv = (mv, GameTree mv)

data GameTree mv = Decision PlayerIx [Edge mv] -- decision made by a player
                 | Chance (Dist (Edge mv))     -- random move from distribution
                 | Payoff Payoff               -- terminatinmv payoff
                 deriving Eq

-- The moves available from a node.
movesFrom :: GameTree mv -> [mv]
movesFrom (Decision _ es) = [m | (m,_) <- es]
movesFrom (Chance d) = [m | (_,(m,_)) <- d]
movesFrom _ = []

-- The immediate children of a node.
children :: GameTree mv -> [GameTree mv]
children (Decision _ es) = [n | (_,n) <- es]
children (Chance d) = [n | (_,(_,n)) <- d]
children _ = []

doMove :: (Eq mv, Show mv) => mv -> GameTree mv -> GameTree mv
doMove m t = case t of
    Decision _ es -> look es
    Chance d      -> look (map snd d)
  where look = fromMaybe (error ("move not found: " ++ show m)) . lookup m

location :: (Searchable g, GameM m g) => m (GameTree (Move g))
location = gameTreeM

-- Nodes in BFS order.
bfs :: GameTree mv -> [GameTree mv]
bfs t = let b [] = []
            b ns = ns ++ b (concatMap children ns)
        in b [t]

-- Nodes DFS order.
dfs :: GameTree mv -> [GameTree mv]
dfs t = t : concatMap dfs (children t)

-- The highest numbered player in this finite game tree.
maxPlayer :: GameTree mv -> Int
maxPlayer t = foldl1 max $ map player (dfs t)
  where player (Decision p _) = p
        player _ = 0

----------------------
-- Searchable Class --
----------------------

class Game g => Searchable g where
  gameTree :: g -> State g -> GameTree (Move g)
  nextState :: g -> State g -> Move g -> State g

gameTreeM :: (Searchable g, GameM m g) => m (GameTree (Move g))
gameTreeM = do g <- game
               s <- gameState
               return (gameTree g s)

nextStateM :: (Searchable g, GameM m g) => Move g -> m (State g)
nextStateM m = do g <- game
                  s <- gameState
                  return (nextState g s m)

step :: (Searchable g, Eq (Move g), Show (Move g)) => ExecM g (Maybe Payoff)
step = gameTreeM >>= \t -> case t of
  Decision i es ->
    do m <- decide i 
       s <- nextStateM m
       putGameState s
       return Nothing
  Chance d ->
    do (m, _) <- fromDist d
       chanceMoved m
       s <- nextStateM m 
       putGameState s
       return Nothing
  Payoff p -> return (Just p)

finish :: (Searchable g, Eq (Move g), Show (Move g)) => ExecM g ()
finish = once

runTree :: (Searchable g, Eq (Move g), Show (Move g)) => ExecM g Payoff
runTree = step >>= maybe runTree return

---------------
-- Instances --
---------------

instance Show mv => Show (GameTree mv) where
  show t = condense (Tree.drawTree (tree "" t))
    where str (Decision p es) = "Player " ++ show p
          str (Chance d) = "Chance"
          str (Payoff (ByPlayer vs)) = show vs
          sub (Decision p es) = [tree (show m ++ " -> ") t | (m,t) <- es]
          sub (Chance d) = [tree (show i ++ " * " ++ show m ++ " -> ") t | (i,(m,t)) <- d]
          sub (Payoff _) = []
          tree pre t = Tree.Node (pre ++ str t) (sub t)
          condense s = let empty = not . all (\c -> c == ' ' || c == '|')
                       in unlines $ filter empty $ lines s
