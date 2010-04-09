{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             TypeFamilies #-}

module Hagl.Iterated.Game where

import Hagl.Base
import Hagl.Iterated.State


--------------------
-- Representation --
--------------------

data Limit = Finite Int | Infinite deriving Eq

data Iterated g = Iterated Limit g

uniterated :: Iterated g -> g
uniterated (Iterated _ g) = g

limit :: Iterated g -> Limit
limit (Iterated l _) = l

limitToMaybe :: Limit -> Maybe Int
limitToMaybe (Finite i) = Just i
limitToMaybe Infinite   = Nothing

reached :: Int -> Limit -> Bool
reached _ Infinite   = False
reached a (Finite b) = a < b


----------------------------------
-- Building Iterated Game Trees --
----------------------------------

iterGameTree :: Limit         -- ^ number of iterations
             -> Int           -- ^ number of players
             -> GameTree s mv -- ^ original uniterated game tree
             -> GameTree (Iter s mv) mv
iterGameTree l np orig = build initIter orig
  where 
    build f (GameTree s node) = case node of
        Internal d es -> GameTree i $ Internal d [(m, next m t) | (m,t) <- es]
          where next m = build (Iter n hist ((moved d m):tran))
        Payoff p | reached n l -> GameTree (Iter n h' [] s) (Payoff (_score h'))
                 | otherwise   -> build (Iter (n+1) h' []) orig
          where h' = (tran, (summarize np tran, Just p)) `dcons` hist
      where i@(Iter n hist tran _) = f s


---------------
-- Instances --
---------------

instance IterGame (Iterated g) g where
  getIter = getExec >>= return . treeState . _location

instance Game g => Game (Iterated g) where
  type Move  (Iterated g)    = Move g
  type State (Iterated g)    = Iter (State g) (Move g)
  gameTree (Iterated l g) np = iterGameTree l np (gameTree g np)

instance Show Limit where
  show (Finite n) = "Iterated " ++ show n ++ " times"
  show Infinite   = "Iterated"

instance Show g => Show (Iterated g) where
  show (Iterated l g) = "(" ++ show l ++ ")\n" ++ show g
