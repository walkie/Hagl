{-# LANGUAGE TypeFamilies #-}
module Hagl.Extensive where

import Data.List

import Hagl.Core
import Hagl.Game
import Hagl.GameTree

-- Extensive form game
data Extensive mv = Extensive Int (GameTree mv -> Info mv) (GameTree mv) 

-- Information group
data Info mv = Perfect (GameTree mv)
             | Imperfect [GameTree mv]
             | Simultaneous

-- The number of players that play this game.
numPlayers :: Extensive mv -> Int
numPlayers (Extensive np _ _) = np

-- Construct a perfect information game from a finite GameTree.
extensive :: GameTree mv -> Extensive mv
extensive t = Extensive (maxPlayer t) Perfect t

-- Construct a decision node with only one option.
player :: PlayerIx -> Edge mv -> GameTree mv
player i e = Decision i [e]

pays :: [Float] -> GameTree mv
pays = Payoff . ByPlayer

-- Combines two game trees.
(<+>) :: GameTree mv -> GameTree mv -> GameTree mv
Payoff     as <+> Payoff bs = Payoff (dzipWith (+) as bs)
Chance     as <+> Chance bs = Chance (as ++ bs)
Decision a as <+> Decision b bs | a == b = Decision a (as ++ bs)

-- Add a decision branch to a game tree.
(<|>) :: GameTree mv -> Edge mv -> GameTree mv
Decision i ms <|> m = Decision i (m:ms)

---------------
-- Instances --
---------------

instance (Eq mv, Show mv) => Game (Extensive mv) where
  type Move (Extensive mv) = mv
  type State (Extensive mv) = GameTree mv
  initState (Extensive _ _ t) = t
  runGame = runTree

instance (Eq mv, Show mv) => Searchable (Extensive mv) where
  gameTree _ s = s
  nextState _ s m = doMove m s

-- Eq
instance Eq mv => Eq (Extensive mv) where
  (Extensive n1 _ t1) == (Extensive n2 _ t2) = n1 == n2 && t1 == t2

instance Eq mv => Eq (Info mv) where
  (Perfect t1) == (Perfect t2) = t1 == t2
  (Imperfect t1) == (Imperfect t2) = all (flip elem t2) t1
  Simultaneous == Simultaneous = True
  _ == _ = False

-- Show
instance Show mv => Show (Extensive mv) where
  show (Extensive _ _ t) = show t

instance Show mv => Show (Info mv) where
  show (Perfect t) = show t
  show (Imperfect ts) = unlines $ intersperse "*** OR ***" (map show ts)
  show Simultaneous = "Cannot show this location in the game tree."
