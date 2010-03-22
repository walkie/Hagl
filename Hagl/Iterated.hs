{-# LANGUAGE FlexibleInstances,
             FunctionalDependencies,
             MultiParamTypeClasses,
             PatternGuards,
             TypeFamilies #-}

module Hagl.Iterated where

import Control.Monad (liftM)

import Data.Maybe (fromMaybe)
import Data.List (transpose)

import Hagl.Core

--------------------
-- Representation --
--------------------

data Iterated g = Finite Int g
                | Infinite g

class IterGame i g | i -> g where
  getIter :: GameM m i => m (Iter (State g) (Move g))

uniterated :: Iterated g -> g
uniterated (Finite _ g) = g
uniterated (Infinite g) = g

endAfter :: Iterated g -> Maybe Int
endAfter (Finite n _) = Just n
endAfter _            = Nothing

---------------------
-- Execution State --
---------------------

type MoveSummary mv = ByPlayer (ByTurn mv)
type Summary mv     = (MoveSummary mv, Maybe Payoff)
type History mv     = ByGame (Transcript mv, Summary mv)

data Iter s mv = Iter {
  _gameNumber     :: Int,           -- True if this node is the start of a new iteration
  _history        :: History mv,    -- history of all completed game iterations
  _gameTranscript :: Transcript mv, -- the transcript of the current iteration
  _gameState      :: s              -- the state of the current game iteration
}

initIter :: s -> Iter s mv
initIter = Iter 1 (ByGame []) []

_transcripts :: History mv -> ByGame (Transcript mv)
_transcripts = fmap fst

_summaries :: History mv -> ByGame (Summary mv)
_summaries = fmap snd

_moves :: Summary mv -> MoveSummary mv
_moves = fst

_payoff :: Summary mv -> Payoff
_payoff = fromMaybe e . snd
  where e = error "Incomplete game does not have a payoff!"

_score :: History mv -> Payoff
_score = ByPlayer . map sum . transpose . toList2 . fmap _payoff . _summaries

---------------
-- Accessors --
---------------

gameNumber :: (GameM m i, IterGame i g) => m Int
gameNumber = liftM _gameNumber getIter

history :: (GameM m i, IterGame i g) => m (History (Move g))
history = liftM _history getIter

gameTranscript :: (GameM m i, IterGame i g) => m (Transcript (Move g))
gameTranscript = liftM _gameTranscript getIter

gameState :: (GameM m i, IterGame i g) => m (State g)
gameState = liftM _gameState getIter

transcripts :: (GameM m i, IterGame i g) => m (ByGame (Transcript (Move g)))
transcripts = liftM _transcripts history

summaries :: (GameM m i, IterGame i g) => m (ByGame (Summary (Move g)))
summaries = liftM _summaries history

-- would be better/easiest to write this with lastGame... in Selector
summary :: (GameM m i, IterGame i g) => m (Summary (Move g))
summary = liftM (head . toList) summaries

moves :: (GameM m i, IterGame i g) => m (MoveSummary (Move g))
moves = liftM _moves summary

payoff :: (GameM m i, IterGame i g) => m Payoff
payoff = liftM _payoff summary

score :: (GameM m i, IterGame i g) => m Payoff
score = liftM _score history

----------------------
-- Helper Functions --
----------------------

summarize :: Int -> Transcript mv -> MoveSummary mv
summarize np t = ByPlayer [ByTurn [mv | (mi,mv) <- t, mi == Just p] | p <- [1..np]]

iterGameTree :: Game g => Iterated g -> Int -> GameTree (Iter (State g) (Move g)) (Move g)
iterGameTree g np = buildIT g np initIter (gameTree (uniterated g) np)

buildIT :: Game g => Iterated g                          -- ^ game definition
        -> Int                                           -- ^ number of players
        -> (State g -> Iter (State g) (Move g))          -- ^ Iter state builder
        -> GameTree (State g) (Move g)                   -- ^ the uniterated game tree
        -> GameTree (Iter (State g) (Move g)) (Move g)
buildIT g np f (GameTree s nt) = case nt of 
    Internal d es -> GameTree i $ Internal d [(m, next m gt) | (m,gt) <- es]
        where next m = buildIT g np (Iter n h ((moved d m):t))
    Payoff p | end       -> GameTree (Iter n h' [] s) (Payoff (_score h'))
             | otherwise -> buildIT g np (Iter (n+1) h' []) (gameTree (uniterated g) np)
        where h' = (t, (summarize np t, Just p)) `dcons` h
  where i@(Iter n h t _) = f s
        end = maybe False (n >=) (endAfter g)

---------------
-- Instances --
---------------

-- Game Instance
instance Game g => Game (Iterated g) where
  type Move  (Iterated g) = Move g
  type State (Iterated g) = Iter (State g) (Move g)
  gameTree = iterGameTree 

instance IterGame (Iterated g) g where
  getIter = getExec >>= return . nodeState . _location

-- Show Instances
instance Show g => Show (Iterated g) where
  show (Finite n g) = "(Iterated " ++ show n ++ " times)" ++ show g
  show (Infinite g) = "(Iterated)" ++ show g
