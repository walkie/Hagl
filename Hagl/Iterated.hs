{-# LANGUAGE PatternGuards,TypeFamilies #-}

module Hagl.Iterated where

import Data.Maybe (fromMaybe)
import Data.List (transpose)

import Hagl.Core

data Iterated g = Finite Int g
                | Infinite g

type MoveSummary mv = ByPlayer (ByTurn mv)
type Summary mv     = (MoveSummary mv, Maybe Payoff)
type History mv     = ByGame (Transcript mv, Summary mv)

data Iter s mv = Iter {
  _gameNumber     :: Int,           -- True if this node is the start of a new iteration
  _history        :: History mv,    -- history of all completed game iterations
  _gameTranscript :: Transcript mv, -- the transcript of the current iteration
  _gameState      :: s              -- the state of the current game iteration
}

endAfter :: Iterated g -> Maybe Int
endAfter (Finite n _) = Just n
endAfter _            = Nothing

uniterated :: Iterated g -> g
uniterated (Finite _ g) = g
uniterated (Infinite g) = g

iterations :: Iterated g -> Maybe Int
iterations (Finite n _) = Just n
iterations _            = Nothing

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

-- Game Instance

instance Game g => Game (Iterated g) where
  type Move  (Iterated g) = Move g
  type State (Iterated g) = Iter (State g) (Move g)
  gameTree = iterGameTree 

-- Show Instances

instance Show g => Show (Iterated g) where
  show (Finite n g) = "(Iterated " ++ show n ++ " times)" ++ show g
  show (Infinite g) = "(Iterated)" ++ show g
