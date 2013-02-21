{-# LANGUAGE TypeFamilies #-}

module Hagl.Iterated.Game where

import Data.Maybe (fromMaybe)
import Data.List  (transpose)

import Hagl.Base
import Hagl.Iterated.List


--------------------
-- Representation --
--------------------

data Limit = Finite Int | Infinite deriving Eq

data Iterated g = Iterated Limit g

iterated :: g -> Iterated g
iterated = Iterated Infinite

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


---------------------
-- Execution State --
---------------------

type Summary mv = (MoveSummary mv, Maybe Payoff)
type History mv = ByGame (Transcript mv, Summary mv)

data Iter s mv = Iter {
  _gameNumber     :: Int,           -- ^ the current iteration number
  _history        :: History mv,    -- ^ history of all completed game iterations
  _gameTranscript :: Transcript mv, -- ^ the transcript of the current iteration
  _gameState      :: s              -- ^ the state of the current game iteration
}

initIter :: s -> Iter s mv
initIter = Iter 1 (ByGame []) []

_transcripts :: History mv -> ByGame (Transcript mv)
_transcripts = fmap fst

_summaries :: History mv -> ByGame (Summary mv)
_summaries = fmap snd

_moveSummary :: Summary mv -> MoveSummary mv
_moveSummary = fst

_payoff :: Summary mv -> Payoff
_payoff = fromMaybe e . snd
  where e = error "Incomplete game does not have a payoff!"

_score :: History mv -> Payoff
_score = ByPlayer . map sum . transpose . toList2 . fmap _payoff . _summaries


-------------------------
-- Iterated Game Trees --
-------------------------

type IterGameTree s mv = GameTree (Iter s mv) mv

iterGameTree :: Limit         -- ^ number of iterations
             -> Int           -- ^ number of players
             -> GameTree s mv -- ^ original uniterated game tree
             -> IterGameTree s mv
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

instance Game g => Game (Iterated g) where
  type Move  (Iterated g)    = Move g
  type State (Iterated g)    = Iter (State g) (Move g)
  gameTree (Iterated l g) np = iterGameTree l np (gameTree g np)

instance Show Limit where
  show (Finite n) = "Iterated " ++ show n ++ " times"
  show Infinite   = "Iterated"

instance Show g => Show (Iterated g) where
  show (Iterated l g) = "(" ++ show l ++ ")\n" ++ show g
