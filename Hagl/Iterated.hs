{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module Hagl.Iterated where

import Control.Monad (liftM)
import Data.Maybe    (fromMaybe)
import Data.List     (transpose)

import Hagl.Lists
import Hagl.Game
import Hagl.GameTree
import Hagl.Exec

--
-- * ByGame lists
--

-- | A list where each element corresponds to one played iteration of
--   an iterated game.
newtype ByGame a = ByGame [a] deriving (Eq,Show,Functor)

-- | Return the element corresponding to the given iteration number.
forGame :: Int -> ByGame a -> a
forGame i (ByGame as) = as !! (length as - i)

-- | Add an element corresponding to a new game iteration.
addForNewGame :: a -> ByGame a -> ByGame a
addForNewGame a (ByGame as) = ByGame (a:as)

-- | Return the elements corresponding to every iteration (all elements as a
--   plain list).
everyGame :: ByGame a -> [a]
everyGame (ByGame as) = as

-- | Return the elements corresponding to all completed iterations.
completedGames :: ByGame a -> [a]
completedGames (ByGame []) = error "completedGames: Empty game list."
completedGames (ByGame as) = tail as

-- | Return the element corresponding to the first iteration.
firstGame :: ByGame a -> a
firstGame (ByGame []) = error "firstGame: Empty game list."
firstGame (ByGame as) = last as

-- | Return the element corresponding to the current iteration.
thisGame :: ByGame a -> a
thisGame (ByGame []) = error "thisGame: Empty game list."
thisGame (ByGame as) = head as

-- | Return the element corresponding to the most recently completed iteration.
lastGame :: ByGame a -> a
lastGame (ByGame [])      = error "lastGame: Empty game list."
lastGame (ByGame [a])     = error "lastGame: No completed games."
lastGame (ByGame (_:a:_)) = a

-- | Return the elements corresponding to the most recently completed n
--   iterations of the game.
lastNGames :: Int -> ByGame a -> [a]
lastNGames _ (ByGame []) = error "lastNGames: Empty game list."
lastNGames i (ByGame (_:as))
    | length as' == i = as'
    | otherwise       = error "lastNGames: Not enough games."
  where as' = take i as

-- Instances

instance ByX ByGame where
  toAssocList (ByGame l) = zip [length l ..] l
  minX = firstGame
  maxX = thisGame


-- ** ByGame selectors
--

-- | Selects the elements corresponding to all iterations (i.e. all elements).
everyGames' :: GameM m g => m (ByGame a) -> m [a]
everyGames' = liftM everyGame

-- | Selects the elements correspondings to all completed iterations.
completedGames' :: GameM m g => m (ByGame a) -> m [a]
completedGames' = liftM completedGames

-- | Selects the element corresponding to the first iteration.
firstGame's :: GameM m g => m (ByGame a) -> m a
firstGame's = liftM firstGame

-- | Selects the element corresponding to the current iteration.
thisGame's :: GameM m g => m (ByGame a) -> m a
thisGame's = liftM thisGame

-- | Selects the element corresponding to the most recently completed iteration.
lastGame's :: GameM m g => m (ByGame a) -> m a
lastGame's = liftM lastGame

-- | Selects the elements corresponding to the last `n` completed iterations of the game.
lastNGames' :: GameM m g => Int -> m (ByGame a) -> m [a]
lastNGames' i = liftM (lastNGames i)


--
-- * Representation
--

-- | An iterated game can have either a finite or infinite number of iterations.
data Limit = Finite Int | Infinite deriving Eq

-- | Representation of iterated games.
data Iterated g = Iterated Limit g

-- | Construct an infinitely iterated game from a non-iterated game.
iterated :: g -> Iterated g
iterated = Iterated Infinite

-- | Get the uniterated form of a game.
uniterated :: Iterated g -> g
uniterated (Iterated _ g) = g

-- | Get the limit of an iterated game.
limit :: Iterated g -> Limit
limit (Iterated l _) = l

-- | `Just n` if the limit is finite, `Nothing` otherwise.
limitToMaybe :: Limit -> Maybe Int
limitToMaybe (Finite i) = Just i
limitToMaybe Infinite   = Nothing

-- | Given a number of iterations, has the limit been reached?
reached :: Int -> Limit -> Bool
reached _ Infinite   = False
reached a (Finite b) = a < b


--
-- Iterated game execution state
--

-- | Summary of each iteration: a summary of moves by each player, and
--   a payoff if the game is complete.
type Summary mv = (MoveSummary mv, Maybe Payoff)

-- | The execution history of an iterated game: a transcript and summary
--   of each completed game.
type History mv = ByGame (Transcript mv, Summary mv)

-- | Iterated game execution state.
data Iter s mv = Iter {
  _gameNumber     :: Int,           -- ^ The current iteration number.
  _history        :: History mv,    -- ^ History of all completed game iterations.
  _gameTranscript :: Transcript mv, -- ^ The transcript of the current iteration.
  _gameState      :: s              -- ^ The state of the current game iteration.
}

-- | Initial iterated game execution state.
initIter :: s -> Iter s mv
initIter = Iter 1 (ByGame []) []

-- | Get the transripts from a history.
_transcripts :: History mv -> ByGame (Transcript mv)
_transcripts = fmap fst

-- | Get the iteration summaries from a history.
_summaries :: History mv -> ByGame (Summary mv)
_summaries = fmap snd

-- | Get the move summaries from an iteration summary.
_moveSummary :: Summary mv -> MoveSummary mv
_moveSummary = fst

-- | Get the payoff from the summary of a completed game.
_payoff :: Summary mv -> Payoff
_payoff = fromMaybe e . snd
  where e = error "Incomplete game does not have a payoff!"

-- | Compute the current score from a history.
_score :: History mv -> Payoff
_score = ByPlayer . map sum . transpose .  -- calculate score
         map everyPlayer . everyGame .     -- convert to plain lists
         fmap _payoff . _summaries         -- get payoffs for each game


instance Game g => Game (Iterated g) where
  
  type Move  (Iterated g) = Move g
  type State (Iterated g) = Iter (State g) (Move g)
  
  start (Iterated _ g) = (initIter s, a)
    where (s,a) = start g

  transition (Iterated l g) (Iter n h t s, a) m =
      case transition g (s,a) m of
        (s',Payoff p) | reached n l -> (Iter n h' [] s', Payoff (_score h'))
                      | otherwise   -> (Iter (n+1) h' [] (startState g), startAction g)
          where np = length (toAssocList p)
                h' = addForNewGame (t', (summarize np t', Just p)) h
        (s',a') -> (Iter n h t' s', a')
    where t' = moveEvent a m : t


-------------------------
-- Iterated Game Trees --
-------------------------

{-

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
-}
