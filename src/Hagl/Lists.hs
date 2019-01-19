{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module provides list utility functions and specialized list
--   representations used within Hagl.
module Hagl.Lists where

import Control.Monad.IO.Class

import Data.List     (elemIndex,intercalate,nub,sort)
import Data.Maybe    (fromJust)
import System.Random (randomRIO)


--
-- * Probability distributions
--

-- | A probability distribution.  The integer coefficients indicate the
--   relative likelihood of each element of type 'a'.  For example,
--   given distribution @[(3,True),(1,False)]@, @True@ is three times as
--   likely as @False@.
type Dist a = [(Int,a)]

-- | Expand a distribution into a list of values according to their
--   relative likelihoods.
--
--   >>> expandDist [(3,True),(1,False)]
--   [True,True,True,False]
expandDist :: Dist a -> [a]
expandDist d = concat [replicate i a | (i, a) <- d]

-- | Pick an element randomly from a distribution.
fromDist :: MonadIO m => Dist a -> m a
fromDist = randomlyFrom . expandDist


--
-- * Dimensioned lists
--

-- | A class for finite integer-indexed lists where each integer
--   corresponds to an element in a set X.
--   Minimum definition is @toList@, @fromList@, @indexes@.
--   Others can be overridden for efficiency.
class Functor d => ByX d where

  -- | Convert the dimensioned list into an association list from
  --   indexes to values.
  toAssocList :: d a -> [(Int, a)]

  -- | Get the element corresponding to the given index.
  forX :: Int -> d a -> Maybe a
  forX i = lookup i . toAssocList

  -- | Get the element corresponding to the smallest index in the list.
  minX :: d a -> a
  minX l | null ixs  = error "minX: empty list"
         | otherwise = fromJust $ forX (minimum ixs) l
    where ixs = map fst (toAssocList l)
  
  -- | Get the element corresponding to the largest index in the list.
  maxX :: d a -> a
  maxX l | null ixs  = error "maxX: empty list"
         | otherwise = fromJust $ forX (maximum ixs) l
    where ixs = map fst (toAssocList l)

-- | Length of a dimensioned list.
dlength :: ByX d => d a -> Int
dlength = length . toAssocList

-- | Is this dimensioned list empty?
dnull :: ByX d => d a -> Bool
dnull = null . toAssocList


-- ** ByPlayer lists
--

-- | A player ID is used to index a `ByPlayer` list.
type PlayerID = Int

-- | A list where each element corresponds to a particular player.
newtype ByPlayer a = ByPlayer [a] deriving (Eq,Show,Functor)

-- | Return the element corresponding to the given `PlayerID`.
forPlayer :: PlayerID -> ByPlayer a -> a
forPlayer i (ByPlayer as) | i <= n    = as !! (i-1)
                          | otherwise = error e
  where n = length as
        e = "forPlayer: PlayerID=" ++ show i ++ " numPlayers=" ++ show n

-- | Return the elements corresponding to every player (all elements as
--   a plain list).
everyPlayer :: ByPlayer a -> [a]
everyPlayer (ByPlayer as) = as

-- | The next player ID out of @n@ players.
nextPlayer :: Int -> PlayerID -> PlayerID
nextPlayer n p | p >= n    = 1
               | otherwise = p + 1

-- | Set the element corresponding to a the given player ID.
setForPlayer :: PlayerID -> a -> ByPlayer a -> ByPlayer a
setForPlayer p a (ByPlayer l) = ByPlayer (h ++ a:t)
  where (h,_:t) = splitAt (p-1) l

instance ByX ByPlayer where
  toAssocList (ByPlayer l) = zip [1 ..] l


-- ** ByTurn lists
--

-- | A list where each element corresponds to a played turn in a game.
newtype ByTurn a = ByTurn [a] deriving (Eq,Show,Functor)

-- | Return the element corresponding to the given turn number.
forTurn :: Int -> ByTurn a -> a
forTurn i (ByTurn as) = as !! (length as - i)

-- | Return the elements corresponding to every turn (all elements as
--   a plain list).
everyTurn :: ByTurn a -> [a]
everyTurn (ByTurn as) = as

-- | Return the element corresponding to the first turn of the game.
firstTurn :: ByTurn a -> a
firstTurn (ByTurn []) = error "firstTurn: Empty turn list."
firstTurn (ByTurn as) = last as

-- | Return the element corresponding to the most recently played turn.
lastTurn :: ByTurn a -> a
lastTurn (ByTurn []) = error "lastTurn: Empty turn list."
lastTurn (ByTurn as) = head as

-- | Return the elements corresponding to the most recently played n
--   turns of the game.
lastNTurns :: Int -> ByTurn a -> [a]
lastNTurns n (ByTurn as)
    | length as' == n = as'
    | otherwise       = error $ "lastNTurns: Not enough turns: " ++ show n
  where as' = take n as


instance ByX ByTurn where
  toAssocList (ByTurn l) = zip [length l ..] l
  minX = firstTurn
  maxX = lastTurn


-- ** ByGame lists
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


instance ByX ByGame where
  toAssocList (ByGame l) = zip [length l ..] l
  minX = firstGame
  maxX = thisGame


-- * List utility functions
--

-- | Produce a list of all ordered combinations of elements drawn from each
--   sublist of the argument.  Probably best demonstrated by example.
--
--   >>> cross [[1,2],[3,4],[5,6]]
--   [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]] 
-- 
--   >>> cross [[1,2],[2,2,1]]
--   [[1,2],[1,2],[1,1],[2,2],[2,2],[2,1]]
cross :: [[a]] -> [[a]]
cross (xs:xss) = [y:ys | y <- xs, ys <- cross xss]
cross [] = [[]]

-- | Similar to 'cross' but does not preserve the ordering elements in the
--   argument list and returns only unique combinations.
--
--   >>> ucross [[1,2],[2,2,1]]
--   [[1,2],[1,1],[2,2]]
ucross :: (Ord a) => [[a]] -> [[a]]
ucross = nub . map sort . cross

-- | Break a list into n-sized chunks.
--
--   >>> chunk 4 [1..9]
--   [[1,2,3,4],[5,6,7,8],[9]]
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = take n l : chunk n (drop n l)

-- | Pick an element randomly from a list.
randomlyFrom :: MonadIO m => [a] -> m a
randomlyFrom as = do
  i <- liftIO $ randomRIO (0, length as -1) 
  return (as !! i)

-- | Concatenate a sequence of elements, separated by commas.
showSeq :: [String] -> String
showSeq = intercalate ","
