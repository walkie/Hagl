{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Adds 'ByGame' dimensioned-lists.
module Hagl.Iterated.List where

import Hagl.Base.List (ByX(..))


-- ** ByGame Lists
--

-- | A list where each element corresponds to one played iteration of
--   an iterated game.
newtype ByGame a = ByGame [a] deriving (Eq,Show,Functor)

-- | Return the element corresponding to the given iteration number.
forGame :: Int -> ByGame a -> a
forGame i (ByGame as) = as !! (length as - i)

-- | Return the element corresponding to the first iteration.
firstGame :: ByGame a -> a
firstGame (ByGame []) = error "firstGame: Empty game list."
firstGame (ByGame as) = last as

-- | Return the element corresponding to the current iteration.
thisGame :: ByGame a -> a
thisGame (ByGame []) = error "thisGame: Empty game list."
thisGame (ByGame as) = head as

-- | Return the elements corresponding to all completed iterations.
completedGames :: ByGame a -> [a]
completedGames (ByGame []) = error "completedGames: Empty game list."
completedGames (ByGame as) = tail as

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
  fromList = ByGame
  toList (ByGame as) = as
