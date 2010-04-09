module Hagl.Iterated.List where

import Control.Monad       (liftM)
import Control.Monad.Trans (MonadIO(..))
import Data.Monoid         (Monoid(..))

import Hagl.Base

-- ByGame lists

newtype ByGame a = ByGame [a] deriving (Eq, Show)

forGame :: Int -> ByGame a -> a
forGame i (ByGame as) = as !! (length as - i)

firstGame :: ByGame a -> a
firstGame (ByGame []) = error "firstGame: Empty game list."
firstGame (ByGame as) = last as

thisGame :: ByGame a -> a
thisGame (ByGame []) = error "thisGame: Empty game list."
thisGame (ByGame as) = head as

completedGames :: ByGame a -> [a]
completedGames (ByGame []) = error "completedGames: Empty game list."
completedGames (ByGame as) = tail as

lastGame :: ByGame a -> a
lastGame (ByGame [])      = error "lastGame: Empty game list."
lastGame (ByGame [a])     = error "lastGame: No completed games."
lastGame (ByGame (_:a:_)) = a

lastNGames :: Int -> ByGame a -> [a]
lastNGames _ (ByGame []) = error "lastNGames: Empty game list."
lastNGames i (ByGame (_:as))
    | length as' == i = as'
    | otherwise       = error "lastNGames: Not enough games."
  where as' = take i as

-- Instances

instance Functor ByGame where
  fmap f (ByGame as) = ByGame (map f as)

instance ByX ByGame where
  toList (ByGame as) = as
  fromList = ByGame

instance Monoid (ByGame a) where
  mempty = ByGame []
  mappend (ByGame as) (ByGame bs) = ByGame (as ++ bs)
