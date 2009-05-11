
module Game.Lists where

import Control.Monad
import Data.Monoid

data ByGame a = ByGame [a] deriving (Eq, Show)
data ByTurn a = ByTurn [a] deriving (Eq, Show)
data ByPlayer a = ByPlayer [a] deriving (Eq, Show)

instance Functor ByGame where
  fmap f (ByGame as) = ByGame (map f as)
instance Functor ByTurn where
  fmap f (ByTurn as) = ByTurn (map f as)
instance Functor ByPlayer where
  fmap f (ByPlayer as) = ByPlayer (map f as)

instance Monoid (ByGame a) where
  mempty = ByGame []
  mappend (ByGame as) (ByGame bs) = ByGame (as ++ bs)
instance Monoid (ByTurn a) where
  mempty = ByTurn []
  mappend (ByTurn as) (ByTurn bs) = ByTurn (as ++ bs)
instance Monoid (ByPlayer a) where
  mempty = ByPlayer []
  mappend (ByPlayer as) (ByPlayer bs) = ByPlayer (as ++ bs)

forGame :: ByGame a -> Int -> a
forGame (ByGame as) i = as !! (length as - i)

forTurn :: ByTurn a -> Int -> a
forTurn (ByTurn as) i = as !! (length as - i)

forPlayer :: ByPlayer a -> Int -> a
forPlayer (ByPlayer as) i = as !! (i-1)

forGameM :: Monad m => m (ByGame a) -> Int -> m a
forGameM l i = liftM (flip forGame i) l

forTurnM :: Monad m => m (ByTurn a) -> Int -> m a
forTurnM l i = liftM (flip forTurn i) l

forPlayerM :: Monad m => m (ByPlayer a) -> Int -> m a
forPlayerM l i = liftM (flip forPlayer i) l

class DList d => ByGameOrTurn d where
  forGameOrTurn  :: d a -> Int -> a
  forGameOrTurnM :: Monad m => m (d a) -> Int -> m a
instance ByGameOrTurn ByGame where
  forGameOrTurn = forGame
  forGameOrTurnM = forGameM
instance ByGameOrTurn ByTurn where
  forGameOrTurn = forTurn
  forGameOrTurnM = forTurnM

game's = undefined :: ByGame a
games' = undefined :: ByGame a

turn   = undefined :: ByTurn a
turn's = undefined :: ByTurn a
turns' = undefined :: ByTurn a

class Functor d => DList d where
  toList :: d a -> [a]
instance DList ByGame where
  toList (ByGame as) = as
instance DList ByTurn where
  toList (ByTurn as) = as
instance DList ByPlayer where
  toList (ByPlayer as) = as

toList2 :: (DList f, DList g) => f (g a) -> [[a]]
toList2 = map toList . toList

toList3 :: (DList f, DList g, DList h) => f (g (h a)) -> [[[a]]]
toList3 = map toList2 . toList
