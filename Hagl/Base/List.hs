module Hagl.Base.List where

import Control.Monad       (liftM)
import Control.Monad.Trans (MonadIO(..))
import Data.Monoid         (Monoid(..))

import Hagl.Base.Util

-------------------
-- Distributions --
-------------------
-- TODO: Maybe replace with Martin's probability package?

type Dist a = [(Int, a)]

expandDist :: Dist a -> [a]
expandDist d = concat [replicate i a | (i, a) <- d]

-- Pick an element randomly from a distribution
fromDist :: MonadIO m => Dist a -> m a
fromDist = randomlyFrom . expandDist


-----------------------
-- Dimensioned Lists --
-----------------------

class Functor d => ByX d where
  toList   :: d a -> [a]
  fromList :: [a] -> d a

toList2 :: (ByX f, ByX g) => f (g a) -> [[a]]
toList2 = map toList . toList

toList3 :: (ByX f, ByX g, ByX h) => f (g (h a)) -> [[[a]]]
toList3 = map toList2 . toList

inList :: ByX f => ([a] -> [b]) -> f a -> f b
inList f = fromList . f . toList

setListElem :: ByX f => Int -> a -> f a -> f a
setListElem i a as = fromList (h ++ a : t)
  where (h, _:t) = splitAt i (toList as)

dcons :: ByX f => a -> f a -> f a
dcons = inList . (:)

dlength :: ByX f => f a -> Int
dlength = length . toList

dcross :: ByX f => f [a] -> [f a]
dcross = map fromList . cross . toList

dzipWith :: ByX f => (a -> b -> c) -> f a -> f b -> f c
dzipWith f as bs = fromList (zipWith f (toList as) (toList bs))

-- ByGame, ByTurn, ByPlayer lists

newtype ByGame a = ByGame [a] deriving (Eq, Show)
newtype ByTurn a = ByTurn [a] deriving (Eq, Show)
newtype ByPlayer a = ByPlayer [a] deriving (Eq, Show)

forGame :: Int -> ByGame a -> a
forGame i (ByGame as) = as !! (length as - i)

forTurn :: Int -> ByTurn a -> a
forTurn i (ByTurn as) = as !! (length as - i)

forPlayer :: Int -> ByPlayer a -> a
forPlayer i (ByPlayer as) = as !! (i-1)

forGameM :: Monad m => Int -> m (ByGame a) -> m a
forGameM = liftM . forGame

forTurnM :: Monad m => Int -> m (ByTurn a) -> m a
forTurnM = liftM . forTurn

forPlayerM :: Monad m => Int -> m (ByPlayer a) -> m a
forPlayerM = liftM . forPlayer

class ByX d => ByGameOrTurn d where
  forGameOrTurn  :: Int -> d a -> a
instance ByGameOrTurn ByGame where
  forGameOrTurn = forGame
instance ByGameOrTurn ByTurn where
  forGameOrTurn = forTurn

forGameOrTurnM :: (Monad m, ByGameOrTurn d) => Int -> m (d a) -> m a
forGameOrTurnM = liftM . forGameOrTurn

game's = undefined :: ByGame a
games' = undefined :: ByGame a

turn   = undefined :: ByTurn a
turn's = undefined :: ByTurn a
turns' = undefined :: ByTurn a

-- Instances

instance Functor ByGame where
  fmap f (ByGame as) = ByGame (map f as)
instance Functor ByTurn where
  fmap f (ByTurn as) = ByTurn (map f as)
instance Functor ByPlayer where
  fmap f (ByPlayer as) = ByPlayer (map f as)

instance ByX ByGame where
  toList (ByGame as) = as
  fromList = ByGame
instance ByX ByTurn where
  toList (ByTurn as) = as
  fromList = ByTurn
instance ByX ByPlayer where
  toList (ByPlayer as) = as
  fromList = ByPlayer

instance Monoid (ByGame a) where
  mempty = ByGame []
  mappend (ByGame as) (ByGame bs) = ByGame (as ++ bs)
instance Monoid (ByTurn a) where
  mempty = ByTurn []
  mappend (ByTurn as) (ByTurn bs) = ByTurn (as ++ bs)
instance Monoid (ByPlayer a) where
  mempty = ByPlayer []
  mappend (ByPlayer as) (ByPlayer bs) = ByPlayer (as ++ bs)
