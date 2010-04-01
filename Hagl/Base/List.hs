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

-- ByPlayer lists

newtype ByPlayer a = ByPlayer [a] deriving (Eq, Show)

forPlayer :: Int -> ByPlayer a -> a
forPlayer i (ByPlayer as) = as !! (i-1)

forPlayerM :: Monad m => Int -> m (ByPlayer a) -> m a
forPlayerM = liftM . forPlayer

-- Instances

instance Functor ByPlayer where
  fmap f (ByPlayer as) = ByPlayer (map f as)

instance ByX ByPlayer where
  toList (ByPlayer as) = as
  fromList = ByPlayer

instance Monoid (ByPlayer a) where
  mempty = ByPlayer []
  mappend (ByPlayer as) (ByPlayer bs) = ByPlayer (as ++ bs)
