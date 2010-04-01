module Hagl.Iterated.List where

import Control.Monad       (liftM)
import Control.Monad.Trans (MonadIO(..))
import Data.Monoid         (Monoid(..))

import Hagl.Base

-- ByGame and ByTurn lists

newtype ByGame a = ByGame [a] deriving (Eq, Show)
newtype ByTurn a = ByTurn [a] deriving (Eq, Show)

forGame :: Int -> ByGame a -> a
forGame i (ByGame as) = as !! (length as - i)

forTurn :: Int -> ByTurn a -> a
forTurn i (ByTurn as) = as !! (length as - i)

forGameM :: Monad m => Int -> m (ByGame a) -> m a
forGameM = liftM . forGame

forTurnM :: Monad m => Int -> m (ByTurn a) -> m a
forTurnM = liftM . forTurn

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

instance ByX ByGame where
  toList (ByGame as) = as
  fromList = ByGame
instance ByX ByTurn where
  toList (ByTurn as) = as
  fromList = ByTurn

instance Monoid (ByGame a) where
  mempty = ByGame []
  mappend (ByGame as) (ByGame bs) = ByGame (as ++ bs)
instance Monoid (ByTurn a) where
  mempty = ByTurn []
  mappend (ByTurn as) (ByTurn bs) = ByTurn (as ++ bs)
