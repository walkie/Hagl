module Hagl.Iterated.List where

import Control.Monad       (liftM)
import Control.Monad.Trans (MonadIO(..))
import Data.Monoid         (Monoid(..))

import Hagl.Base

-- ByGame lists

newtype ByGame a = ByGame [a] deriving (Eq, Show)

forGame :: Int -> ByGame a -> a
forGame i (ByGame as) = as !! (length as - i)

forGameM :: Monad m => Int -> m (ByGame a) -> m a
forGameM = liftM . forGame

game's = undefined :: ByGame a
games' = undefined :: ByGame a

class ByX d => ByGameOrTurn d where
  forGameOrTurn  :: Int -> d a -> a
instance ByGameOrTurn ByGame where
  forGameOrTurn = forGame
instance ByGameOrTurn ByTurn where
  forGameOrTurn = forTurn

forGameOrTurnM :: (Monad m, ByGameOrTurn d) => Int -> m (d a) -> m a
forGameOrTurnM = liftM . forGameOrTurn

-- Instances

instance Functor ByGame where
  fmap f (ByGame as) = ByGame (map f as)

instance ByX ByGame where
  toList (ByGame as) = as
  fromList = ByGame

instance Monoid (ByGame a) where
  mempty = ByGame []
  mappend (ByGame as) (ByGame bs) = ByGame (as ++ bs)
