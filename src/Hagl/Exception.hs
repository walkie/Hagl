module Hagl.Exception where

import Control.Exception (Exception)
import Data.Data (Typeable)


-- | Exception thrown when an invalid move is played.
data InvalidMove mv = InvalidMove mv
  deriving (Typeable)

instance Show mv => Show (InvalidMove mv) where
  show (InvalidMove mv) = "Invalid move: " ++ show mv

instance (Show mv, Typeable mv) => Exception (InvalidMove mv)


-- | Exception thrown when an element is referenced that does not exist.
data NoSuchElement = NoSuchElement String
  deriving (Typeable)

instance Show NoSuchElement where
  show (NoSuchElement s) = "No such element: " ++ s

instance Exception NoSuchElement


-- | Exception thrown for any other kind of error not explicitly represented.
data OtherError = OtherError String
  deriving (Typeable)

instance Show OtherError where
  show (OtherError s) = "Other error: " ++ s

instance Exception OtherError
