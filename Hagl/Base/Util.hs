-- | Some generic helper functions for manipulating lists and working
--   with monads.  This module is not publicly visible.
module Hagl.Base.Util where

import Control.Monad.State (MonadState,get,modify)
import Control.Monad.Trans (MonadIO,liftIO)
import Data.List     (nub,sort)
import System.Random (RandomGen,randomR)

--
-- * Useful Monadic Functions
--

-- | Print a debug message.
debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn

-- | Modify then return the state within a state monad.
update :: MonadState s m => (s -> s) -> m s
update f = modify f >> get
