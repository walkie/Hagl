-- | Some generic helper functions for manipulating lists and working
--   with monads.  This module is not publicly visible.
module Hagl.Base.Util where

import Control.Monad.State (MonadState,get,modify)
import Control.Monad.Trans (MonadIO,liftIO)
import Data.List     (nub,sort)
import System.Random (RandomGen,randomR)

--
-- * List Utility Functions
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
randomlyFrom :: RandomGen g => [a] -> g -> (a, g)
randomlyFrom as g = (as !! i, g')
  where (i,g') = randomR (0, length as - 1) g


--
-- * Useful Monadic Functions
--

-- | Print a debug message.
debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn

-- | Modify then return the state within a state monad.
update :: MonadState s m => (s -> s) -> m s
update f = modify f >> get
