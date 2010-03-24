module Hagl.Base.Util where

import Control.Monad.State
import Data.List     (nub, sort)
import System.Random (randomRIO)

----------------------------
-- List Utility Functions --
----------------------------

cross :: [[a]] -> [[a]]
cross (xs:xss) = [y:ys | y <- xs, ys <- cross xss]
cross [] = [[]]

ucross :: (Ord a) => [[a]] -> [[a]]
ucross = nub . map sort . cross

-- Break a list into n-sized chunks.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = take n l : chunk n (drop n l)

randomIndex :: MonadIO m => [a] -> m Int
randomIndex as = liftIO $ randomRIO (0, length as - 1)

-- Pick an element randomly from a list.
randomlyFrom :: MonadIO m => [a] -> m a
randomlyFrom as = liftM (as !!) (randomIndex as)


------------------------------
-- Useful Monadic Functions --
------------------------------

debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn

update :: MonadState s m => (s -> s) -> m s
update f = modify f >> get
