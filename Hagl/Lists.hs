{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module provides list utility functions and specialized list
--   representations used within Hagl.
module Hagl.Lists where

import Data.List     (elemIndex,nub,sort)
import System.Random (RandomGen,randomR)


--
-- * Probability Distributions
--

-- | A probability distribution.  The integer coefficients indicate the
--   relative likelihood of each element of type 'a'.  For example,
--   given distribution @[(3,True),(1,False)]@, @True@ is three times as
--   likely as @False@.
type Dist a = [(Int,a)]

-- | Expand a distribution into a list of values according to their
--   relative likelihoods.
--
--   >>> expandDist [(3,True),(1,False)]
--   [True,True,True,False]
expandDist :: Dist a -> [a]
expandDist d = concat [replicate i a | (i, a) <- d]

-- | Pick an element randomly from a distribution.
fromDist :: RandomGen g => Dist a -> g -> (a, g)
fromDist = randomlyFrom . expandDist


--
-- * Dimensioned Lists
--

-- | A class for finite integer-indexed lists where each integer
--   corresponds to an element in a set X.
--   Minimum definition is @toList@, @fromList@, @indexes@.
--   Others can be overridden for efficiency.
class Functor d => ByX d where
  
  -- | The underlying plain list.
  toList :: d a -> [a]
  
  -- | Convert from a plain list.
  fromList :: [a] -> d a
  
{-
  -- | List of indexes corresponding to the underlying plain list.
  indexes :: d a -> [Int]
  
  -- | Get the element corresponding to the given index.
  forX :: Int -> d a -> a
  forX i d = maybe err (toList d !!) (elemIndex i (indexes d))
    where err = error $ "forX: invalid index: " ++ show i
          
  -- | Set the element at the given index.
  setX :: Int -> a -> d a -> d a 
  setX i a d = maybe err (fromList . setAt) (elemIndex i (indexes d))
    where err = error $ "setX: invalid index: " ++ show i
          setAt j = let (h,_:t) = splitAt j (toList d) in (h ++ a:t)

  -- | Get the element corresponding to the minimum index in the list.
  minX :: d a -> a
  minX d | null ixs  = error $ "minX: empty list"
         | otherwise = forX (minimum ixs) d
    where ixs = indexes d
  
  -- | Get the element corresponding to the maximum index in the list.
  maxX :: d a -> a
  maxX d | null ixs  = error $ "maxX: empty list"
         | otherwise = forX (maximum ixs) d
    where ixs = indexes d
-}

-- | Convert a nested dimensioned list into a nested plain list.
toList2 :: (ByX f, ByX g) => f (g a) -> [[a]]
toList2 = map toList . toList

-- | Convert a nested dimensioned list into a nested plain list.
toList3 :: (ByX f, ByX g, ByX h) => f (g (h a)) -> [[[a]]]
toList3 = map toList2 . toList

-- | Apply some function to the underlying plain list.
onList :: ByX f => ([a] -> [b]) -> f a -> f b
onList f = fromList . f . toList

-- | Prepend an element to a dimensioned list.
dcons :: ByX f => a -> f a -> f a
dcons = onList . (:)

-- | Length of a dimensioned list.
dlength :: ByX f => f a -> Int
dlength = length . toList

-- | Apply `cross` to a dimensioned list of lists.
dcross :: ByX f => f [a] -> [f a]
dcross = map fromList . cross . toList

-- | Apply zipWith to two like-dimensioned lists.
dzipWith :: ByX f => (a -> b -> c) -> f a -> f b -> f c
dzipWith f as bs = fromList (zipWith f (toList as) (toList bs))


-- ** ByPlayer Lists
--

-- | A player ID is used to index a `ByPlayer` list.
type PlayerID = Int

-- | A list where each element corresponds to a particular player.
newtype ByPlayer a = ByPlayer [a] deriving (Eq,Show,Functor)

-- | Return the element corresponding to the given `PlayerID`.
forPlayer :: PlayerID -> ByPlayer a -> a
forPlayer i (ByPlayer as) = as !! (i-1)

-- | The next player ID out of @n@ players.
nextPlayer :: Int -> PlayerID -> PlayerID
nextPlayer n p | p >= n    = 1
               | otherwise = p + 1


-- ** ByTurn Lists
--

-- | A list where each element corresponds to a played turn in a game.
newtype ByTurn a = ByTurn [a] deriving (Eq,Show,Functor)

-- | Return the element corresponding to the given turn number.
forTurn :: Int -> ByTurn a -> a
forTurn i (ByTurn as) = as !! (length as - i)

-- | Return the element corresponding to the first turn of the game.
firstTurn :: ByTurn a -> a
firstTurn (ByTurn []) = error "firstTurn: Empty turn list."
firstTurn (ByTurn as) = last as

-- | Return the element corresponding to the most recently played turn.
lastTurn :: ByTurn a -> a
lastTurn (ByTurn []) = error "lastTurn: Empty turn list."
lastTurn (ByTurn as) = head as

-- | Return the elements corresponding to the most recently played n
--   turns of the game.
lastNTurns :: Int -> ByTurn a -> [a]
lastNTurns n (ByTurn as)
    | length as' == n = as'
    | otherwise       = error $ "lastNTurns: Not enough turns: " ++ show n
  where as' = take n as

-- Instances

instance ByX ByPlayer where
  fromList = ByPlayer
  toList (ByPlayer as) = as
instance ByX ByTurn where
  fromList = ByTurn
  toList (ByTurn as) = as


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
