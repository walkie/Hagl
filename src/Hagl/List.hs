-- | This module provides list utility functions and specialized list
--   representations used within Hagl.
module Hagl.List where

import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow,throwM)

import Data.List (intercalate,nub,sort)
import System.Random (randomRIO)

import Hagl.Exception


--
-- * Probability distributions
--

-- | A probability distribution. The integer coefficients indicate the
--   relative likelihood of each element of type 'a'. For example, given
--   distribution @[(3,True),(1,False)]@, the value @True@ is three times
--   as likely as @False@.
type Dist a = [(Int,a)]

-- | Expand a distribution into a list of values according to their relative
--   likelihoods.
--
--   >>> expandDist [(3,True),(1,False)]
--   [True,True,True,False]
--
expandDist :: Dist a -> [a]
expandDist d = concat [replicate i a | (i, a) <- d]

-- | Pick an element randomly from a distribution.
fromDist :: MonadIO m => Dist a -> m a
fromDist = randomlyFrom . expandDist


--
-- * Dimensioned lists
--

-- ** ByPlayer lists

-- | A player ID is used to index a `ByPlayer` list.
type PlayerID = Int

-- | A list where each element corresponds to a particular player.
newtype ByPlayer a = ByPlayer [a]
  deriving (Eq,Show,Foldable,Functor,Traversable)

-- | Throw an exception for a bad PlayerID argument.
throwBadPlayerID :: MonadThrow m => Int -> PlayerID -> m a
throwBadPlayerID np i = throwM (NoSuchElement msg)
  where msg = "Bad PlayerID: " ++ show i ++ ", number of players: " ++ show np

-- | Return the element corresponding to the given `PlayerID`.
forPlayer :: MonadThrow m => PlayerID -> ByPlayer a -> m a
forPlayer i (ByPlayer as)
    | i > 0 && i <= n = return (as !! (i-1))
    | otherwise       = throwBadPlayerID n i
  where n = length as

-- | Return the elements corresponding to every player (all elements as
--   a plain list).
everyPlayer :: ByPlayer a -> [a]
everyPlayer (ByPlayer as) = as

-- | The next player ID out of @n@ players.
nextPlayer :: Int -> PlayerID -> PlayerID
nextPlayer n i | i < 0 || i >= n = 1
               | otherwise       = i + 1

-- | Modify the element corresponding to a given player ID.
modifyForPlayer :: MonadThrow m => PlayerID -> (a -> a) -> ByPlayer a -> m (ByPlayer a)
modifyForPlayer i f (ByPlayer as)
    | i > 0 && i <= np = return (ByPlayer (pre ++ f a : post))
    | otherwise        = throwBadPlayerID np i
  where
    np = length as
    (pre,a:post) = splitAt (i-1) as

-- | Set the element corresponding to a the given player ID.
setForPlayer :: MonadThrow m => PlayerID -> a -> ByPlayer a -> m (ByPlayer a)
setForPlayer i a = modifyForPlayer i (const a)


-- ** ByTurn lists

-- | A list where each element corresponds to a played turn in a game.
newtype ByTurn a = ByTurn [a]
  deriving (Eq,Show,Foldable,Functor,Traversable)

-- | Return the element corresponding to the given turn number.
forTurn :: MonadThrow m => Int -> ByTurn a -> m a
forTurn i (ByTurn as)
    | i > 0 && i <= n = return (as !! (n-i))
    | otherwise       = throwM (NoSuchElement msg)
  where
    n = length as
    msg = "forTurn: " ++ show i ++ ", number of turns: " ++ show n

-- | Return the elements corresponding to every turn (all elements as
--   a plain list).
everyTurn :: ByTurn a -> [a]
everyTurn (ByTurn as) = as

-- | Return the element corresponding to the first turn of the game.
firstTurn :: MonadThrow m => ByTurn a -> m a
firstTurn (ByTurn []) = throwM (NoSuchElement "firstTurn: empty turn list")
firstTurn (ByTurn as) = return (last as)

-- | Return the element corresponding to the most recently played turn.
lastTurn :: MonadThrow m => ByTurn a -> m a
lastTurn (ByTurn []) = throwM (NoSuchElement "lastTurn: empty turn list")
lastTurn (ByTurn as) = return (head as)

-- | Return the elements corresponding to the most recently played n
--   turns of the game.
lastNTurns :: MonadThrow m => Int -> ByTurn a -> m [a]
lastNTurns n (ByTurn as)
    | length as' == n = return as'
    | otherwise       = throwM (NoSuchElement msg)
  where
    as' = take n as
    msg = "lastNTurns: not enough turns: " ++ show n


-- ** ByGame lists

-- | A list where each element corresponds to one played iteration of
--   an iterated game.
newtype ByGame a = ByGame [a]
  deriving (Eq,Show,Foldable,Functor,Traversable)

-- | Return the element corresponding to the given iteration number.
forGame :: MonadThrow m => Int -> ByGame a -> m a
forGame i (ByGame as)
    | i > 0 && i <= n = return (as !! (n-i))
    | otherwise       = throwM (NoSuchElement msg)
  where
    n = length as
    msg = "forGame: " ++ show i ++ ", number of games: " ++ show n

-- | Add an element corresponding to a new game iteration.
addForNewGame :: a -> ByGame a -> ByGame a
addForNewGame a (ByGame as) = ByGame (a:as)

-- | Return the elements corresponding to every iteration (all elements as a
--   plain list).
everyGame :: ByGame a -> [a]
everyGame (ByGame as) = as

-- | Return the elements corresponding to all completed iterations.
completedGames :: MonadThrow m => ByGame a -> m [a]
completedGames (ByGame []) = throwM (NoSuchElement "completedGames: empty game list")
completedGames (ByGame as) = return (tail as)

-- | Return the element corresponding to the first iteration.
firstGame :: MonadThrow m => ByGame a -> m a
firstGame (ByGame []) = throwM (NoSuchElement "firstGame: empty game list")
firstGame (ByGame as) = return (last as)

-- | Return the element corresponding to the current iteration.
thisGame :: MonadThrow m => ByGame a -> m a
thisGame (ByGame []) = throwM (NoSuchElement "thisGame: empty game list")
thisGame (ByGame as) = return (head as)

-- | Return the element corresponding to the most recently completed iteration.
lastGame :: MonadThrow m => ByGame a -> m a
lastGame (ByGame [])      = throwM (NoSuchElement "lastGame: empty game list")
lastGame (ByGame [_])     = throwM (NoSuchElement "lastGame: no completed games")
lastGame (ByGame (_:a:_)) = return a

-- | Return the elements corresponding to the most recently completed n
--   iterations of the game.
lastNGames :: MonadThrow m => Int -> ByGame a -> m [a]
lastNGames _ (ByGame []) = throwM (NoSuchElement "lastNGames: empty game list")
lastNGames i (ByGame (_:as))
    | length as' == i = return as'
    | otherwise       = throwM (NoSuchElement "lastNGames: not enough games")
  where as' = take i as


--
-- * List utility functions
--

-- | Produce a list of all ordered combinations of elements drawn from each
--   sublist of the argument.
--
--   >>> cross [[1,2],[3,4],[5,6]]
--   [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]] 
-- 
--   >>> cross [[1,2],[2,2,1]]
--   [[1,2],[1,2],[1,1],[2,2],[2,2],[2,1]]
--   
cross :: [[a]] -> [[a]]
cross (xs:xss) = [y:ys | y <- xs, ys <- cross xss]
cross [] = [[]]

-- | Similar to 'cross' but does not preserve the ordering elements in the
--   argument list and returns only unique combinations.
--
--   >>> ucross [[1,2],[2,2,1]]
--   [[1,2],[1,1],[2,2]]
--
ucross :: (Ord a) => [[a]] -> [[a]]
ucross = nub . map sort . cross

-- | Break a list into n-sized chunks.
--
--   >>> chunk 4 [1..9]
--   [[1,2,3,4],[5,6,7,8],[9]]
--
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = take n l : chunk n (drop n l)

-- | Pick an element randomly from a list.
randomlyFrom :: MonadIO m => [a] -> m a
randomlyFrom as = do
    i <- liftIO $ randomRIO (0, length as -1) 
    return (as !! i)

-- | Concatenate a sequence of elements, separated by commas.
showSeq :: [String] -> String
showSeq = intercalate ","
