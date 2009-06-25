module Hagl.Selector where

import Control.Monad
import Data.List hiding (last)

import Hagl.Core
import Hagl.Accessor
import Hagl.Game

--------------------
-- List Selectors --
--------------------

-- Apply selection to each element of a list.
eachAnd :: Monad m => (m a -> m b) -> m [a] -> m [b]
eachAnd f xs = mapM (f . return) =<< xs

-- Apply list selectors in reverse order.
inThe = flip ($)

-- ByPlayer Selection --

-- Select the element corresponding to the current player.
my :: GameM m g => m (ByPlayer a) -> m a
my x = myIx >>= forPlayerM x

-- Selects the element corresponding to the other player in a two-player game.
his :: GameM m g => m (ByPlayer a) -> m a
his x = do i  <- myIx
           np <- numPlayers
           if np /= 2 then error "his/her can only be used in two player games"
                      else x `forPlayerM` nextPlayer np i

-- Selects the element corresponding to the other player in a two-player game.
her :: GameM m g => m (ByPlayer a) -> m a
her = his

-- Selects the elements corresponding to all players (i.e. all elements).
our :: GameM m g => m (ByPlayer a) -> m [a]
our = liftM toList

-- Selects the elements corresponding to all players except the current player.
their :: GameM m g => m (ByPlayer a) -> m [a]
their x = do ByPlayer as <- x
             i <- myIx
             return $ (take i as) ++ (drop (i+1) as)

-- ByGame Selection

-- Selects the element corresponding to the current game.
this :: GameM m g => ByGame a -> m (ByGame a) -> m a
this _ x = gameNumber >>= forGameOrTurnM x

-- ByGameOrTurn Selection --

-- Selects the element corresponding to all games or moves.
every :: (ByGameOrTurn d, GameM m g) => d a -> m (d a) -> m [a]
every _ = liftM toList

-- Selects the element corresponding to the first game or move.
first :: (ByGameOrTurn d, GameM m g) => d a -> m (d a) -> m a
first _ = flip forGameOrTurnM 1

-- Selects the element corresponding to the first n games or moves.
firstN :: (ByGameOrTurn d, GameM m g) => Int -> d a -> m (d a) -> m [a]
firstN n _ x = mapM (forGameOrTurnM x) [n, n-1 .. 1]

-- Selects the element corresponding to the last game or move.
last :: (Last d, GameM m g) => d a -> m (d a) -> m a
last d x = lastGameOrTurn d >>= forGameOrTurnM x

-- Selects the element corresponding to the last n games or moves.
lastN :: (Last d, GameM m g) => Int -> d a -> m (d a) -> m [a]
lastN n d x = do m <- lastGameOrTurn d
                 mapM (forGameOrTurnM x) (take n [m, m-1 ..])

--
-- Utility stuff.
--

class ByGameOrTurn d => Last d where
  lastGameOrTurn :: GameM m g => d a -> m Int
instance Last ByGame where
  lastGameOrTurn _ = liftM (subtract 1) gameNumber
instance Last ByTurn where
  lastGameOrTurn _ = liftM (subtract 1) turnNumber
