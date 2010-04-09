module Hagl.Base.Selector where

import Control.Monad (liftM, liftM2)

import Hagl.Base.Accessor
import Hagl.Base.Exec
import Hagl.Base.List


-----------------
-- Combinators --
-----------------

-- | Apply selector to each element of a list.
each :: GameM m g => (m a -> m b) -> m [a] -> m [b]
each f = (>>= mapM (f . return))

-- | Apply selectors in reverse order.
inThe :: GameM m g => m a -> (m a -> m b) -> m b
inThe = flip ($)


------------------------
-- ByPlayer Selection --
------------------------

-- | Select the element corresponding to the current player.
my :: GameM m g => m (ByPlayer a) -> m a
my = liftM2 forPlayer myIx

-- | Selects the element corresponding to the other player in a two-player game.
his :: GameM m g => m (ByPlayer a) -> m a
his x = check >> liftM2 (forPlayer . next) myIx x 
  where next 1 = 2
        next 2 = 1
        check = numPlayers >>= \np -> if np == 2 then return ()
                else fail "his/her can only be used in two player games."
                              
-- | Selects the element corresponding to the other player in a two-player game.
her :: GameM m g => m (ByPlayer a) -> m a
her = his

-- | Selects the elements corresponding to all players (i.e. all elements).
our :: GameM m g => m (ByPlayer a) -> m [a]
our = liftM toList

-- | Selects the elements corresponding to all players except the current player.
their :: GameM m g => m (ByPlayer a) -> m [a]
their x = do ByPlayer as <- x
             i <- myIx
             return (take i as ++ drop (i+1) as)


----------------------
-- ByTurn Selection --
----------------------

everyTurn's :: GameM m g => m (ByTurn a) -> m [a]
everyTurn's = liftM toList

firstTurn's :: GameM m g => m (ByTurn a) -> m a
firstTurn's = liftM firstTurn

lastTurn's :: GameM m g => m (ByTurn a) -> m a
lastTurn's = liftM lastTurn

lastTurns' :: GameM m g => Int -> m (ByTurn a) -> m [a]
lastTurns' i = liftM (lastNTurns i)

{-
-- ByGame Selection

-- Selects the element corresponding to the current game.
this :: GameM m g => ByGame a -> m (ByGame a) -> m a
this _ = liftM2 forGameOrTurn gameNumber

-- Selects the elements corresponding to completed games.
completed :: GameM m g => ByGame a -> m (ByGame a) -> m [a]
completed _ x = do ByGame as <- x
                   return (if null as then [] else tail as)

-- ByGameOrTurn Selection --

-- Selects the element corresponding to all games or moves.
every :: (ByGameOrTurn d, GameM m g) => d a -> m (d a) -> m [a]
every _ = liftM toList

-- Selects the element corresponding to the first game or move.
first :: (ByGameOrTurn d, GameM m g) => d a -> m (d a) -> m a
first _ = forGameOrTurnM 1

-- Selects the element corresponding to the first n games or moves.
firstN :: (ByGameOrTurn d, GameM m g) => Int -> d a -> m (d a) -> m [a]
firstN n _ x = sequence [forGameOrTurnM i x | i <- [n, n-1 .. 1]]

-- Selects the element corresponding to the last game or move.
last :: (Last d, GameM m g) => d a -> m (d a) -> m a
last d = liftM2 forGameOrTurn (lastGameOrTurn d)

-- Selects the element corresponding to the last n games or moves.
lastN :: (Last d, GameM m g) => Int -> d a -> m (d a) -> m [a]
lastN n d x = do m <- lastGameOrTurn d
                 sequence [forGameOrTurnM i x | i <- take n [m, m-1 ..]]

--
-- Utility stuff.
--

class ByGameOrTurn d => Last d where
  lastGameOrTurn :: GameM m g => d a -> m Int
instance Last ByGame where
  lastGameOrTurn _ = liftM (subtract 1) gameNumber
instance Last ByTurn where
  lastGameOrTurn _ = liftM (subtract 1) turnNumber
-}
