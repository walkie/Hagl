module Hagl.Base.Selector where

import Control.Monad (liftM, liftM2)

import Hagl.Base.Accessor
import Hagl.Base.Game
import Hagl.Base.List
import Hagl.Base.Monad


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
his x = check >> liftM2 (forPlayer . nextPlayer 2) myIx x
  where check = numPlayers >>= \np -> if np == 2 then return ()
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
