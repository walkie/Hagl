module Game.Strategy.Selector where

import Control.Monad
import Data.List
import Game.Definition
import Game.Execution
import Game.Strategy.Accessor

--------------------
-- List Selectors --
--------------------

-- Apply selection to each element of a list.
each :: Monad m => (m a -> m b) -> m [a] -> m [b]
each f xs = (sequence . map f . map return) =<< xs

-- ByPlayer Selection --

-- The index of the current player.
myIx :: GameMonad m mv => m PlayerIx
myIx = do Decision p _ <- _exactLoc
          return (p-1)

my :: GameMonad m mv => m (ByPlayer a) -> m a
my x = liftM2 (!!) (liftM asList x) myIx

-- Selects the next player's x.
his :: GameMonad m mv => m (ByPlayer a) -> m a
his x = do ByPlayer as <- x
           i <- myIx
           g <- game
           return $ as !! ((i+1) `mod` numPlayers g)

her :: GameMonad m mv => m (ByPlayer a) -> m a
her = his

our :: GameMonad m mv => m (ByPlayer a) -> m [a]
our = liftM asList

their :: GameMonad m mv => m (ByPlayer a) -> m [a]
their x = do ByPlayer as <- x
             i <- myIx
             return $ (take i as) ++ (drop (i+1) as)

playern :: GameMonad m mv => PlayerIx -> m (ByPlayer a) -> m a
playern i x = do ByPlayer as <- x
                 return $ as !! (i-1)

-- ByGame Selection --

every :: GameMonad m mv => m (ByGame a) -> m [a]
every = liftM asList

first :: GameMonad m mv => m (ByGame a) -> m a
first = liftM (last . asList)

firstn :: GameMonad m mv => Int -> m (ByGame a) -> m [a]
firstn n = liftM (reverse . take n . reverse . asList)

prev :: GameMonad m mv => m (ByGame a) -> m a
prev = liftM (head . asList)

prevn :: GameMonad m mv => Int -> m (ByGame a) -> m [a]
prevn n = liftM (take n . asList)

gamen :: GameMonad m mv => Int -> m (ByGame a) -> m a
gamen i x = do ByGame as <- x
               n <- numGames
               return $ as !! (n-i)

-----------------------
-- Utility Functions --
-----------------------

_exactLoc :: GameMonad m mv => m (GameTree mv)
_exactLoc = liftM _location getExecState
