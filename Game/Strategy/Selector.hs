module Game.Strategy.Selector where

import Control.Monad
import Data.List hiding (last)
import Game.Definition
import Game.Execution
import Game.Lists
import Game.Strategy.Accessor

--------------------
-- List Selectors --
--------------------

-- Apply selection to each element of a list.
each :: Monad m => (m a -> m b) -> m [a] -> m [b]
each f xs = mapM (f . return) =<< xs

-- Apply list selectors in reverse order.
inThe = flip ($)

-- ByPlayer Selection --

-- The index of the current player.
myIx :: GameMonad m mv => m PlayerIx
myIx = do Decision p _ <- _exactLoc
          return p

my :: GameMonad m mv => m (ByPlayer a) -> m a
my x = myIx >>= forPlayerM x

-- Selects the next player's x.
his :: GameMonad m mv => m (ByPlayer a) -> m a
his x = myIx >>= forPlayerM x . next
  where next 1 = 2
        next 2 = 1
        next _ = error "his/her can only be used in two player games"

her :: GameMonad m mv => m (ByPlayer a) -> m a
her = his

our :: GameMonad m mv => m (ByPlayer a) -> m [a]
our = liftM toList

their :: GameMonad m mv => m (ByPlayer a) -> m [a]
their x = do ByPlayer as <- x
             i <- myIx
             return $ (take i as) ++ (drop (i+1) as)

-- ByGame Selection

this :: GameMonad m mv => ByGame a -> m (ByGame a) -> m a
this _ x = gameNumber >>= forGameOrTurnM x

-- ByGameOrTurn Selection --

every :: (ByGameOrTurn d, GameMonad m mv) => d a -> m (d a) -> m [a]
every _ = liftM toList

first :: (ByGameOrTurn d, GameMonad m mv) => d a -> m (d a) -> m a
first _ = flip forGameOrTurnM 1

firstn :: (ByGameOrTurn d, GameMonad m mv) => Int -> d a -> m (d a) -> m [a]
firstn n _ x = mapM (forGameOrTurnM x) [n, n-1 .. 1]

last :: (Last d, GameMonad m mv) => d a -> m (d a) -> m a
last d x = lastGameOrTurn d >>= forGameOrTurnM x

lastn :: (Last d, GameMonad m mv) => Int -> d a -> m (d a) -> m [a]
lastn n d x = do m <- lastGameOrTurn d
                 mapM (forGameOrTurnM x) (take n [m, m-1 ..])


--
-- These are actually accessors, but are here because they rely on selectors.
--
numMine :: GameMonad m mv => m (ByPlayer (ByTurn a)) -> m Int
numMine = liftM (length . toList) . my

turnNumber :: GameMonad m mv => m Int
turnNumber = liftM (1+) (numMine (this game's moves))

totalMoves :: GameMonad m mv => m Int
totalMoves = do ms <- every game's moves
                ns <- sequence [numMine (return m) | m <- ms]
                return (sum ns)

class ByGameOrTurn d => Last d where
  lastGameOrTurn :: GameMonad m mv => d a -> m Int
instance Last ByGame where
  lastGameOrTurn _ = liftM (subtract 1) gameNumber
instance Last ByTurn where
  lastGameOrTurn _ = liftM (subtract 1) turnNumber

-----------------------
-- Utility Functions --
-----------------------

_exactLoc :: GameMonad m mv => m (GameTree mv)
_exactLoc = liftM _location getExecState
