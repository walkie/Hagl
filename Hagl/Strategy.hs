{-# LANGUAGE FlexibleContexts #-}

-- | This module provides impelemntations of some common strategies and
--   data selectors for writing custom strategies in a nice way.
module Hagl.Strategy where

import Control.Exception   (catch)
import Control.Monad.Trans (liftIO)
import Control.Monad       (liftM, liftM2)
import System.IO.Error     (isUserError)

import Prelude hiding (catch)

import Hagl.Lists
import Hagl.Game
import Hagl.Exec

--
-- * Some common strategies
--

-- | A pure strategy. Always plays the same move.
pure :: Move g -> Strategy s g
pure = return

-- | A mixed strategy. Plays moves based on a distribution.
mixed :: Dist (Move g) -> Strategy s g
mixed = fromDist

-- | Perform some pattern of moves periodically.
periodic :: Game g => [Move g] -> Strategy s g
periodic ms = my numMoves >>= \n -> return $ ms !! mod n (length ms)

-- | Select a move randomly.
randomly :: DiscreteGame g => Strategy s g
randomly = availMoves >>= randomlyFrom

-- | A human player, who enters moves on the console.
human :: (Game g, Read (Move g)) => Strategy () g
human = me >>= liftIO . getMove . name
  where getMove n = putStr (n ++ "'s move: ") >> catch readLn (retry n)
        retry n e | isUserError e = putStrLn "Not a valid move... try again." >> getMove n
                  | otherwise     = ioError e


--
-- * Strategy components
--

-- | Play a move.
play :: Move g -> Strategy s g
play = return

-- | Play a list of initial strategies, then a primary strategy thereafter.
thereafter :: Game g => [Strategy s g] -> Strategy s g -> Strategy s g
thereafter ss s = my numMoves >>= \n -> if n < length ss then ss !! n else s

-- | Play an initial strategy for the first move, then a primary strategy thereafter.
atFirstThen :: Game g => Strategy s g -> Strategy s g -> Strategy s g
atFirstThen s = thereafter [s]


--
-- * Selector combinators
--

-- | Apply selector to each element of a list.
each :: GameM m g => (m a -> m b) -> m [a] -> m [b]
each f = (>>= mapM (f . return))

-- | Apply selectors in reverse order.
inThe :: GameM m g => m a -> (m a -> m b) -> m b
inThe = flip ($)


--
-- * Selectors
--

-- ** ByPlayer selection
--

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
our = liftM everyPlayer

-- | Selects the elements corresponding to all players except the current player.
their :: GameM m g => m (ByPlayer a) -> m [a]
their x = do ByPlayer as <- x
             i <- myIx
             return (take i as ++ drop (i+1) as)


-- ** ByTurn selection
--

-- | Selects the elements corresponding to all turns (i.e. all elements).
everyTurn's :: GameM m g => m (ByTurn a) -> m [a]
everyTurn's = liftM everyTurn

-- | Selects the elements corresponding to the first turn of the game.
firstTurn's :: GameM m g => m (ByTurn a) -> m a
firstTurn's = liftM firstTurn

-- | Selects the element corresponding to the most recently played term.
lastTurn's :: GameM m g => m (ByTurn a) -> m a
lastTurn's = liftM lastTurn

-- | Selects the elements corresponding to the last `n` turns of the game.
lastNTurns' :: GameM m g => Int -> m (ByTurn a) -> m [a]
lastNTurns' i = liftM (lastNTurns i)
