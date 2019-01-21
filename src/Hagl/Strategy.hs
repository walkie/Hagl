{-# LANGUAGE FlexibleContexts #-}

-- | This module provides impelemntations of some common strategies and
--   data selectors for writing custom strategies in a nice way.
--   Also essential for writing custom strategies are the execution state
--   accessor functions in "Hagl.Exec" and "Hagl.Iterated".
module Hagl.Strategy where

import Control.Exception   (catch)
import Control.Monad.Trans (liftIO)
import Control.Monad       (liftM, liftM2, unless)
import Data.Function       (on)
import Data.List           (maximumBy)
import System.IO.Error     (isUserError)

import Prelude hiding (catch)

import Hagl.List
import Hagl.Game
import Hagl.Exec


--
-- * Some common strategies
--

-- | A pure strategy. Always plays the same move. This is defined to @pure@
--   from the Applicative class, which can be used equivalently and reads
--   a bit better.
pureStrategy :: Move g -> Strategy s g
pureStrategy = pure

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

-- | Minimax strategy.  Computes the best move for the current player. 
--   Conceptually explores the entire game tree, but implements alpha-beta
--   pruning for efficiency.  Assumes games without chance.
minimax :: DiscreteGame g => Strategy s g
minimax = liftM minimaxAlg location

-- | Minimax algorithm.  Computes the best move for the player whose decision
--   is at the root of the game tree.  Conceptually explores the entire game
--   tree, but implements alpha-beta pruning for efficiency.  Assumes games
--   without chance.
minimaxAlg :: Discrete s mv -> mv
minimaxAlg (Discrete (_,Decision me) es) =
    fst $ maximumBy (compare `on` snd) [(m, val me (-inf) inf t) | (m,t) <- es]
  where inf  = 1/0 :: Float
        val me _ _ (Discrete (_,Payoff vs) _) = forPlayer me vs
        val me a b (Discrete (_,Decision p) es) | a >= b    = ifMe a b
                                                | otherwise = ifMe a' b'
          where
            ifMe :: a -> a -> a
            ifMe a b   = if me == p then a else b
            mm (a,b) t = let v = val me a b t
                         in ifMe (max a v, b) (a, min b v)
            (a',b')    = foldl mm (a,b) (map snd es)
minimaxAlg _ = error "minimaxAlg: root of game tree is not a decision!"


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
my = liftM2 forPlayer myPlayerID

-- | Selects the element corresponding to the other player in a two-player game.
his :: GameM m g => m (ByPlayer a) -> m a
his x = check >> liftM2 (forPlayer . nextPlayer 2) myPlayerID x
  where check = numPlaying >>= \np -> unless (np == 2) $
                fail "his/her can only be used in two player games."
                              
-- | Selects the element corresponding to the other player in a two-player game.
her :: GameM m g => m (ByPlayer a) -> m a
her = his

-- | Selects the elements corresponding to all players (i.e. all elements).
our :: GameM m g => m (ByPlayer a) -> m [a]
our = liftM everyPlayer

-- | Selects the elements corresponding to all players except the current player.
their :: GameM m g => m (ByPlayer a) -> m [a]
their x = do ByPlayer as <- x
             i <- myPlayerID
             return (take i as ++ drop (i+1) as)


-- ** ByTurn selection
--

-- | Selects the elements corresponding to all turns (i.e. all elements).
everyTurn's :: GameM m g => m (ByTurn a) -> m [a]
everyTurn's = liftM everyTurn

-- | Selects the element corresponding to the first turn of the game.
firstTurn's :: GameM m g => m (ByTurn a) -> m a
firstTurn's = liftM firstTurn

-- | Selects the element corresponding to the most recently played term.
lastTurn's :: GameM m g => m (ByTurn a) -> m a
lastTurn's = liftM lastTurn

-- | Selects the elements corresponding to the last `n` turns of the game.
lastNTurns' :: GameM m g => Int -> m (ByTurn a) -> m [a]
lastNTurns' i = liftM (lastNTurns i)


-- ** ByGame selection
--

-- | Selects the elements corresponding to all iterations (i.e. all elements).
everyGames' :: GameM m g => m (ByGame a) -> m [a]
everyGames' = liftM everyGame

-- | Selects the elements correspondings to all completed iterations.
completedGames' :: GameM m g => m (ByGame a) -> m [a]
completedGames' = liftM completedGames

-- | Selects the element corresponding to the first iteration.
firstGame's :: GameM m g => m (ByGame a) -> m a
firstGame's = liftM firstGame

-- | Selects the element corresponding to the current iteration.
thisGame's :: GameM m g => m (ByGame a) -> m a
thisGame's = liftM thisGame

-- | Selects the element corresponding to the most recently completed iteration.
lastGame's :: GameM m g => m (ByGame a) -> m a
lastGame's = liftM lastGame

-- | Selects the elements corresponding to the last `n` completed iterations of the game.
lastNGames' :: GameM m g => Int -> m (ByGame a) -> m [a]
lastNGames' i = liftM (lastNGames i)
