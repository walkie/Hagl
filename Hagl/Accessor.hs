{-# LANGUAGE MultiParamTypeClasses #-}

module Hagl.Accessor where

import Control.Monad (liftM, liftM2)
import Data.List     (transpose)
import Data.Maybe    (fromMaybe)

import Hagl.Core

--------------------
-- Data Accessors --
--------------------

--
-- Core accessors
--

game :: GameM m g => m g
game = liftM _game getExec

players :: GameM m g => m (ByPlayer (Player g))
players = liftM _players getExec

location :: GameM m g => m (GameTree (State g) (Move g))
location = liftM _location getExec

transcript :: GameM m g => m (Transcript (Move g))
transcript = liftM _transcript getExec

numMoves :: GameM m g => m (ByPlayer Int)
numMoves = liftM _numMoves getExec

--
-- Other accesssors
--

playerIx :: GameM m g => m (Maybe PlayerIx)
playerIx = location >>= return . ix
  where ix (GameTree _ (Internal (Decision p) _) = Just p
        ix _                                     = Nothing

-- The index of the currently active player.
myIx :: GameM m g => m PlayerIx
myIx = liftM (fromMaybe e) playerIx
  where e = error "Internal error: playerIx not set!"

-- The currently active player.
me :: GameM m g => m (Player g)
me = liftM2 forPlayer myIx players

-- The number of players playing the game.
numPlayers :: GameM m g => m Int
numPlayers = liftM dlength players

-- The number of completed games.
numGames :: GameM m g => m Int
numGames = liftM dlength history

-- The current game number.
gameNumber :: GameM m g => m Int
gameNumber = liftM (+1) numGames

-- True if this is the first iteration in this execution instance.
isFirstGame :: GameM m g => m Bool
isFirstGame = liftM (>1) gameNumber

-- The turn number of the current game, for the current player.
turnNumber :: GameM m g => m Int
turnNumber = liftM2 (forPlayer . (+1)) myIx numMoves

-- Transcript of each game.
transcripts :: GameM m g => m (ByGame (Transcript (Move g)))
transcripts = do ts <- liftM _transcripts history
                 t  <- transcript
                 return (t `dcons` ts)

-- MoveSummary of the current (incomplete) game.
moveSummary :: GameM m g => m (MoveSummary (Move g))
moveSummary = do np <- numPlayers
                 t  <- transcript
                 (return . ByPlayer . map (forp t)) [1..np]
  where forp t i = ByTurn [mv | (mi,mv) <- t, mi == Just i]

-- Summary of the current (incomplete) game.
summary :: GameM m g => m (Summary (Move g))
summary = do ms <- moveSummary 
             return (ms, Nothing)

-- Summary of each game.
summaries :: GameM m g => m (ByGame (Summary (Move g)))
summaries = do ss <- liftM _summaries history
               s  <- summary
               return (s `dcons` ss)

-- All moves made by each player in each game.
moves :: GameM m g => m (ByGame (MoveSummary (Move g)))
moves = liftM (fmap _moves) summaries

class (ByX d, ByX e) => MoveList d e where
  move :: GameM m g => m (d (e (Move g)))
instance MoveList ByGame ByPlayer where
  move = do ByGame ms <- moves
            return $ ByGame [fmap (head . toList) m | m <- tail ms]
instance MoveList ByPlayer ByTurn where
  move = liftM _moves summary

-- The total payoff for each player for each game.
payoff :: GameM m g => m (ByGame Payoff)
payoff = liftM (fmap _payoff) summaries

-- The current score of each player.
score :: GameM m g => m (ByPlayer Float)
score = do ByGame ps <- payoff
           np <- numPlayers
           (return . ByPlayer) (if null ps then replicate np 0.0
                                else (map sum . transpose . map toList . tail) ps)
