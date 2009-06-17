{-# LANGUAGE MultiParamTypeClasses #-}

module Hagl.Accessor where

import Control.Monad
import Data.List
import Data.Maybe    (fromMaybe)

import Hagl.Core

--------------------
-- Data Accessors --
--------------------

--
-- Core accessors
--

game :: (Game g, GameM m g) => m g
game = liftM _game getExec

players :: (Game g, GameM m g) => m [Player g]
players = liftM _players getExec

gameState :: (Game g, GameM m g) => m (State g)
gameState = liftM _gameState getExec

playerIx :: (Game g, GameM m g) => m (Maybe PlayerIx)
playerIx = liftM _playerIx getExec

transcript :: (Game g, GameM m g) => m (Transcript g)
transcript = liftM _transcript getExec

history :: (Game g, GameM m g) => m (History g)
history = liftM _history getExec

numMoves :: (Game g, GameM m g) => m (ByPlayer Int)
numMoves = liftM _numMoves getExec

--
-- Other accesssors
--

-- The index of the currently active player.
myIx :: (Game g, GameM m g) => m PlayerIx
myIx = liftM (fromMaybe e) playerIx
  where e = error "Internal error: playerIx not set!"

-- The number of players playing the game.
numPlayers :: (Game g, GameM m g) => m Int
numPlayers = liftM length players

-- The number of completed games.
numGames :: (Game g, GameM m g) => m Int
numGames = liftM (length . toList) history

-- The current game number.
gameNumber :: (Game g, GameM m g) => m Int
gameNumber = liftM (+1) numGames

-- True if this is the first iteration in this execution instance.
isFirstGame :: (Game g, GameM m g) => m Bool
isFirstGame = liftM (> 1) gameNumber

-- Transcript of each game.
transcripts :: (Game g, GameM m g) => m (ByGame (Transcript g))
transcripts = do ts <- liftM _transcripts history
                 t  <- transcript
                 return (t `mcons` ts)
{-
transcripts :: (Game g, GameM m g) => m (ByGame (Transcript g))
transcripts = do h  <- history
                 t' <- transcript
                 return (ByGame (t' : [t | (t,_) <- toList h]))
-}

-- MoveSummary of the current (incomplete) game.
moveSummary :: (Game g, GameM m g) => m (MoveSummary g)
moveSummary = do np <- numPlayers
                 t  <- transcript
                 (return . ByPlayer . map (forp t)) [1..np]
  where forp t i = ByTurn [mv | (mi,mv) <- t, mi == Just i]

-- Summary of the current (incomplete) game.
summary :: (Game g, GameM m g) => m (Summary g)
summary = do ms <- moveSummary 
             return (ms, Nothing)

-- Summary of each game.
summaries :: (Game g, GameM m g) => m (ByGame (Summary g))
summaries = do ss <- liftM _summaries history
               s  <- summary
               return (s `mcons` ss)

-- All moves made by each player in each game.
moves :: (Game g, GameM m g) => m (ByGame (ByPlayer (ByTurn (Move g))))
moves = liftM (fmap _moves) summaries

class (ByX d, ByX e) => MoveList d e where
  move :: (Game g, GameM m g) => m (d (e (Move g)))
instance MoveList ByGame ByPlayer where
  move = do ByGame ms <- moves
            return $ ByGame [fmap (head . toList) m | m <- tail ms]
instance MoveList ByPlayer ByTurn where
  move = liftM _moves summary
            
-- The total payoff for each player for each game.
payoff :: (Game g, GameM m g) => m (ByGame Payoff)
payoff = liftM (fmap _payoff) summaries

-- The current score of each player.
score :: (Game g, GameM m g) => m (ByPlayer Float)
score = liftM (ByPlayer . map sum . transpose . toList2) payoff

