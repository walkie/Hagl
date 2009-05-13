{-# OPTIONS_GHC -fglasgow-exts #-}

module Game.Strategy.Accessor where

import Control.Monad
import Data.List
import Game.Definition
import Game.Execution
import Game.Execution.Util
import Game.Lists

--------------------
-- Data Accessors --
--------------------

game :: GameMonad m mv => m (Game mv)
game = liftM _game getExecState

players :: GameMonad m mv => m [Player mv]
players = liftM _players getExecState

numMoves :: GameMonad m mv => m (ByPlayer Int)
numMoves = liftM _numMoves getExecState

location :: GameMonad m mv => m (InfoGroup mv)
location = getExecState >>= \s -> return $ info (_game s) (_location s)

transcript :: GameMonad m mv => m (Transcript mv)
transcript = liftM _transcript getExecState

history :: GameMonad m mv => m (History mv)
history = liftM _history getExecState

finished :: GameMonad m mv => m Int
finished = liftM (length . toList) history

gameNumber :: GameMonad m mv => m Int
gameNumber = liftM (+1) finished

-- True if this is the first iteration in this execution instance.
isFirstGame :: GameMonad m mv => m Bool
isFirstGame = liftM (null . toList) history

transcripts :: GameMonad m mv => m (ByGame (Transcript mv))
transcripts = do ts <- liftM _transcripts history
                 t  <- transcript
                 return (t `mcons` ts)

summary :: GameMonad m mv => m (Summary mv)
summary = liftM2 summarize game transcript

summaries :: GameMonad m mv => m (ByGame (Summary mv))
summaries = do ss <- liftM _summaries history
               s  <- summary
               return (s `mcons` ss)

-- All moves made by each player in each game.
moves :: GameMonad m mv => m (ByGame (ByPlayer (ByTurn mv)))
moves = liftM (fmap _moves) summaries

class (DList d, DList e) => MoveList d e where
  move :: GameMonad m mv => m (d (e mv))
instance MoveList ByGame ByPlayer where
  move = do ByGame ms <- moves
            return $ ByGame [fmap (head . toList) m | m <- tail ms]
instance MoveList ByPlayer ByTurn where
  move = liftM _moves summary
            
-- The total payoff for each player for each game.
payoff :: GameMonad m mv => m (ByGame (ByPlayer Float))
payoff = liftM (fmap _payoff) summaries

-- The current score of each player.
score :: GameMonad m mv => m (ByPlayer Float)
score = liftM (ByPlayer . map sum . transpose . toList2) payoff

