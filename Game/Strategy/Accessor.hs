module Game.Strategy.Accessor where

import Control.Monad
import Data.List
import Game.Definition
import Game.Execution
import Game.Execution.Util

--------------------
-- Data Accessors --
--------------------

game :: GameMonad m mv => m (Game mv)
game = liftM _game getExecState

players :: GameMonad m mv => m [Player mv]
players = liftM _players getExecState

location :: GameMonad m mv => m (InfoGroup mv)
location = getExecState >>= \s -> return $ info (_game s) (_location s)

transcript :: GameMonad m mv => m (Transcript mv)
transcript = liftM _transcript getExecState

history :: GameMonad m mv => m (History mv)
history = liftM _history getExecState

numGames :: GameMonad m mv => m Int
numGames = liftM (length . asList) history

-- True if this is the first iteration in this execution instance.
isFirstGame :: GameMonad m mv => m Bool
isFirstGame = liftM (null . asList) history

-- Transcript of each game.
transcripts :: GameMonad m mv => m (ByGame (Transcript mv))
transcripts = do t <- transcript
                 h <- history
                 return (ByGame (t : (fst . unzip . asList) h))

-- Summary of each game.
summaries :: GameMonad m mv => m (ByGame (Summary mv))
summaries = do g <- game
               t <- transcript
               h <- history
               return (ByGame (summarize g t : (snd . unzip . asList) h))

-- All moves made by each player in each game.
moves :: GameMonad m mv => m (ByGame (ByPlayer [mv]))
moves = liftM (ByGame . fst . unzip . asList) summaries

-- The last move by each player in each game.
move :: GameMonad m mv => m (ByGame (ByPlayer mv))
move = liftM (ByGame . map (ByPlayer . map head) . asList2) moves

-- The total payoff for each player for each game.
payoff :: GameMonad m mv => m (ByGame (ByPlayer Float))
payoff = liftM (ByGame . snd . unzip . asList) summaries

-- The current score of each player.
score :: GameMonad m mv => m (ByPlayer Float)
score = liftM (ByPlayer . map sum . transpose . asList2) payoff

