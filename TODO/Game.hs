module Hagl.Game where

import Control.Monad.State (liftM, put)

import Hagl.Core
import Hagl.Accessor

--
-- High-level monadic functions for defining games.
--

decide :: Game g => PlayerIx -> ExecM g (Move g)
decide i = do startTurn i
              p <- getPlayer i
              (m, p') <- runStrategy p
              setPlayer i p'
              playerMoved i m
              endTurn
              return m

chance :: Game g => Dist (Move g) -> ExecM g (Move g)
chance d = do m <- fromDist d
              chanceMoved m
              return m

allPlayers :: Game g => (PlayerIx -> ExecM g a) -> ExecM g (ByPlayer a)
allPlayers f = do n <- numPlayers
                  liftM ByPlayer (mapM f [1..n])

takeTurns :: Game g => (PlayerIx -> ExecM g a) -> ExecM g Bool -> ExecM g [a]
takeTurns go until = turn 1
  where turn p = do a <- go p
                    b <- until
                    n <- numPlayers
                    if b then return [a] else liftM (a:) (turn (nextPlayer n p))

-- Like takeTurns, but only returned the last element.
takeTurns_ :: Game g => (PlayerIx -> ExecM g a) -> ExecM g Bool -> ExecM g a
takeTurns_ go until = turn 1
  where turn p = do a <- go p
                    b <- until
                    n <- numPlayers
                    if b then return a else turn (nextPlayer n p)

marginal :: Game g => (Payoff -> Payoff) -> ExecM g Payoff
marginal f = liftM f score

--
-- Lower-level monadic functions.
--

-- Player turns

startTurn :: Game g => PlayerIx -> ExecM g ()
startTurn = setPlayerIx . Just

endTurn :: Game g => ExecM g ()
endTurn = setPlayerIx Nothing

-- Tracking moves

chanceMoved :: Game g => Move g -> ExecM g ()
chanceMoved m = do 
    e <- getExec
    put e { _transcript = (Nothing, m) : _transcript e }

playerMoved :: Game g => PlayerIx -> Move g -> ExecM g ()
playerMoved i m = do 
    e  <- getExec
    ns <- numMoves
    put e { _transcript = (Just i, m) : _transcript e,
            _numMoves = setListElem (i-1) (forPlayer i ns + 1) ns }

--
-- Getters and setters. (Also see Hagl.Accessor for basic getters.)
--

setPlayerIx :: Game g => Maybe PlayerIx -> ExecM g ()
setPlayerIx i = do exec <- getExec
                   put exec { _playerIx = i }

putGameState :: Game g => State g -> ExecM g ()
putGameState s = getExec >>= \e -> put e { _gameState = s }

updateGameState :: Game g => (State g -> State g) -> ExecM g (State g)
updateGameState f = gameState >>= \s -> 
                    let s' = f s in putGameState s' >> return s'

getPlayer :: Game g => PlayerIx -> ExecM g (Player g)
getPlayer i = forPlayerM i players

setPlayer :: Game g => PlayerIx -> Player g -> ExecM g ()
setPlayer i p = do e <- getExec
                   put e { _players = setListElem (i-1) p (_players e) }
