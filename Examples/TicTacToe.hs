{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Examples.TicTacToe where

import Control.Monad.Trans (liftIO)
import Data.List           (elemIndices, intersperse, transpose)
import Text.Printf         (printf)

import Hagl
import Hagl.Searchable

-----------------
-- Tic Tac Toe --
-----------------

data Square = X | O | E deriving Eq
type Board = [Square]
type Mark  = Int

data TicTacToe = TicTacToe

-- The initial board.
start :: Board
start = replicate 9 E

-- The mark corresponding to each player.
xo 1 = X
xo 2 = O

-- Get a list of empty squares.
markable :: Board -> [Mark]
markable = elemIndices E

-- The player whose turn it is.
who :: Board -> PlayerIx
who b = if (odd . length . markable) b then 1 else 2

-- Player p marks square m.
mark :: PlayerIx -> Mark -> Board -> Board
mark p m b = take m b ++ xo p : drop (m+1) b

-- True if player p has won.
win :: PlayerIx -> Board -> Bool
win p b = any (all (xo p ==)) (h ++ v ++ d)
  where h = chunk 3 b
        v = transpose h
        d = map (map (b !!)) [[0,4,8],[2,4,6]]

-- True if the game is over.
end :: Board -> Bool
end b = null (markable b) || win 1 b || win 2 b

-- The payoff awarded for a final state.
pay :: Board -> Payoff
pay b | win 1 b   = winner 2 1
      | win 2 b   = winner 2 2
      | otherwise = tie 2

-- Play a game against minimax!
challenge = execGame TicTacToe ["Puny Human" ::: human, "Minimax" ::: minimax]
    (run >> printScore)
  where run = printGame >> step >>= maybe run (\p -> printGame >> conclude p)

--
-- Game instances
--

instance Game TicTacToe where
  type Move TicTacToe = Mark
  type State TicTacToe = Board
  initState _ = start
  runGame     = runTree
  
instance Searchable TicTacToe where
  gameTree g = stateGameTree g who end markable (nextState g) pay
  nextState _ b m = mark (who b) m b

--
-- Pretty printing
--

instance Show Square where
  show X = "X"
  show O = "O"
  show E = " "

showGame :: Board -> String
showGame = concat . intersperse "\n" . intersperse line . map row . chunk 3 . map show
  where row [a,b,c] = printf " %s | %s | %s " a b c
        line = "-----------"

printGame :: GameM m TicTacToe => m ()
printGame = gameState >>= liftIO . putStrLn . showGame
