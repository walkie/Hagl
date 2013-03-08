{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

{-|

An implementation of the game Tic Tac Toe.
Try playing against the minimax algorithm by running in GHCi:

>>> playTicTacToe

Valid moves are the integers [0..8], where each integer names square of
the board, starting in the upper left and proceeding in the order that 
you would read a book.  The named square must be empty.

-}
module Hagl.Examples.TicTacToe where

import Control.Monad.Trans (liftIO)
import Data.List           (elemIndices, intersperse, transpose)
import Text.Printf         (printf)

import Hagl

--
-- * Game represetation.
--

-- | Each square either has an X, an O, or is empty.
data Square = X | O | E deriving Eq

-- | A board is a 3x3 grid of squares, represented as a 9-element list.
type Board = [Square]

-- | A move is indicated by a naming the index of the square to put your
--   mark in, [0..8].
type Mark = Int

-- | A trivial data type for Tic Tac Toe.
data TicTacToe = TicTacToe

-- | The initial board.
start :: Board
start = replicate 9 E

-- | The mark corresponding to each player.
xo 1 = X
xo 2 = O

-- | Get a list of empty squares.
markable :: Board -> [Mark]
markable = elemIndices E

-- | The player whose turn it is.
who :: Board -> PlayerID
who b = if (odd . length . markable) b then 1 else 2

-- | Player p marks square m.
mark :: Board -> Mark -> Board
mark b m = take m b ++ xo (who b) : drop (m+1) b

-- | True if player p has won.
win :: PlayerID -> Board -> Bool
win p b = any (all (xo p ==)) (h ++ v ++ d)
  where h = chunk 3 b
        v = transpose h
        d = map (map (b !!)) [[0,4,8],[2,4,6]]

-- | True if the game is over.
end :: Board -> Bool
end b = null (markable b) || win 1 b || win 2 b

-- | The payoff awarded for a final state.
pay :: Board -> Payoff
pay b | win 1 b   = winner 2 1
      | win 2 b   = winner 2 2
      | otherwise = tie 2

-- | Play a game against minimax!
playTicTacToe = evalGame TicTacToe ["Puny Human" ::: human, "Minimax" ::: minimax]
                         (run >> printScore)
  where run = printGame >> step >>= maybe run (\p -> printGame >> return p)

-- Game instance
instance Game TicTacToe where
  type TreeType TicTacToe = Discrete
  type Move  TicTacToe = Mark
  type State TicTacToe = Board
  gameTree _ = stateTreeD who end markable mark pay start


--
-- * Pretty printing
--

instance Show Square where
  show X = "X"
  show O = "O"
  show E = " "

-- | A string representation of the game board.
showGame :: Board -> String
showGame = concat . intersperse "\n" . intersperse line . map row . chunk 3 . map show
  where row [a,b,c] = printf " %s | %s | %s " a b c
        line = "-----------"

-- | Print out the game during game execution.
printGame :: GameM m TicTacToe => m ()
printGame = gameState >>= liftIO . putStrLn . showGame
