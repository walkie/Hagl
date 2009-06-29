{-# LANGUAGE TypeFamilies #-}
module Examples.TicTacToe where

import Control.Monad (liftM)
import Data.List     (elemIndices, transpose)

import Hagl
import Hagl.Searchable

-----------------
-- Tic Tac Toe --
-----------------

data Square = X | O | E deriving (Eq, Show)
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

--
-- Game instance.
--

instance Game TicTacToe where
  type Move TicTacToe = Mark
  type State TicTacToe = Board
  initState _ = start
  runGame     = runTicTacToe
  
runTicTacToe :: ExecM TicTacToe Payoff
runTicTacToe = takeTurns turn (liftM end gameState)
  where turn p = do m <- decide p
                    b <- updateGameState (mark p m)
                    return (pay b)

-- 
-- Searchable instance.
-- TODO: Add library functions to make deriving trees easy, 
--       given helper funcions similar to above.
-- 

instance Searchable TicTacToe where
  gameTree  _ b   = undefined
  nextState _ b m = undefined
