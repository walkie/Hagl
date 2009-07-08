{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

{-

An implementation of the Match Game, described in the paper.
Try to force your opponent to take the last match.

Example experiments from GHCi:
> execGame matchGame [matchy, randy] (once >> printTranscript)
> execGame matchGame [matchy, randy] (times 1000 >> printScore)
> execGame matchGame [randy, matchy] (times 1000 >> printScore)

-}

module Examples.Matches where

import Control.Monad (liftM)
import Data.List     (find)

import Hagl hiding (last, moves, payoff, turn)

-- Match game:
--   * Number of matches at the beginning of the game.
--   * List of moves (i.e. # of matches a player may take).
-- e.g. Matches 15 [1,2,3] -- 15 matches, can take 1-3 each turn.
data Matches = Matches Int [Int]

instance Game Matches where
  type Move Matches = Int
  type State Matches = Int
  initState (Matches n _) = n
  runGame = takeTurns turn end >>= payoff . last

matches :: GameM m Matches => m Int
matches = gameState

draw = updateGameState . subtract

turn p = decide p >>= draw >> return p
          
end = liftM (<= 0) matches

payoff p = liftM (flip loser p) numPlayers

moves = do n <- matches
           (Matches _ ms) <- game
           return [m | m <- ms, n-m >= 0]

randomly = moves >>= randomlyFrom

-- An example match game
matchGame = Matches 15 [1,2,3]

--
-- Players
--

randy = "Randy" ::: randomly

matchy = "Matchy" ::: do
    n  <- matches
    ms <- moves
    let winning m = mod (n-1) (maximum ms + 1) == m
    maybe randomly play (find winning ms)
