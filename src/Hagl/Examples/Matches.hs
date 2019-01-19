{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

{- |

An implementation of the Match Game, described in the paper.
Try to force your opponent to take the last match.

Example experiments from GHCi:

>>> execGame matchGame [matchy, randy] (once >> printTranscript)
>>> execGame matchGame [matchy, randy] (times 1000 >> printScore)
>>> execGame matchGame [randy, matchy] (times 1000 >> printScore)

-}

module Hagl.Examples.Matches where

import Data.List (find)


import Hagl

--
-- * Game representation
--

-- | Match game.  Arguments are:
--
--     1. Number of players.
--
--     2. Number of matches at the beginning of the game.
--
--     3. List of moves--the number of matches a player may take.
--
--   For example: @Matches 2 15 [1,2,3]@ -- 2 players, 15 matches, can take 1-3 each turn.
data Matches = Matches Int Int [Int]

instance Game Matches where
  
  type TreeType Matches = Discrete
  type Move  Matches = Int
  type State Matches = Int

  gameTree (Matches np init ms) = takeTurnsD np end moves up pay 1 init
    where
      end   _ n   = n <= 0
      moves _ n   = [m | m <- ms, n-m >= 0]
      up    _ n m = n-m
      pay   p _   = loser np p

-- | An example match game
matchGame = Matches 2 15 [1,2,3]


--
-- * Players
--

-- | A player that plays randomly.
randy :: DiscreteGame g => Player g
randy = "Randy" ::: randomly

-- | A player that plays optimally.  Guaranteed to win if given the first move.
--   (Only works with move sets of the form [1..n]!)
matchy :: Player Matches
matchy = "Matchy" ::: do
    n  <- gameState
    ms <- availMoves
    let winning m = mod n (maximum ms + 1) == m
    maybe randomly play (find winning ms)
