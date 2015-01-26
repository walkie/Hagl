{-# LANGUAGE NoMonomorphismRestriction #-}

{- |

An example zero-sum game: Rock-paper-scissors.

From GHCi, try some of the following.

>>> execGame rps [rocky, randy] (times 10 >> printTranscripts >> printScore)
>>> roundRobin rps 2 [rocky, rotate, randy, rpsPavlov, frequency] (times 100) >>= printResults

-}

module Hagl.Examples.RPS where

import Prelude hiding (last)

import Hagl

--
-- * Game representation
--

-- | Moves in rock-paper-scissors.
data RPS = Rock | Paper | Scissors deriving (Enum, Eq, Show)

-- | Rock-papers-scissors game.
rps :: Matrix RPS
rps = square [Rock .. Scissors] [0,-1, 1,
                                 1, 0,-1,
                                -1, 1, 0]


--
-- * Players
--

-- | Good ol' rock, nothing beats that.
rocky  = "Stalone" ::: pureStrat Rock

-- | Rotates through all three options in a cycle.
rotate = "RPS" ::: periodic [Rock, Paper, Scissors]

-- | Plays rock, then paper, the scissors forever after.
tricky = "Tricky" ::: [play Rock, play Paper] `thereafter` play Scissors

-- | Plays randomly
randy  = "Randy" ::: randomly

-- | If last move resulted in a "big" payoff, do it again, otherwise play randomly
rpsPavlov = "Pavlov" :::
    randomly `atFirstThen`
    do p <- my (lastGame's payoffs)
       m <- my (lastGame's onlyMove)
       if p > 0 then return m else randomly

-- | Play the move that will beat the move the opponent has played most.
frequency = "Huckleberry" :::
    randomly `atFirstThen`
    do ms <- her `each` completedGames' onlyMove
       let r = length $ filter (Rock ==) ms
           p = length $ filter (Paper ==) ms
           s = length $ filter (Scissors ==) ms
           x = maximum [r,p,s]
        in return $ if x == r then Paper else
                    if x == p then Scissors
                              else Rock
