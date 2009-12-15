{-# LANGUAGE NoMonomorphismRestriction #-}

{-

From GHCi:

> execGame rps [rocky, randy] (times 10 >> printTranscripts >> printScore)
> roundRobin rps [rocky, rotate, randy, pavlov, frequency] (times 100)

-}

module Examples.RPS where

import Prelude hiding (last)

import Hagl
import Hagl.Normal

-------------------------
-- Rock Paper Scissors --
-------------------------

data RPS = Rock | Paper | Scissors deriving (Enum, Eq, Show)

rps = square [Rock .. Scissors] [0,-1, 1,
                                 1, 0,-1,
                                -1, 1, 0]

-- Some simple players
rocky  = "Stalone" ::: pure Rock
rotate = "RPS"     ::: periodic [Rock, Paper, Scissors]
tricky = "Tricky"  ::: [play Rock, play Paper] `thereafter` play Scissors
randy  = "Randy"   ::: randomly

-- If last move resulted in a "big" payoff, do it again, otherwise switch.
pavlov = "Pavlov" :::
    randomly `atFirstThen`
    do p <- my (last game's payoff)
       m <- my (last game's move)
       if p > 0 then return m else randomly

-- Play the move that will beat the move the opponent has played most.
frequency = "Huckleberry" :::
    do ms <- her `each` every games' move
       let r = length $ filter (Rock ==) ms
           p = length $ filter (Paper ==) ms
           s = length $ filter (Scissors ==) ms
           x = maximum [r,p,s]
        in return $ if x == r then Paper else 
                    if x == p then Scissors
                              else Rock
