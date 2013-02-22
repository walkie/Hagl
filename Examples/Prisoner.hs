{-# LANGUAGE NoMonomorphismRestriction #-}

{- |

Defines the prisoner's dilemma, stag hunt, and a suite of strategies.

From GHCi, try some of the following.

> nash pd
> pareto pd
> paretoNash pd
> paretoNash stag
> execGame (iterated pd) [tft, pavlov] (times 10 >> printTranscripts >> printScore)
> axelrod [fink, tft, grim', pavlov, preserver]

-}

module Examples.Prisoner where

import Control.Monad.State
import Prelude hiding (last, print)

import Hagl

------------------------
-- Prisoner's Dilemma --
------------------------

-- Game definition
data Cooperation = C | D deriving (Eq, Read, Show)
type Dilemma = Normal Cooperation

pd   = symmetric [C,D] [2,0,3,1]
stag = symmetric [C,D] [3,0,2,1]

-- Some simple players.
fink = "Fink" ::: pure D
mum = "Mum" ::: pure C
alt = "Alternator" ::: periodic [C,D]
dc = "(DC)*" ::: periodic [D,C]
ccd = "(CCD)*" ::: periodic [C,C,D]
randy = "Randy" ::: randomly
rr = "Russian Roulette" ::: mixed [(5,C), (1,D)]

-- The famous Tit-for-Tat.
tft = "Tit for Tat" ::: play C `atFirstThen` his (lastGame's onlyMove)

stately = Player "Stately Alternator" C $
  do m <- get
     put $ if m == C then D else C
     return m

mod3 = Player "Mod3 Cooperator" 0 $
  do i <- get
     put (i+1)
     return $ if i `mod` 3 == 0 then C else D
     
-- Suspicious Tit-for-Tat (like Tit-for-Tat but defect on first move)
suspicious = "Suspicious Tit-for-Tat" ::: play D `atFirstThen` his (lastGame's onlyMove)

-- Tit-for-Tat that only defects after two defects in a row.
titForTwoTats = "Tit-for-Two-Tats" :::
    do ms <- his `each` lastNGames' 2 onlyMove
       return $ if ms == [D, D] then D else C

-- The Grim Trigger: Cs until opponent defects, then defects forever.
grim = "Grim Trigger" :::
    do ms <- his `each` completedGames' onlyMove
       if D `elem` ms then play D else play C

grim' = Player "Stately Grim" False $ 
  play C `atFirstThen`
  do m <- her (lastGame's onlyMove)
     triggered <- update (|| m == D)
     if triggered then play D else play C

-- If last move resulted in a "big" payoff, do it again, otherwise switch.
pavlov = "Pavlov" :::
    randomly `atFirstThen`
    do p <- my (lastGame's payoffs)
       m <- my (lastGame's onlyMove)
       return $ if p > 1 then m else
                if m == C then D else C

-- Made-up strategy: Pick randomlyly until we have a lead, then
-- preserve it by repeatedly choosing D.
preserver = "Preserver" :::
    randomly `atFirstThen`
    do me <- my score
       he <- his score
       if me > he then return D else randomly

-- Run an Axelrod-style tournament.
axelrod :: [Player (Iterated Dilemma)] -> IO ()
axelrod ps = roundRobin (iterated pd) 2 ps (times 200) >>= printResults
