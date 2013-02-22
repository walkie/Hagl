{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

{- |

Defines the prisoner's dilemma, stag hunt, and a suite of strategies.

From GHCi, try some of the following.

> nash pd
> pareto pd
> paretoNash pd
> paretoNash stag
> execGame ipd [tft, pavlov] (times 10 >> printTranscripts >> printScore)
> axelrod [fink, tft, grim', pavlov, preserver]

-}

module Examples.Prisoner where

import Control.Monad.State
import Prelude hiding (last, print)

import Hagl

--
-- * Game representations
--

-- | A move indicating whether to cooperate or not.
data Cooperation = C -- ^ Cooperate
                 | D -- ^ Defect
  deriving (Eq, Read, Show)

-- | A dilemma game is a normal form game about cooperating or defecting.
type Dilemma = Normal Cooperation

-- | The classic prisoner's dilemma.
pd :: Dilemma
pd = symmetric [C,D] [2,0,3,1]

-- | Iterated prisoner's dilemma.
ipd :: Iterated Dilemma
ipd = iterated pd

-- | The stag hunt. Similar to the prisoner's dilemma except mutual
--   cooperation is both a Nash equilibrium and Pareto optimal, and is
--   therefore stable.
stag :: Dilemma
stag = symmetric [C,D] [3,0,2,1]

-- | Iterated stag hunt.
istag :: Iterated Dilemma
istag = iterated stag

-- | Captures both iterated and un-iterated dilemma games.
--   (Mostly just to improve type annotations.)
class (Game g, Move g ~ Cooperation) => DilemmaGame g
instance DilemmaGame Dilemma
instance DilemmaGame (Iterated Dilemma)


--
-- * Players
--

--
-- ** Simple strategies

-- | Always defects.
fink :: DilemmaGame g => Player g
fink = "Fink" ::: pure D

-- | Always cooperates.
mum :: DilemmaGame g => Player g
mum = "Mum" ::: pure C

-- | Alternates between cooperation and defection.
alt :: DilemmaGame g => Player g
alt = "Alternator" ::: periodic [C,D]

-- | Alternates between defection and cooperation.
dc :: DilemmaGame g => Player g
dc = "(DC)*" ::: periodic [D,C]

-- | Defects every third round.
ccd :: DilemmaGame g => Player g
ccd = "(CCD)*" ::: periodic [C,C,D]

-- | Defects randomly with a probability of 1/6.
rr :: DilemmaGame g => Player g
rr = "Russian Roulette" ::: mixed [(5,C), (1,D)]

-- | Plays randomly, cooperates or defects with equal probability.
randy :: DiscreteGame g => Player g
randy = "Randy" ::: randomly


--
-- ** More sophisticated/complex strategies

-- | The famous Tit-for-Tat. Cooperates initially, then plays the last
--   move played by its opponent.
tft :: Player (Iterated Dilemma)
tft = "Tit for Tat" ::: play C `atFirstThen` his (lastGame's onlyMove)

-- | The same strategy as 'alt', implemented using state.
alt' :: DilemmaGame g => Player g
alt' = Player "Stately Alternator" C $
  do m <- get
     put $ if m == C then D else C
     return m

-- | Cooperates every third round, implemented using state.
mod3 :: DilemmaGame g => Player g
mod3 = Player "Mod3 Cooperator" 0 $
  do i <- get
     put (i+1)
     return $ if i `mod` 3 == 0 then C else D
     
-- | Suspicious Tit-for-Tat.  Like Tit-for-Tat but defect on first move.
suspicious :: Player (Iterated Dilemma)
suspicious = "Suspicious Tit-for-Tat" ::: play D `atFirstThen` his (lastGame's onlyMove)

-- | A variant of Tit-for-Tat that only defects after two defects in a row.
titForTwoTats :: Player (Iterated Dilemma)
titForTwoTats = "Tit-for-Two-Tats" :::
    do ms <- his `each` lastNGames' 2 onlyMove
       return $ if ms == [D, D] then D else C

-- | The Grim Trigger.  Cooperates until the opponent defects, then defects
--   forever.
grim :: Player (Iterated Dilemma)
grim = "Grim Trigger" :::
    do ms <- his `each` completedGames' onlyMove
       if D `elem` ms then play D else play C

-- | The Grim Trigger, implemented using state.  Much faster than 'grim', since
--   it doesn't examine every previous game iteration.
grim' :: Player (Iterated Dilemma)
grim' = Player "Stately Grim" False $ 
  play C `atFirstThen`
  do m <- her (lastGame's onlyMove)
     triggered <- update (|| m == D)
     if triggered then play D else play C

-- | If last move resulted in a "big" payoff, do it again, otherwise switch.
pavlov :: Player (Iterated Dilemma)
pavlov = "Pavlov" :::
    randomly `atFirstThen`
    do p <- my (lastGame's payoffs)
       m <- my (lastGame's onlyMove)
       return $ if p > 1 then m else
                if m == C then D else C

-- | Picks randomly until it has a lead, then preserves it by repeatedly
--   defecting.
preserver :: Player (Iterated Dilemma)
preserver = "Preserver" :::
    randomly `atFirstThen`
    do me <- my score
       he <- his score
       if me > he then return D else randomly


--
-- * Experiments
--

-- | Run a tournament similar to Robert Axelrod's famous study.
axelrod :: [Player (Iterated Dilemma)] -> IO ()
axelrod ps = roundRobin ipd 2 ps (times 200) >>= printResults
