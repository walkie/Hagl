module Examples.Dice where

import Hagl
import Hagl.Extensive

------------------
-- Dice Rolling --
------------------

die = Extensive 1 Perfect (Chance [(1, side a) | a <- [1..6]])
  where side a = (a, Payoff (ByPlayer [a]))

-- roll n dice and print out the total
roll n = execGame die ["Total" ::: undefined] (times n >> printScore)

-------------------
-- Coin Flipping --
-------------------

data Coin = H | T deriving (Eq, Show)

coin :: GameTree Coin -> GameTree Coin -> GameTree Coin
coin h t = Chance [(1,(H,h)), (1,(T,t))]

pick :: PlayerIx -> Coin -> GameTree Coin
pick i c = Decision i [(H, pay H), (T, pay T)]
  where pay p = Payoff (ByPlayer [if p == c then 1 else -1])

pickRight = coin (pick 1 H) (pick 1 T)
