{-# OPTIONS_GHC -fglasgow-exts #-}

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
