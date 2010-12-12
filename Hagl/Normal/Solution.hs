{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Hagl.Normal.Solution where

import Data.Function (on)
import Data.Maybe    (fromJust)
import Data.List     (elemIndex,intersect)

import Hagl.Base hiding (numPlayers)
import Hagl.Normal.Game

---------------------------
-- Equilibrium Solutions --
---------------------------

-- | All pure Nash equilibrium solutions.
--nash :: (Norm g, Eq (Move g)) => g -> [Profile (Move g)]
nash :: (Norm g mv, Eq mv) => g -> [Profile mv]
nash g = [s | s <- profiles n, stable s]
  where n = toNormal g
        stable s = all (uni s) [1 .. numPlayers n]
        uni s p  = and [on (>=) (forPlayer p . getPayoff n) s s' | s' <- change s p]
        change (ByPlayer s) p = let (h,_:t) = splitAt (p-1) s
                                in [ByPlayer (h++e:t) | e <- getMoves n p]

-- | All strong Pareto optimal solutions.
pareto :: (Norm g mv, Eq mv) => g -> [Profile mv]
pareto g = [s | s <- profiles n, opt s]
  where n = toNormal g
        opt s = not (any (imp s) (profiles n))
        imp s s' = let p  = toList (getPayoff n s)
                       p' = toList (getPayoff n s')
                   in or (zipWith (>) p' p) && and (zipWith (>=) p' p)

-- | All Pareto optimal, pure equilibriums.
paretoNash :: (Norm g mv, Eq mv) => g -> [Profile mv]
paretoNash g = pareto n `intersect` nash n
  where n = toNormal g

-- | All saddle points of a matrix game.
saddle :: Eq mv => Matrix mv -> [Profile mv]
saddle g = [p | p <- profiles n, v p == minimum (r p), v p == maximum (c p)]
  where n = toNormal g
        v = forPlayer 1 . getPayoff n
        r (ByPlayer [m,_]) = row g (fromJust (elemIndex m (getMoves n 1)) + 1)
        c (ByPlayer [_,m]) = col g (fromJust (elemIndex m (getMoves n 2)) + 1)
