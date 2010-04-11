
---------------------------
-- Equilibrium Solutions --
---------------------------

-- Finds all pure nash equilibrium solutions..
nash :: (Norm g, Eq (Move g)) => g -> [Profile (Move g)]
nash g = [s | s <- profiles g, stable s]
  where stable s = all (uni s) [1 .. numPlayers g]
        uni s p = and [on (>=) (forPlayer p . pays g) s s' | s' <- change s p]
        change (ByPlayer s) p = let (h,_:t) = splitAt (p-1) s 
                                in [ByPlayer (h ++ e:t) | e <- moves g p]

-- Finds all strong Pareto optimal solutions.
pareto :: (Norm g, Eq (Move g)) => g -> [Profile (Move g)]
pareto g = [s | s <- profiles g, opt s]
  where opt s = not (any (imp s) (profiles g))
        imp s s' = let p  = toList (pays g s)
                       p' = toList (pays g s')
                   in or (zipWith (>) p' p) && and (zipWith (>=) p' p)

-- Finds all pareto optimal, pure equilibriums.
paretoNash :: (Norm g, Eq (Move g)) => g -> [Profile (Move g)]
paretoNash g = pareto g `intersect` nash g

-- Finds all saddle points of a matrix game.
saddle :: Eq mv => Matrix mv -> [Profile mv]
saddle g = [p | p <- profiles g, v p == minimum (r p), v p == maximum (c p)]
  where v = forPlayer 1 . pays g
        r (ByPlayer [m,_]) = row g (fromJust (elemIndex m (moves g 1)) + 1)
        c (ByPlayer [_,m]) = col g (fromJust (elemIndex m (moves g 2)) + 1)

