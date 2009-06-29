{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Normal where

import Data.List
import Data.Maybe

import Hagl.Core
import Hagl.Game
import Hagl.Accessor (game, playerIx)

type Profile mv = ByPlayer mv -- pure strategy profile

-- A Normal form game (extends Game type class)
class Game g => Norm g where
  numPlayers :: g -> Int
  pays       :: g -> Profile (Move g) -> Payoff
  moves      :: g -> PlayerIx -> [Move g]
  
-- 
-- Normal form game types.
--

-- A general normal form game.
data Normal mv = Normal Int (ByPlayer [mv]) [Payoff] deriving (Eq, Show)

-- A two-player, zero-sum game.
data Matrix mv = Matrix [mv] [mv] [Float] deriving (Eq, Show)

------------------------
-- Smart Constructors --
------------------------

-- Smart constructor to build from bare lists.
normal :: Eq mv => Int -> [[mv]] -> [[Float]] -> Normal mv
normal np mss vs = Normal np (ByPlayer mss) (map ByPlayer vs)

-- Construct a two-player game.
bimatrix :: Eq mv => [[mv]] -> [[Float]] -> Normal mv
bimatrix = normal 2

-- Construct a two-player, symmetric game.
symmetric :: Eq mv => [mv] -> [Float] -> Normal mv
symmetric ms vs = bimatrix [ms, ms] vs'
  where sym = concat (transpose (chunk (length ms) vs))
        vs' = zipWith (\a b -> [a,b]) vs sym

-- Construct a two-player, zero-sum game.
matrix :: [mv] -> [mv] -> [Float] -> Matrix mv
matrix = Matrix

square :: [mv] -> [Float] -> Matrix mv
square ms = Matrix ms ms

---------------------------
-- Equilibrium Solutions --
---------------------------

-- Finds all pure nash equilibrium solutions..
nash :: (Norm g, Eq (Move g)) => g -> [Profile (Move g)]
nash g = [s | s <- profiles g, stable s]
  where stable s = all (uni s) [1 .. numPlayers g]
        uni s p = and [g `pays` s `forPlayer` p >= 
                       g `pays` s' `forPlayer` p | s' <- change s p]
        change (ByPlayer s) p = let (h,_:t) = splitAt (p-1) s 
                                in [ByPlayer (h ++ e:t) | e <- moves g p]

-- Finds all strong Pareto optimal solutions.
pareto :: (Norm g, Eq (Move g)) => g -> [Profile (Move g)]
pareto g = [s | s <- profiles g, opt s]
  where opt s = not (any (imp s) (profiles g))
        imp s s' = let p  = toList (g `pays` s)
                       p' = toList (g `pays` s')
                   in or (zipWith (>) p' p) && and (zipWith (>=) p' p)

-- Finds all pareto optimal, pure equilibriums.
paretoNash :: (Norm g, Eq (Move g)) => g -> [Profile (Move g)]
paretoNash g = pareto g `intersect` nash g

-- Finds all saddle points of a matrix game.
saddle :: Eq mv => Matrix mv -> [Profile mv]
saddle g = [p | p <- profiles g, v p == minimum (r p), v p == maximum (c p)]
  where v p = pays g p `forPlayer` 1
        r (ByPlayer [m,_]) = row g (fromJust (elemIndex m (moves g 1)) + 1)
        c (ByPlayer [_,m]) = col g (fromJust (elemIndex m (moves g 2)) + 1)

--
-- Utility functions used in definitions.
--

-- Get a particular row of the payoff matrix.
row :: Matrix mv -> Int -> [Float]
row (Matrix _ ms ps) i = chunk (length ms) ps !! (i-1)

-- Get a particular col of the payoff matrix.
col :: Matrix mv -> Int -> [Float]
col (Matrix _ ms ps) i = transpose (chunk (length ms) ps) !! (i-1)

-- The dimensions of the payoff matrix.
dimensions :: Norm g => g -> [Int]
dimensions g = let np = numPlayers g
               in [length (moves g i) | i <- [1..np]]

-- A list of all pure strategy profiles.
profiles :: Norm g => g -> [Profile (Move g)]
profiles g = let np = numPlayers g
             in dcross (fromList [moves g i | i <- [1..np]])

runNormal :: Norm g => ExecM g Payoff
runNormal = do g <- game
               ms <- allPlayers decide
               return (g `pays` ms)

lookupPay :: Eq mv => ByPlayer [mv] -> [Payoff] -> Profile mv -> Payoff
lookupPay mss ps ms = fromJust (lookup ms (zip (dcross mss) ps))

-- Construct a zero-sum payoff grid.
zerosum :: [Float] -> [Payoff]
zerosum vs = [fromList [v, -v] | v <- vs]

-- Select a move randomly, for use in strategies.
randomly :: Norm g => Strategy () g
randomly = do g <- game
              Just i <- playerIx
              randomlyFrom (moves g i)

---------------------------
-- Instance Declarations --
---------------------------

instance Eq mv => Game (Normal mv) where
  type Move (Normal mv) = mv
  type State (Normal mv) = ()
  initState _ = ()
  runGame = runNormal

instance Eq mv => Norm (Normal mv) where
  numPlayers (Normal np _ _) = np
  pays (Normal _ mss ps) = lookupPay mss ps
  moves (Normal _ mss _) p = mss `forPlayer` p

instance Eq mv => Game (Matrix mv) where
  type Move (Matrix mv) = mv
  type State (Matrix mv) = ()
  initState _ = ()
  runGame = runNormal

instance Eq mv => Norm (Matrix mv) where
  numPlayers _ = 2
  pays (Matrix ms ns ps) = lookupPay (fromList [ms,ns]) (zerosum ps)
  moves (Matrix ms _ _) 1 = ms
  moves (Matrix _ ms _) 2 = ms

---------------------
-- Pretty Printing --
---------------------

-- TODO
