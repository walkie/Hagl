{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Hagl.Normal where

import Data.Function (on)
import Data.List
import Data.Maybe (fromJust)

import Hagl.Core
import Hagl.Game
import Hagl.Accessor   (game, playerIx)
import Hagl.GameTree   (GameTree(..))
import Hagl.Searchable (Searchable(..))

type Profile mv = ByPlayer mv -- pure strategy profile

-- A Normal form game
class Game g => Norm g where
  numPlayers :: g -> Int
  pays       :: g -> Profile (Move g) -> Payoff
  moves      :: g -> PlayerIx -> [Move g]
  
-- 
-- Normal form game types.
--

-- A general normal form game.
data Normal mv = Normal Int (ByPlayer [mv]) [Payoff] deriving Eq

-- A two-player, zero-sum game.
data Matrix mv = Matrix [mv] [mv] [Float] deriving Eq

------------------------
-- Smart Constructors --
------------------------

-- Smart constructor to build from bare lists.
-- TODO should do some consistency checking
normal :: Int -> [[mv]] -> [[Float]] -> Normal mv
normal np mss vs = Normal np (ByPlayer mss) (map ByPlayer vs)

-- Construct a two-player game.
bimatrix :: [[mv]] -> [[Float]] -> Normal mv
bimatrix = normal 2

-- Construct a two-player, symmetric game.
symmetric :: [mv] -> [Float] -> Normal mv
symmetric ms vs = bimatrix [ms, ms] vs'
  where sym = concat (transpose (chunk (length ms) vs))
        vs' = zipWith (\a b -> [a,b]) vs sym

-- Construct a two-player, zero-sum game.
matrix :: [mv] -> [mv] -> [Float] -> Matrix mv
matrix = Matrix

square :: [mv] -> [Float] -> Matrix mv
square ms = Matrix ms ms

-----------------------
-- Utility Functions --
-----------------------

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
               return (pays g ms)

payoffMap :: ByPlayer [mv] -> [Payoff] -> [(Profile mv, Payoff)]
payoffMap = zip . dcross

lookupPay :: Eq mv => ByPlayer [mv] -> [Payoff] -> Profile mv -> Payoff
lookupPay mss ps ms = fromJust (lookup ms (payoffMap mss ps))

-- Construct a zero-sum payoff grid.
zerosum :: [Float] -> [Payoff]
zerosum vs = [fromList [v, -v] | v <- vs]

buildTree :: Int -> ByPlayer [mv] -> [Payoff] -> GameTree mv
buildTree np mss vs = head (level 1)
  where level n | n > np    = map Payoff vs
                | otherwise = let ms = forPlayer n mss
                                  bs = chunk (length ms) (level (n+1))
                              in map (Decision n . zip ms) bs

---------------
-- Instances --
---------------

instance Eq mv => Game (Normal mv) where
  type Move (Normal mv) = mv
  type State (Normal mv) = ()
  initState _ = ()
  runGame = runNormal

instance Eq mv => Norm (Normal mv) where
  numPlayers (Normal np _ _) = np
  pays (Normal _ mss ps) = lookupPay mss ps
  moves (Normal _ mss _) p = forPlayer p mss

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

instance Eq mv => Searchable (Normal mv) where
  gameTree (Normal np mss ps) _ = buildTree np mss ps
  nextState _ _ _ = ()

instance Eq mv => Searchable (Matrix mv) where
  gameTree (Matrix ms ns ps) _ = buildTree 2 (ByPlayer [ms,ns]) (zerosum ps)
  nextState _ _ _ = ()

