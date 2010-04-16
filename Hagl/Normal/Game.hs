{-# LANGUAGE FlexibleContexts, PatternGuards, TypeFamilies #-}

module Hagl.Normal.Game where

import Data.List (transpose)

import Hagl.Base hiding (numPlayers)


-----------
-- Types --
-----------

-- | Pure strategy profile; one move per player.
type Profile mv = ByPlayer mv

-- | Normal form game type class.
class Game g => Norm g where
  numPlayers :: g -> Int
  pays       :: g -> Profile (Move g) -> Payoff
  moves      :: g -> PlayerIx -> [Move g]
  
-- | A general normal form game.
data Normal mv = Normal Int (ByPlayer [mv]) [Payoff] deriving Eq

-- | A two-player, zero-sum game.
data Matrix mv = Matrix [mv] [mv] [Float] deriving Eq


------------------------
-- Smart Constructors --
------------------------

-- | Smart constructor to build from bare lists.
-- TODO should do some consistency checking
normal :: Int -> [[mv]] -> [[Float]] -> Normal mv
normal np mss vs = Normal np (ByPlayer mss) (map ByPlayer vs)

-- | Construct a two-player game.
bimatrix :: [[mv]] -> [[Float]] -> Normal mv
bimatrix = normal 2

-- | Construct a two-player, symmetric game.
symmetric :: [mv] -> [Float] -> Normal mv
symmetric ms vs = bimatrix [ms, ms] vs'
  where sym = concat (transpose (chunk (length ms) vs))
        vs' = zipWith (\a b -> [a,b]) vs sym

-- | Construct a two-player, zero-sum game.
matrix :: [mv] -> [mv] -> [Float] -> Matrix mv
matrix = Matrix

-- | Construct a two-player, symmetric, zero-sum game.
square :: [mv] -> [Float] -> Matrix mv
square ms = Matrix ms ms


-----------------------
-- Utility Functions --
-----------------------

-- | Get a particular row of the payoff matrix.
row :: Matrix mv -> Int -> [Float]
row (Matrix _ ms ps) i = chunk (length ms) ps !! (i-1)

-- | Get a particular col of the payoff matrix.
col :: Matrix mv -> Int -> [Float]
col (Matrix _ ms ps) i = transpose (chunk (length ms) ps) !! (i-1)

-- | The dimensions of the payoff matrix.
dimensions :: Norm g => g -> [Int]
dimensions g = let np = numPlayers g
               in [length (moves g i) | i <- [1..np]]

-- | A list of all pure strategy profiles.
profiles :: Norm g => g -> [Profile (Move g)]
profiles g = let np = numPlayers g
             in dcross (fromList [moves g i | i <- [1..np]])

-- | Construct a zero-sum payoff grid.
zerosum :: [Float] -> [Payoff]
zerosum vs = [fromList [v, -v] | v <- vs]


------------------
-- Helper Stuff --
------------------

type PayoffMap mv = [(Profile mv, Payoff)]

-- | A mapping from strategy profiles to payoffs.
payoffMap :: ByPlayer [mv] -> [Payoff] -> PayoffMap mv
payoffMap = zip . dcross

-- | Lookup a payoff in a `PayoffMap`.
lookupPay :: Eq mv => Profile mv -> PayoffMap mv -> Payoff
lookupPay p m | Just v <- lookup p m = v
              | otherwise            = error "Invalid strategy profile."

-- | Build the game tree for a normal form game.
buildTree :: Norm g => g -> GameTree () (Move g)
buildTree g = decisions ms pay
  where ms  = [(p, moves g p) | p <- [1 .. numPlayers g]]
        pay = payoff . pays g . fromList

-- | Variant of `buildTree` with the type needed for the `Game` type class.
buildTree' :: Norm g => g -> Int -> GameTree () (Move g)
buildTree' g np | np == numPlayers g = buildTree g
                | otherwise          = error "Incorrect number of players."


---------------
-- Instances --
---------------

instance Eq mv => Game (Normal mv) where
  type Move  (Normal mv) = mv
  type State (Normal mv) = ()
  gameTree = buildTree'

instance Eq mv => Norm (Normal mv) where
  numPlayers (Normal np _   _ )    = np
  pays       (Normal _  mss ps) ms = lookupPay ms (payoffMap mss ps)
  moves      (Normal _  mss _ ) p  = forPlayer p mss

instance Eq mv => Game (Matrix mv) where
  type Move  (Matrix mv) = mv
  type State (Matrix mv) = ()
  gameTree = buildTree'

instance Eq mv => Norm (Matrix mv) where
  numPlayers _ = 2
  pays  (Matrix ms ns vs) os = lookupPay os (payoffMap (fromList [ms,ns]) (zerosum vs))
  moves (Matrix ms _  _ ) 1  = ms
  moves (Matrix _  ms _ ) 2  = ms
