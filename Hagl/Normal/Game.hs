{-# LANGUAGE FlexibleInstances,
             FunctionalDependencies,
             MultiParamTypeClasses,
             PatternGuards,
             TypeFamilies #-}

module Hagl.Normal.Game where

import Data.List (transpose)

import Hagl.Base hiding (numPlayers)


-----------
-- Types --
-----------

-- | Pure strategy profile; one move per player.
type Profile mv = ByPlayer mv

-- | Normal form game type class.
class Norm g mv | g -> mv where
  toNormal :: g -> Normal mv
  
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

-- | The number of players that can play this game.
numPlayers :: Normal mv -> Int
numPlayers (Normal np _ _) = np

-- | Get the payoff for a particular strategy profile.
getPayoff :: Eq mv => Normal mv -> Profile mv -> Payoff
getPayoff (Normal _ ms ps) m = lookupPay m (payoffMap ms ps)

-- | Get the moves for a particular player.
getMoves :: Normal mv -> PlayerIx -> [mv]
getMoves (Normal _ ms _) p = forPlayer p ms

-- | Get a particular row of the payoff matrix.
row :: Matrix mv -> Int -> [Float]
row (Matrix _ ms ps) i = chunk (length ms) ps !! (i-1)

-- | Get a particular col of the payoff matrix.
col :: Matrix mv -> Int -> [Float]
col (Matrix _ ms ps) i = transpose (chunk (length ms) ps) !! (i-1)

-- | The dimensions of the payoff matrix.
dimensions :: Normal mv -> ByPlayer Int
dimensions (Normal _ mss _) = fmap length mss

-- | A list of all pure strategy profiles.
profiles :: Normal mv -> [Profile mv]
profiles (Normal _ mss _) = dcross mss

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
buildTree :: Eq mv => Normal mv -> GameTree () mv
buildTree g@(Normal _ mss ps) = decisions ms pay
  where ms  = zip [1..] (toList mss)
        pay = GameTree () . Payoff . getPayoff g . fromList

-- | Variant of `buildTree` with the type needed for the `Game` type class.
buildTree' :: Eq mv => Normal mv -> Int -> GameTree () mv
buildTree' g np | np == numPlayers g = buildTree g
                | otherwise          = error "Incorrect number of players."


---------------
-- Instances --
---------------

instance Norm (Normal mv) mv where
  toNormal = id

instance Norm (Matrix mv) mv where
  toNormal (Matrix ms ns vs) = Normal 2 (ByPlayer [ms,ns]) (zerosum vs)

instance Eq mv => Game (Normal mv) where
  type Move  (Normal mv) = mv
  type State (Normal mv) = ()
  gameTree = buildTree'

instance Eq mv => Game (Matrix mv) where
  type Move  (Matrix mv) = mv
  type State (Matrix mv) = ()
  gameTree = buildTree' . toNormal
