{-# LANGUAGE FlexibleInstances,
             FunctionalDependencies,
             MultiParamTypeClasses,
             PatternGuards,
             TypeFamilies #-}

-- | This module provides different representations of normal form games, smart
--   constructors for creating them, and functions for analyzing them.
module Hagl.Normal where

import Data.Function (on)
import Data.Maybe    (fromJust)
import Data.List     (elemIndex,intersect,intersperse,isPrefixOf,transpose)

import Hagl.Lists
import Hagl.Game
import Hagl.GameTree

--
-- * Normal Form Game Representations
--

-- ** Types
--

-- | Pure strategy profile; one move per player.
type Profile mv = ByPlayer mv

-- | Normal form game type class.
class Norm g mv | g -> mv where
  toNormal :: g -> Normal mv
  
-- | A general normal form game.
data Normal mv = Normal Int (ByPlayer [mv]) [Payoff] deriving Eq

-- | A two-player, zero-sum game.
data Matrix mv = Matrix [mv] [mv] [Float] deriving Eq


-- ** Smart Constructors
--

-- | Smart constructor to build a normal form game from bare lists.
--   TODO should do some consistency checking
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


--
-- * Basic Functions
--

-- TODO generalize Normal mv ---> Norm g mv => g ?

-- | The number of players that can play this game.
numPlayers :: Normal mv -> Int
numPlayers (Normal np _ _) = np

-- | Get the payoff for a particular strategy profile.
getPayoff :: Eq mv => Normal mv -> Profile mv -> Payoff
getPayoff (Normal _ ms ps) m = lookupPay m (payoffMap ms ps)

-- | Get the moves for a particular player.
getMoves :: Normal mv -> PlayerID -> [mv]
getMoves (Normal _ ms _) p = forPlayer p ms

-- | The dimensions of the payoff matrix.
dimensions :: Normal mv -> ByPlayer Int
dimensions (Normal _ mss _) = fmap length mss

-- | A list of all pure strategy profiles.
profiles :: Normal mv -> [Profile mv]
profiles (Normal _ mss _) = buildProfiles mss

-- | Get a particular row of the payoff matrix.
row :: Matrix mv -> Int -> [Float]
row (Matrix _ ms ps) i = chunk (length ms) ps !! (i-1)

-- | Get a particular column of the payoff matrix.
col :: Matrix mv -> Int -> [Float]
col (Matrix _ ms ps) i = transpose (chunk (length ms) ps) !! (i-1)

-- | Construct a zero-sum payoff grid.
zerosum :: [Float] -> [Payoff]
zerosum vs = [ByPlayer [v, -v] | v <- vs]


--
-- * Equilibrium Solutions
--

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
        imp s s' = let ByPlayer p  = getPayoff n s
                       ByPlayer p' = getPayoff n s'
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


--
-- * Helper Stuff
--

-- | A mapping from strategy profiles to payoffs.
type PayoffMap mv = [(Profile mv, Payoff)]

-- | Build a list of all strategy profiles from a list of moves available
--   to each player.
buildProfiles :: ByPlayer [mv] -> [Profile mv]
buildProfiles (ByPlayer mss) = map ByPlayer (cross mss)

-- | Build a mapping from strategy profiles to payoffs.
payoffMap :: ByPlayer [mv] -> [Payoff] -> PayoffMap mv
payoffMap = zip . buildProfiles

-- | Lookup a payoff in a `PayoffMap`.
lookupPay :: Eq mv => Profile mv -> PayoffMap mv -> Payoff
lookupPay p m | Just v <- lookup p m = v
              | otherwise            = error "Invalid strategy profile."

-- | Build the game tree for a normal form game.
buildTree :: Eq mv => Normal mv -> GameTree mv
buildTree g@(Normal _ (ByPlayer mss) ps) = decisions ms pay
  where ms  = zip [1..] mss
        pay ms = GameTree (Payoff (getPayoff g (ByPlayer ms))) []

-- | Variant of `buildTree` with the type needed for the `Game` type class.
buildTree' :: Eq mv => Normal mv -> Int -> GameTree mv
buildTree' g np | np == numPlayers g = buildTree g
                | otherwise          = error "Incorrect number of players."


--
-- * Instances
--

instance Norm (Normal mv) mv where
  toNormal = id

instance Norm (Matrix mv) mv where
  toNormal (Matrix ms ns vs) = Normal 2 (ByPlayer [ms,ns]) (zerosum vs)

instance Eq mv => Game (Normal mv) where
  
  type State (Normal mv) = [mv]
  type Move  (Normal mv) = mv
  
  start _ = ([], Decision 1)
  
  transition g (ms, Decision p) m
      | p < numPlayers g = (ms', Decision (p+1))
      | otherwise        = (ms', (Payoff . getPayoff g . ByPlayer . reverse) ms')
    where ms' = m:ms

instance Eq mv => DiscreteGame (Normal mv) where
  movesFrom g (_, Decision p) = getMoves g p
  movesFrom g _               = []

instance Eq mv => Game (Matrix mv) where
  type State (Matrix mv) = [mv]
  type Move  (Matrix mv) = mv
  start      = start      . toNormal
  transition = transition . toNormal

instance (Eq mv,Show mv) => Show (Normal mv) where
  show = showNormal
instance (Eq mv,Show mv) => Show (Matrix mv) where
  show = showMatrix


--
-- * Pretty Printing
--

-- | Pretty print a matrix game.
showMatrix :: Show mv => Matrix mv -> String
showMatrix (Matrix ms ns vs) = showGrid ms ns (map (ByPlayer . (:[])) vs)

-- | Pretty print a normal form game.
--   Just prints the corresponding game tree if numPlayers != 2.
showNormal :: (Eq mv, Show mv) => Normal mv -> String
showNormal (Normal 2 (ByPlayer [ms,ns]) vs) = showGrid ms ns vs
showNormal g = show (toGameTree g)

-- | Pretty printer helper function.
--   TODO some bugs here (assumes move names are the same length...)
showGrid :: Show mv => [mv] -> [mv] -> [Payoff] -> String
showGrid rs cs vs = showRows (colHead : zipWith (:) rowHead grid)
  where 
    rs' = map show rs
    cs' = map show cs
    vs' = chunk (length cs) (map showPayoff vs)
    n  = max (maxLength cs') (gridMax vs')
    colHead = padLeft n "" : map (pad n) cs'
    rowHead = map (padLeft n) rs'
    grid    = (map . map) (padLeft n) vs'

    padLeft :: Int -> String -> String
    padLeft n s = replicate (n - length s) ' ' ++ s
    
    pad :: Int -> String -> String
    pad n s | length s <  n-1 = pad n (' ' : s ++ " ")
            | length s == n-1 = ' ' : s
            | otherwise       = s
    
    maxLength :: [String] -> Int
    maxLength = maximum . map length
    
    gridMax :: [[String]] -> Int
    gridMax = maximum . map maxLength
    
    showRow :: [String] -> String
    showRow = concat . intersperse " | "
    
    showRows :: [[String]] -> String
    showRows = unlines . map showRow
    
    toGrid :: [mv] -> [Payoff] -> [[Payoff]]
    toGrid = chunk . length
    
    extractGrid :: Eq mv => ByPlayer [mv] -> [Payoff] -> [mv] -> [Payoff]
    extractGrid mss ps ms = [vs | (ByPlayer ms', vs) <- payoffMap mss ps, ms `isPrefixOf` ms']
