{-# LANGUAGE FlexibleInstances,
             FunctionalDependencies,
             MultiParamTypeClasses,
             TypeFamilies #-}

-- | This module provides different representations of normal form games, smart
--   constructors for creating them, and functions for analyzing them.
module Hagl.Normal where

import Data.Function (on)
import Data.Maybe    (fromJust)
import Data.List     (elemIndex,intercalate,intersect,isPrefixOf,transpose)

import Hagl.Lists
import Hagl.Payoff
import Hagl.Game
import Hagl.Extensive
import Hagl.Simultaneous


--
-- * Representation
--

-- | A class of type constructors for games that can be converted to normal form.
--   Technically this includes all finite, discrete games, but such a generic
--   instance is not provided.  Rather this is used to provide a common
--   interface for grid-structured games.
class IsNormal g mv | g -> mv where
  toNormal :: g -> Normal mv
  
-- | A general normal form game.  Arguments are the number of players,
--   a list of moves available to each player, and the payoff corresponding
--   to each potential combination of moves.
data Normal mv = Normal Int (ByPlayer [mv]) [Payoff] deriving Eq

-- | A two-player, zero-sum game.
data Matrix mv = Matrix [mv] [mv] [Float] deriving Eq


--
-- * Smart constructors
--

-- | Smart constructor to build a normal form game from bare lists.
--   (TODO: Should do some consistency checking.)
normal :: Int -> [[mv]] -> [[Float]] -> Normal mv
normal np mss vs = Normal np (ByPlayer mss) (map ByPlayer vs)

-- | Construct a two-player game.
bimatrix :: [mv] -> [mv] -> [Float] -> [Float] -> Normal mv
bimatrix ms ns vs ws = normal 2 [ms,ns] (zipWith (\a b -> [a,b]) vs ws)

-- | Construct a two-player, symmetric game.
symmetric :: [mv] -> [Float] -> Normal mv
symmetric ms vs = bimatrix ms ms vs sym
  where sym = concat (transpose (chunk (length ms) vs))

-- | Construct a two-player, zero-sum game.
matrix :: [mv] -> [mv] -> [Float] -> Matrix mv
matrix = Matrix

-- | Construct a two-player, symmetric, zero-sum game.
square :: [mv] -> [Float] -> Matrix mv
square ms = Matrix ms ms


--
-- * Basic functions
--

-- | Get the moves for a particular player.
getMoves :: IsNormal g mv => g -> PlayerID -> [mv]
getMoves g p = forPlayer p ms
  where (Normal _ ms _) = toNormal g

-- | The dimensions of the payoff matrix.
dimensions :: IsNormal g mv => g -> ByPlayer Int
dimensions g = fmap length mss
  where (Normal _ mss _) = toNormal g

-- | A list of all pure strategy profiles.
profiles :: IsNormal g mv => g -> [Profile mv]
profiles g = buildProfiles mss
  where (Normal _ mss _) = toNormal g

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
-- * Equilibrium solutions
--

-- | All pure Nash equilibrium solutions.
nash :: (IsNormal g mv, Eq mv) => g -> [Profile mv]
nash g = [s | s <- profiles n, stable s]
  where n = toNormal g
        stable s = all (uni s) [1 .. numPlayers n]
        uni s p  = and [on (>=) (forPlayer p . getPayoff n) s s' | s' <- change s p]
        change (ByPlayer s) p = let (h,_:t) = splitAt (p-1) s
                                in [ByPlayer (h++e:t) | e <- getMoves n p]

-- | All strong Pareto optimal solutions.
pareto :: (IsNormal g mv, Eq mv) => g -> [Profile mv]
pareto g = [s | s <- profiles n, opt s]
  where n = toNormal g
        opt s = not (any (imp s) (profiles n))
        imp s s' = let ByPlayer p  = getPayoff n s
                       ByPlayer p' = getPayoff n s'
                   in or (zipWith (>) p' p) && and (zipWith (>=) p' p)

-- | All Pareto optimal, pure equilibriums.
paretoNash :: (IsNormal g mv, Eq mv) => g -> [Profile mv]
paretoNash g = pareto n `intersect` nash n
  where n = toNormal g

-- | All saddle points of a matrix game.  Saddle points correspond to pure
--   Nash equilibria.
saddle :: Eq mv => Matrix mv -> [Profile mv]
saddle g = [p | p <- profiles n, v p == minimum (r p), v p == maximum (c p)]
  where n = toNormal g
        v = forPlayer 1 . getPayoff n
        r (ByPlayer [m,_]) = row g (fromJust (elemIndex m (getMoves n 1)) + 1)
        c (ByPlayer [_,m]) = col g (fromJust (elemIndex m (getMoves n 2)) + 1)


--
-- * Helper stuff
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


--
-- Instances
--

instance IsNormal (Normal mv) mv where
  toNormal = id

instance IsNormal (Matrix mv) mv where
  toNormal (Matrix ms ns vs) = Normal 2 (ByPlayer [ms,ns]) (zerosum vs)

instance Eq mv => IsSimultaneous (Normal mv) mv where
  toSimultaneous (Normal np ms ps) = Simultaneous np ok pay
    where pay p  = lookupPay p (payoffMap ms ps)
          ok i m = i >= 1 && i <= dlength ms && (m `elem` forPlayer i ms)

instance Eq mv => IsSimultaneous (Matrix mv) mv where
  toSimultaneous = toSimultaneous . toNormal

instance Eq mv => Game (Normal mv) where
  
  type TreeType (Normal mv) = Discrete 
  type State    (Normal mv) = ()
  type Move     (Normal mv) = mv
  
  gameTree g = tree 1 []
    where
      tree p ms
        | p <= numPlayers g = decision p [(m, tree (p+1) (m:ms)) | m <- getMoves g p]
        | otherwise         = (payoff . getPayoff g . ByPlayer . reverse) ms

instance Eq mv => Game (Matrix mv) where
  type TreeType (Matrix mv) = Discrete 
  type State    (Matrix mv) = ()
  type Move     (Matrix mv) = mv
  gameTree = gameTree . toNormal

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
showNormal g = show (gameTree g)

-- | Pretty printer helper function.
--   (TODO: There are ome bugs in here. For one, it assumes move names
--    are the same length...)
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
    showRow = intercalate " | "
    
    showRows :: [[String]] -> String
    showRows = unlines . map showRow
    
    toGrid :: [mv] -> [Payoff] -> [[Payoff]]
    toGrid = chunk . length
    
    extractGrid :: Eq mv => ByPlayer [mv] -> [Payoff] -> [mv] -> [Payoff]
    extractGrid mss ps ms = [vs | (ByPlayer ms', vs) <- payoffMap mss ps, ms `isPrefixOf` ms']
