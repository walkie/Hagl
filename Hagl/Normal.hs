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

----------------
-- Strategies --
----------------

-- Select a move randomly, for use in strategies.
randomly :: Norm g => Strategy () g
randomly = do g <- game
              Just i <- playerIx
              randomlyFrom (moves g i)

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

---------------------
-- Pretty Printing --
---------------------

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

showGrid :: Show mv => [mv] -> [mv] -> [Payoff] -> String
showGrid rs cs vs = showRows (colHead : zipWith (:) rowHead grid)
  where rs' = map show rs
        cs' = map show cs
        vs' = chunk (length cs) (map showPayoff vs)
        n  = max (maxLength cs') (gridMax vs')
        colHead = padLeft n "" : map (pad n) cs'
        rowHead = map (padLeft n) rs'
        grid    = (map . map) (padLeft n) vs'

toGrid :: [mv] -> [Payoff] -> [[Payoff]]
toGrid = chunk . length

extractGrid :: Eq mv => ByPlayer [mv] -> [Payoff] -> [mv] -> [Payoff]
extractGrid mss ps ms = [vs | (ByPlayer ms', vs) <- payoffMap mss ps, ms `isPrefixOf` ms']

instance (Eq mv, Show mv) => Show (Normal mv) where
  
  show (Normal 2 (ByPlayer [ms,ns]) vs) = showGrid ms ns vs
  
  show (Normal 3 mss@(ByPlayer [ms,xs,ys]) vs) = 
      unlines ["Player 1: " ++ show m ++ "\n" ++
               showGrid xs ys (extractGrid mss vs [m])
              | m <- ms]
  
  -- TODO this is wrong
  show (Normal n mss vs) =
      unlines ["Players 1 - " ++ show (n-2) ++ ": " ++ show ms ++ "\n" ++ 
               showGrid xs ys (extractGrid mss vs ms)
              | ms <- init]
    where init    = take (n-2) (toList mss)
          [xs,ys] = drop (n-2) (toList mss)

instance Show mv => Show (Matrix mv) where
  show (Matrix ms ns vs) = showGrid ms ns (map (fromList . (:[])) vs)
