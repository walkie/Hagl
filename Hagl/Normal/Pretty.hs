module Hagl.Normal.Pretty () where

import Data.List (intersperse,isPrefixOf)

import Hagl.Base hiding (numPlayers)
import Hagl.Normal.Game


---------------
-- Instances --
---------------

instance (Eq mv,Show mv) => Show (Normal mv) where
  show = showNormal
instance (Eq mv,Show mv) => Show (Matrix mv) where
  show = showMatrix


----------------------
-- Helper Functions --
----------------------

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

showMatrix :: Show mv => Matrix mv -> String
showMatrix (Matrix ms ns vs) = showGrid ms ns (map (fromList . (:[])) vs)

showNormal :: (Eq mv,Show mv) => Normal mv -> String
showNormal (Normal 2 (ByPlayer [ms,ns]) vs) = showGrid ms ns vs
showNormal g = show (gameTree g (numPlayers g))

{- TODO
showNormal (Normal 3 mss@(ByPlayer [ms,xs,ys]) vs) = 
    unlines ["Player 1: " ++ show m ++ "\n" ++
             showGrid xs ys (extractGrid mss vs [m])
            | m <- ms]
showNormal (Normal n mss vs) =
    unlines ["Players 1 - " ++ show (n-2) ++ ": " ++ show ms ++ "\n" ++ 
             showGrid xs ys (extractGrid mss vs ms)
            | ms <- init]
  where init    = take (n-2) (toList mss)
        [xs,ys] = drop (n-2) (toList mss)
-}
