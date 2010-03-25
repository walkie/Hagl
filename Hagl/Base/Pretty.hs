--
-- Functions for pretty printing values and drawing game trees.
--
module Hagl.Base.Pretty where

import Data.List  (intersperse)
import qualified Data.Tree as DT (Tree(..), drawTree)

import Hagl.Base.List
import Hagl.Base.Types

----------------------
-- Helper Functions --
----------------------

showFloat :: Float -> String
showFloat f | f == fromIntegral i = show i
            | otherwise           = show f
  where i = floor f

showPayoff :: Payoff -> String
showPayoff = concat . intersperse "," . map showFloat . toList

showPayoffAsList :: Payoff -> String
showPayoffAsList p = "[" ++ showPayoff p ++ "]"


---------------
-- GameTrees --
---------------

drawTree :: Show mv => GameTree s mv -> String
drawTree = condense . DT.drawTree . tree ""
  where
    condense = unlines . filter empty . lines
    empty    = not . all (\c -> c == ' ' || c == '|')
    tree s t@(GameTree _ n) = DT.Node (s ++ show n)
                              [tree (show m ++ " -> ") t | (m,t) <- edges t]

instance Show mv => Show (GameTree s mv) where
  show = drawTree

instance Show mv => Show (Node s mv) where
  show (Internal d _) = show d
  show (Payoff p)     = showPayoffAsList p

instance Show mv => Show (Decision mv) where
  show (Decision p) = "Player " ++ show p
  show (Chance d)   = "Chance " ++ show d
