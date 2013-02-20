{-# LANGUAGE FlexibleContexts #-}

--
-- Functions for pretty printing values and drawing game trees.
--
module Hagl.Base.Pretty where

import Data.List (intersperse)
import qualified Data.Tree as DT (Tree(..), drawTree)

import Hagl.Base.List
import Hagl.Base.Game
import Hagl.Base.Monad

----------------------
-- Helper Functions --
----------------------

showPayoffLine :: Maybe Payoff -> String
showPayoffLine (Just p) = "  Payoff: " ++ showPayoffAsList p
showPayoffLine Nothing  = ""

-- | String representation of a Transcript and Payoff.
showTranscript :: (Game g, Show (Move g)) =>
                  ByPlayer (Player g) -> Transcript (Move g) -> String
showTranscript ps t = (unlines . map mv . reverse) t
  where mv (Just i,  m) = "  " ++ show (forPlayer i ps) ++ "'s move: " ++ show m
        mv (Nothing, m) = "  Chance: " ++ show m

-- | String representation of a MoveSummary.
showMoveSummary :: (Game g, Show (Move g)) =>
                   ByPlayer (Player g) -> MoveSummary (Move g) -> String
showMoveSummary ps mss = (unlines . map row) (zip (toList ps) (toList2 mss))
  where row (p,ms) = "  " ++ show p ++ " moves: " ++ showSeq (reverse (map show ms))

---------------
-- GameTrees --
---------------

instance Show mv => Show (Node s mv) where
  show (Internal d _) = show d
  show (Payoff p)     = showPayoffAsList p

instance Show mv => Show (Decision mv) where
  show (Decision p) = "Player " ++ show p
  show (Chance d)   = "Chance " ++ show d
