{-# LANGUAGE TypeFamilies #-}

-- | Extensive form representation of games.
module Hagl.Extensive where

import Data.List (intersperse)

import Hagl.Lists
import Hagl.Game
import Hagl.GameTree

--
-- * Representation
--

-- | An extensive form game is a game tree with no state.
type Extensive mv = GameTree () mv

-- | An edge in an extensive form game.
type ExtEdge mv = (mv, Extensive mv)

-- | Smart constructor for extensive game tree nodes.
extensive :: Action mv -> [ExtEdge mv] -> Extensive mv
extensive a = GameTree ((),a)


--
-- * Incremental construction
--

-- | Decision node.
decision :: PlayerID -> [ExtEdge mv] -> Extensive mv
decision = extensive . Decision

-- | Chance node.
chance :: Dist mv -> [ExtEdge mv] -> Extensive mv
chance = extensive . Chance

-- | Payoff node.
payoff :: [Float] -> Extensive mv
payoff fs = extensive (Payoff (ByPlayer fs)) []

-- | Begin a game tree in which multiple players decide in turn. The given
--   function defines the branch resulting from each possible sequence of moves.
decisions :: [(PlayerID,[mv])] -> ([mv] -> Extensive mv) -> Extensive mv
decisions ps f = d [] ps
  where d ms []         = f (reverse ms)
        d ms ((p,l):ps) = decision p [(m, d (m:ms) ps) | m <- l]

-- | Construct a payoff node from a list of floats.
pays :: [Float] -> Extensive mv
pays vs = extensive (Payoff (ByPlayer vs)) []

-- | Combines two game trees rooted with the same kind of node.
(<+>) :: Extensive mv -> Extensive mv -> Extensive mv
GameTree ((),a) es <+> GameTree ((),b) fs = extensive (comb a b) (es ++ fs)
  where comb (Payoff (ByPlayer as))
             (Payoff (ByPlayer bs)) = Payoff (ByPlayer (zipWith (+) as bs))
        comb (Chance d) (Chance d') = Chance (d ++ d')
        comb (Decision a) (Decision b) | a == b = Decision a
        comb _ _ = error "<+>: incompatible roots"

-- | Add a decision branch to a game tree.
(<|>) :: GameTree s mv -> Edge s mv -> GameTree s mv
GameTree (s,Decision i) es <|> e = GameTree (s,Decision i) (e:es)


--
-- * Trees with partial information
--

-- | A game tree where players may know only which group they are in,
--   rather than their precise location.
--   (TODO: This isn't really fleshed out yet.)
data GroupedTree s mv = GroupedTree (InfoGroup s mv) (GameTree s mv)

-- | An information group is a mapping from a specific location to a set
--   of possible locations.
type InfoGroup s mv = GameTree s mv -> [GameTree s mv]

instance Eq mv => Game (GroupedTree s mv) where
  type State (GroupedTree s mv)  = (s,[Edge s mv])
  type Move  (GroupedTree s mv)  = mv
  start (GroupedTree _ t)        = start t
  transition (GroupedTree _ t) m = transition t m

instance Show mv => Show (GroupedTree s mv) where
  show (GroupedTree f t) = unlines $ intersperse "*** OR ***" (map show (f t))
