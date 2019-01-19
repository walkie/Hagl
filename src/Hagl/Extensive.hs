{-# LANGUAGE FlexibleContexts, PatternGuards, TypeFamilies #-}

-- | Extensive form representation of games.
module Hagl.Extensive where

import Data.List (intersperse)

import Hagl.Lists
import Hagl.Payoff
import Hagl.Game

--
-- * Representation
--

-- | An extensive form game is a discrete game tree with no state.
type Extensive mv = Discrete () mv

-- | An edge in an extensive form game.
type ExtEdge mv = (mv, Extensive mv)

-- | Smart constructor for extensive game tree nodes.
extensive :: Action mv -> [ExtEdge mv] -> Extensive mv
extensive a = Discrete ((),a)


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
payoff :: Payoff -> Extensive mv
payoff p = extensive (Payoff p) []

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
Discrete ((),a) es <+> Discrete ((),b) fs = extensive (comb a b) (es ++ fs)
  where comb (Payoff (ByPlayer as))
             (Payoff (ByPlayer bs)) = Payoff (ByPlayer (zipWith (+) as bs))
        comb (Chance d) (Chance d') = Chance (d ++ d')
        comb (Decision a) (Decision b) | a == b = Decision a
        comb _ _ = error "<+>: incompatible roots"

-- | Construct a decision node with a single edge. Useful in conjunction
--   the '<|>' operator.
player :: PlayerID -> ExtEdge mv -> Extensive mv
player p e = decision p [e]

-- | Add a decision branch to a game tree.
(<|>) :: Discrete s mv -> Edge s mv -> Discrete s mv
Discrete (s,Decision i) es <|> e = Discrete (s,Decision i) (e:es)


--
-- * Trees with partial information
--

-- | A game tree where players may know only which group they are in,
--   rather than their precise location.
--   (TODO: This isn't really fleshed out yet.)
data GroupedTree t s mv = GroupedTree (InfoGroup t s mv) (t s mv)

-- | An information group is a mapping from a specific location to a set
--   of possible locations.
type InfoGroup t s mv = t s mv -> [t s mv]

instance (GameTree t, Eq mv) => Game (GroupedTree t s mv) where
  type TreeType (GroupedTree t s mv) = t
  type State    (GroupedTree t s mv) = s
  type Move     (GroupedTree t s mv) = mv
  gameTree (GroupedTree _ t) = t

instance (Show (t s mv), Show mv) => Show (GroupedTree t s mv) where
  show (GroupedTree f t) = unlines $ intersperse "*** OR ***" (map show (f t))
