{-# LANGUAGE UndecidableInstances #-}

-- | Extensive form representation of games.
module Hagl.Extensive where

import Data.List (intersperse)

import Hagl.Game
import Hagl.GameTree
import Hagl.List
import Hagl.Payoff


--
-- * Representation
--

-- | An extensive form game is a discrete game tree with no state.
type Extensive mv = GameTree Finite () mv

-- | An edge in an extensive form game.
type ExtEdge mv = (mv, Extensive mv)


--
-- * Incremental construction
--

-- | Decision node.
decision :: PlayerID -> [ExtEdge mv] -> Extensive mv
decision p es = GameTree () (Decision p (Finite es))

-- | Chance node.
chance :: Dist mv -> [ExtEdge mv] -> Extensive mv
chance d es = GameTree () (Chance d (Finite es))

-- | Payoff node.
payoff :: Payoff -> Extensive mv
payoff p = GameTree () (Payoff p)

-- | Begin a game tree in which multiple players decide in turn. The given
--   function defines the branch resulting from each possible sequence of moves.
decisions :: [(PlayerID,[mv])] -> ([mv] -> Extensive mv) -> Extensive mv
decisions ps f = go [] ps
  where
    go ms []         = f (reverse ms)
    go ms ((p,l):ps) = decision p [(m, go (m:ms) ps) | m <- l]

-- | Construct a payoff node from a list of floats.
pays :: [Float] -> Extensive mv
pays vs = payoff (ByPlayer vs)

-- | Combines two game trees rooted with the same kind of node.
(<+>) :: Extensive mv -> Extensive mv -> Extensive mv
GameTree () l <+> GameTree () r = GameTree () (go l r)
  where
    go (Payoff pl) (Payoff pr) = Payoff (addPayoffs pl pr)
    go (Chance dl el) (Chance dr er) = Chance (dl ++ dr) (el <> er)
    go (Decision il el) (Decision ir er) | il == ir = Decision il (el <> er)
    go _ _ = error "<+>: incompatible roots"

-- | Construct a decision node with a single edge. Useful in conjunction
--   the '<|>' operator.
player :: PlayerID -> ExtEdge mv -> Extensive mv
player p e = decision p [e]

-- | Add a decision branch to a game tree.
(<|>) :: GameTree Finite s mv -> (mv, GameTree Finite s mv) -> GameTree Finite s mv
GameTree s (Decision i (Finite es)) <|> e = GameTree s (Decision i (Finite (e:es)))
_ <|> _ = error "<|>: can only add branches to decision nodes"


--
-- * Trees with partial information
--

-- | A game tree where players may know only which group they are in,
--   rather than their precise location.
--   (TODO: This isn't really fleshed out yet.)
data GroupedTree e s mv = GroupedTree (InfoGroup e s mv) (GameTree e s mv)

-- | An information group is a mapping from a specific location to a set
--   of possible locations.
type InfoGroup e s mv = GameTree e s mv -> [GameTree e s mv]

instance Game (GroupedTree e s mv) where
  type Edge  (GroupedTree e s mv) = e
  type State (GroupedTree e s mv) = s
  type Move  (GroupedTree e s mv) = mv
  gameTree (GroupedTree _ t) = t

instance Show (GameTree e s mv) => Show (GroupedTree e s mv) where
  show (GroupedTree f t) = unlines $ intersperse "*** OR ***" (map show (f t))
