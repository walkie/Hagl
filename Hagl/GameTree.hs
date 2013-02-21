{-# LANGUAGE TypeFamilies, PatternGuards #-}

-- | Extensive form representation of games.
module Hagl.GameTree where

import Data.Maybe (fromMaybe)
import Data.List  (intersperse)
import qualified Data.Tree as DT (Tree(..), drawTree)

import Hagl.Lists
import Hagl.Game

--
-- * Representation
--

-- | An edge represents a single transition from one location in a game tree
--   to another, via a move.
type Edge mv = (mv, GameTree mv)

-- | A location in a game tree consists of an action to perform
--   and a discrete set of outbound edges.
data GameTree mv = GameTree {
  action :: Action mv, -- ^ The action to perform at this location.
  edges  :: [Edge mv]  -- ^ The outbound edges.
} deriving Eq

-- | Generate the game tree for a discrete game.
gameTree :: DiscreteGame g => g -> GameTree (Move g)
gameTree g = gameTreeFrom g (start g)

-- | Generate the game tree from a particular location in a discrete game.
gameTreeFrom :: DiscreteGame g => g -> Node (State g) (Move g) -> GameTree (Move g)
gameTreeFrom g n@(_,a) = GameTree a [(m, gameTreeFrom g (transition g n m)) | m <- movesFrom g n]


-- Instances

instance Eq mv => Game (GameTree mv) where
  type State (GameTree mv) = [Edge mv]
  type Move  (GameTree mv) = mv
  start (GameTree a es) = (es,a)
  transition _ (es,_) mv
    | Just (GameTree a es) <- lookup mv es = (es,a)
    | otherwise = error "GameTree: invalid move!"

instance Eq mv => DiscreteGame (GameTree mv) where
  movesFrom _ (es,_) = map fst es


--
-- * Smart constructors
--

-- | Decision node.
decision :: PlayerID -> [Edge mv] -> GameTree mv
decision = GameTree . Decision

-- | Chance node.
chance :: Dist mv -> [Edge mv] -> GameTree mv
chance = GameTree . Chance

-- | Payoff node.
payoff :: [Float] -> GameTree mv
payoff fs = GameTree (Payoff (ByPlayer fs)) []

-- | Begin a game tree in which multiple players decide in turn. The given
--   function defines the branch resulting from each possible sequence of moves.
decisions :: [(PlayerID,[mv])] -> ([mv] -> GameTree mv) -> GameTree mv
decisions ps f = d [] ps
  where d ms []         = f (reverse ms)
        d ms ((p,l):ps) = decision p [(m, d (m:ms) ps) | m <- l]

-- | Build a tree for a state-based game.
stateGameTree :: (s -> PlayerID) -- ^ Whose turn is it?
              -> (s -> Bool)     -- ^ Is the game over?
              -> (s -> [mv])     -- ^ Available moves.
              -> (s -> mv -> s)  -- ^ Execute a move and return the new state.
              -> (s -> Payoff)   -- ^ Payoff for this (final) state.
              -> s               -- ^ The current state.
              -> GameTree mv
stateGameTree who end moves exec pay init = tree init
  where tree s | end s     = GameTree (Payoff (pay s)) []
               | otherwise = GameTree (Decision (who s)) [(m, tree (exec s m)) | m <- moves s]


--
-- * Incremental construction
--

-- | Construct a payoff node from a list of floats.
pays :: [Float] -> GameTree mv
pays vs = GameTree (Payoff (ByPlayer vs)) []

-- | Combines two game trees rooted with the same kind of node.
(<+>) :: GameTree mv -> GameTree mv -> GameTree mv
GameTree a es <+> GameTree b fs = GameTree (comb a b) (es ++ fs)
  where comb (Payoff (ByPlayer as))
             (Payoff (ByPlayer bs)) = Payoff (ByPlayer (zipWith (+) as bs))
        comb (Chance d) (Chance d') = Chance (d ++ d')
        comb (Decision a) (Decision b) | a == b = Decision a
        comb _ _ = error "<+>: incompatible roots"

-- | Add a decision branch to a game tree.
(<|>) :: GameTree mv -> Edge mv -> GameTree mv
GameTree (Decision i) es <|> e = GameTree (Decision i) (e:es)


--
-- * Simple queries
--

-- | Get the PlayerID corresponding to a decision node.
playerID :: GameTree mv -> Maybe PlayerID
playerID (GameTree (Decision p) _) = Just p
playerID _                         = Nothing

-- | The highest numbered player in this finite game tree.
maxPlayer :: GameTree mv -> Int
maxPlayer t = foldl1 max $ map (fromMaybe 0 . playerID) (dfs t)

-- | The immediate children of a node.
children :: GameTree mv -> [GameTree mv]
children = map snd . edges

-- | Get a particular child node by following the edge labeled with mv.
child :: Eq mv => mv -> GameTree mv -> GameTree mv
child mv t | Just t' <- lookup mv (edges t) = t'
child _  _ = error "GameTree.child: invalid move"


--
-- * Traversals
--

-- | Nodes in BFS order.
bfs :: GameTree mv -> [GameTree mv]
bfs t = bfs' [t]
  where bfs' [] = []
        bfs' ns = ns ++ bfs' (concatMap children ns)

-- | Nodes in DFS order.
dfs :: GameTree mv -> [GameTree mv]
dfs t = t : concatMap dfs (children t)


--
-- * Pretty printing
--

-- | A nice string representation of a game tree.
drawTree :: Show mv => GameTree mv -> String
drawTree = condense . DT.drawTree . tree ""
  where
    condense = unlines . filter empty . lines
    empty    = not . all (\c -> c == ' ' || c == '|')
    tree s t@(GameTree a _) = DT.Node (s ++ show a)
                              [tree (show m ++ " -> ") t | (m,t) <- edges t]

instance Show mv => Show (GameTree mv) where
  show = drawTree


--
-- * Partial information
--

-- | Extensive form game with partial information
data Extensive mv = Extensive (Info mv) (GameTree mv) 

-- | Information group
data Info mv = Perfect
             | Imperfect (GameTree mv -> [GameTree mv])

-- | Construct a perfect information game from a GameTree.
extensive :: GameTree mv -> Extensive mv
extensive t = Extensive Perfect t

-- Instances 

instance Eq mv => Game (Extensive mv) where
  type State (Extensive mv) = [Edge mv]
  type Move (Extensive mv) = mv
  start (Extensive _ t) = start t
  transition (Extensive _ t) m = transition t m

instance Show mv => Show (Extensive mv) where
  show (Extensive Perfect t)       = show t
  show (Extensive (Imperfect f) t) = unlines $ intersperse "*** OR ***" (map show (f t))
