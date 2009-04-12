module Game.Definition where

import Data.List
import Data.Tree
import Game.Util

---------------------
-- Game Definition --
---------------------

-- Game Definition
data Game mv = Game {
    numPlayers :: Int,
    info       :: GameTree mv -> InfoGroup mv,
    tree       :: GameTree mv
}

-- Game Tree
type PlayerIx = Int
data GameTree mv = Decision PlayerIx [(mv, GameTree mv)]
                 | Chance [(Int, GameTree mv)]
                 | Payoff [Float]
                 deriving Eq

data InfoGroup mv = Perfect (GameTree mv)
                  | Imperfect [GameTree mv]
                  deriving Eq


-- Instance Declarations
instance (Show mv) => Show (GameTree mv) where
  show t = condense $ drawTree $ s "" t
    where s p (Decision i ts) = Node (p ++ "Player " ++ show i) [s (show m ++ " -> ") t | (m, t) <- ts]
          s p (Chance ts) = Node (p ++ "Chance") [s (show c ++ " -> ") t | (c, t) <- ts]
          s p (Payoff vs) = Node (p ++ show vs) []
          condense s = let empty = not . and . map (\c -> c == ' ' || c == '|')
                       in unlines $ filter empty $ lines s
instance (Show mv) => Show (Game mv) where
  show g = show (tree g)
instance (Show mv) => Show (InfoGroup mv) where
  show (Perfect t) = show t
  show (Imperfect ts) = unlines $ intersperse " ** or **" (map (init . show) ts)

----------------------------
-- Normal Form Definition --
----------------------------

-- Construct a game from a Normal-Form definition
normal :: Int -> [[mv]] -> [[Float]] -> Game mv
normal np mss vs = Game np group (head (level 1))
  where level n | n > np = [Payoff v | v <- vs]
                | otherwise = let ms = mss !! (n-1) 
                                  bs = chunk (length ms) (level (n+1)) 
                              in map (Decision n . zip ms) bs
        group (Decision n _) = Imperfect (level n)
        group t = Perfect t

-- Construct a two-player Normal-Form game, where each player has the same moves.
matrix :: [mv] -> [[Float]] -> Game mv
matrix ms = normal 2 [ms,ms]

-- Construct a two-player Zero-Sum game, where each player has the same moves.
zerosum :: [mv] -> [Float] -> Game mv
zerosum ms vs = matrix ms [[v, -v] | v <- vs]

-------------------------------
-- Extensive Form Definition --
-------------------------------

-- Build a game from a tree. Assumes a finite game tree.
extensive :: GameTree mv -> Game mv
extensive t = Game (maxPlayer t) Perfect t

-----------------------------
-- State-Driven Definition --
-----------------------------

{- Build a state-based game.
 - Args:
     * Number of players.
     * Whose turn is it?
     * Is the game over?
     * What are the available moves?
     * Execute a move and return the new state.
     * What is the payoff for this (final) state?
     * Initial state. -}
stateGame :: Int -> (s -> PlayerIx) -> (s -> PlayerIx -> Bool) -> 
             (s -> PlayerIx -> [mv]) -> (s -> PlayerIx -> mv -> s) -> 
             (s -> PlayerIx -> [Float]) -> s -> Game mv
stateGame np who end moves exec pay init = Game np Perfect (tree init)
  where tree s | end s p = Payoff (pay s p)
               | otherwise = Decision p [(m, tree (exec s p m)) | m <- moves s p]
          where p = who s

{- Build a state-based game where the players take turns. Player 1 goes first.
 - Args:
     * Number of players.
     * Is the game over?
     * What are the available moves?
     * Execute a move and return the new state.
     * What is the payoff for this (final) state?
     * Initial state. -}
takeTurns :: Int -> (s -> PlayerIx -> Bool) -> (s -> PlayerIx -> [mv]) ->
             (s -> PlayerIx -> mv -> s) -> (s -> PlayerIx -> [Float]) -> s ->
             Game mv
takeTurns np end moves exec pay init =
    stateGame np snd (lft end) (lft moves) exec' (lft pay) (init, 1)
  where exec' (s,_) p m = (exec s p m, (mod p np) + 1)
        lft f (s,_) p = f s p

----------------------------
-- Game Tree Construction --
----------------------------

-- Construct a payoff where player w wins (1) and all other players,
-- out of np, lose (-1).
winner :: Int -> PlayerIx -> [Float]
winner np w = replicate (w-1) (-1) ++ (fromIntegral np - 1) : replicate (np - w) (-1)

-- Construct a payoff where player w loses (-1) and all other players,
-- out of np, win (1).
loser :: Int -> PlayerIx -> [Float]
loser np l = replicate (l-1) 1 ++ (1 - fromIntegral np) : replicate (np - l) 1

tie :: Int -> [Float]
tie np = replicate np 0

-- Construct a decision node with only one option.
player :: PlayerIx -> (mv, GameTree mv) -> GameTree mv
player i m = Decision i [m]

-- Combines two game trees.
(<+>) :: GameTree mv -> GameTree mv -> GameTree mv
Payoff as <+> Payoff bs = Payoff (zipWith (+) as bs)
Chance as <+> Chance bs = Chance (as ++ bs)
Decision a as <+> Decision b bs | a == b = Decision a (as ++ bs)

-- Add a decision branch to a game tree.
(<|>) :: GameTree mv -> (mv, GameTree mv) -> GameTree mv
Decision i ms <|> m = Decision i (m:ms)

-------------------------
-- Game Tree Traversal --
-------------------------

-- Returns the highest number player from this finite game tree.
maxPlayer :: GameTree mv -> PlayerIx
maxPlayer t = foldl1 max $ map p (dfs t)
  where p (Decision i _) = i
        p _ = 0

-- Return the moves that are available from this node.
availMoves :: GameTree mv -> [mv]
availMoves (Decision _ ms) = map fst ms
availMoves _ = []

-- The immediate children of a node.
children :: GameTree mv -> [GameTree mv]
children (Decision _ ms) = map snd ms
children (Chance cs) = map snd cs
children _ = []

-- Search nodes in BFS order.
bfs :: GameTree mv -> [GameTree mv]
bfs t = let b [] = []
            b ts = ts ++ b (concatMap children ts)
        in b [t]

-- Search nodes DFS order.
dfs :: GameTree mv -> [GameTree mv]
dfs t = t : concatMap dfs (children t)

-- Get the game tree as a Data.Tree structure.
asTree :: GameTree mv -> Tree (GameTree mv)
asTree t = Node t $ map asTree (children t)
