{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- | All games in Hagl can be reduced to a game tree.
module Hagl.Game where

import Hagl.GameTree


--
-- * Games
--

-- | All games can be reduced to a game tree. This type class's associated
--   types capture the relevant type parameters of that tree.
class Game g where

  -- | The type of edges in the game tree, typically one of: @Finite@ or @NonFinite@.
  type Edge g :: * -> * -> *
  
  -- | The type of state maintained throughout the game (use @()@ for stateless games).
  type State g

  -- | The type of moves that may be played during the game.
  type Move g
  
  -- | A representation of this game as a game tree.
  gameTree :: g -> GameTree (Edge g) (State g) (Move g)

-- | Captures all games whose `Edge` type is `Finite`.
--   Do not instantiate this class directly!
class (Game g, Edge g ~ Finite) => FiniteGame g
instance (Game g, Edge g ~ Finite) => FiniteGame g

-- | Captures all games whose `Edge` type is `NonFinite`.
--   Do not instantiate this class directly!
class (Game g, Edge g ~ NonFinite) => NonFiniteGame g
instance (Game g, Edge g ~ NonFinite) => NonFiniteGame g

instance Game (GameTree e s mv) where
  type Edge  (GameTree e s mv) = e
  type State (GameTree e s mv) = s
  type Move  (GameTree e s mv) = mv
  gameTree = id
