-- | This module provides a higher-level way to build game trees for games
--   that are centered around the manipulation of state.
module Hagl.StateGames where

import Hagl.Lists
import Hagl.Payoff
import Hagl.Game

--
-- * State games
--

-- | Build a discrete game tree for a state-based game.
stateTreeD ::
        (s -> PlayerID) -- ^ Whose turn is it?
     -> (s -> Bool)     -- ^ Is the game over?
     -> (s -> [mv])     -- ^ Available moves.
     -> (s -> mv -> s)  -- ^ Execute a move and return the new state.
     -> (s -> Payoff)   -- ^ Payoff for this (final) state.
     -> s               -- ^ The current state.
     -> Discrete s mv
stateTreeD who end moves exec pay = tree
  where tree s | end s     = Discrete (s, Payoff (pay s)) []
               | otherwise = Discrete (s, Decision (who s)) [(m, tree (exec s m)) | m <- moves s]

-- | Build a continuous game tree for a state-based game.
stateTreeC ::
        (s -> PlayerID) -- ^ Whose turn is it?
     -> (s -> Bool)     -- ^ Is the game over?
     -> (s -> mv -> s)  -- ^ Execute a move and return the new state.
     -> (s -> Payoff)   -- ^ Payoff for this (final) state.
     -> s               -- ^ The current state.
     -> Continuous s mv
stateTreeC who end exec pay = tree
  where tree s | end s     = Continuous (s, Payoff (pay s)) (\_ -> Nothing)
               | otherwise = Continuous (s, Decision (who s)) (\m -> Just (tree (exec s m)))

-- | Build a discrete game tree by taking turns making moves until some condition is met.
takeTurnsD ::
        Int                        -- ^ Number of players.
     -> (PlayerID -> s -> Bool)    -- ^ Is the game over?
     -> (PlayerID -> s -> [mv])    -- ^ Available moves.
     -> (PlayerID -> s -> mv -> s) -- ^ Execute a move and return the new state.
     -> (PlayerID -> s -> Payoff)  -- ^ Payoff for this (final) state.
     -> PlayerID                   -- ^ The current player.
     -> s                          -- ^ The current state.
     -> Discrete s mv
takeTurnsD np end moves exec pay = tree
  where tree p s | end p s   = Discrete (s, Payoff (pay p s)) []
                 | otherwise = Discrete (s, Decision p)
                                        [(m, tree (nextPlayer np p) (exec p s m)) | m <- moves p s]

-- | Build a continuous game tree by taking turns making moves until some condition is met.
takeTurnsC ::
        Int                        -- ^ Number of players.
     -> (PlayerID -> s -> Bool)    -- ^ Is the game over?
     -> (PlayerID -> s -> mv -> s) -- ^ Execute a move and return the new state.
     -> (PlayerID -> s -> Payoff)  -- ^ Payoff for this (final) state.
     -> PlayerID                   -- ^ The current player.
     -> s                          -- ^ The current state.
     -> Continuous s mv
takeTurnsC np end exec pay = tree
  where tree p s | end p s   = Continuous (s, Payoff (pay p s)) (\_ -> Nothing)
                 | otherwise = Continuous (s, Decision p)
                                          (\m -> Just (tree (nextPlayer np p) (exec p s m)))
