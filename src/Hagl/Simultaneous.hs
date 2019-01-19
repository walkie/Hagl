{-# LANGUAGE FlexibleInstances,
             FunctionalDependencies,
             MultiParamTypeClasses,
             TypeFamilies #-}

-- | This module provides a generic representation of simultaneous move games
--   with arbitrary payoff functions.  This representation is intended for
--   games with a continuous domain of moves, whereas the module "Hagl.Normal"
--   supports the discrete case.  Any normal form game can be converted into
--   a simultaneous game, but the reverse is not also true because of the
--   discreteness requirement.
module Hagl.Simultaneous where

import Hagl.Lists
import Hagl.Payoff
import Hagl.Game


--
-- * Simultaneous games
--

-- | Pure strategy profile; one move per player.
type Profile mv = ByPlayer mv

-- | A general simultaneous move game.  Arguments are:
--
--     1. Number of players.
--
--     2. Is this move valid?
--     
--     3. Payoff function.
data Simultaneous mv = Simultaneous Int (PlayerID -> mv -> Bool) (Profile mv -> Payoff)

-- | The class of type constructors for games that can be converted into
--   a simultaneous move game.  Any type constructor that instantiates
--   'IsNormal' (the class of normal form games) should also instantiate
--   this type class.
class IsSimultaneous g mv | g -> mv where
  toSimultaneous :: g -> Simultaneous mv

-- | The number of players that can play this game.
numPlayers :: IsSimultaneous g mv => g -> Int
numPlayers g = np
  where (Simultaneous np _ _) = toSimultaneous g

-- | Is this move valid for the indicated player?
isMoveValid :: IsSimultaneous g mv => g -> PlayerID -> mv -> Bool
isMoveValid g p m = valid p m
  where (Simultaneous _ valid _) = toSimultaneous g

-- | Get the payoff for a particular strategy profile.
getPayoff :: IsSimultaneous g mv => g -> Profile mv -> Payoff
getPayoff g ms = pay ms
  where (Simultaneous _ _ pay) = toSimultaneous g


--
-- Instances
--

instance IsSimultaneous (Simultaneous mv) mv where
  toSimultaneous = id

instance Game (Simultaneous mv) where
  
  type TreeType (Simultaneous mv) = Continuous
  type State    (Simultaneous mv) = ()
  type Move     (Simultaneous mv) = mv

  gameTree (Simultaneous np v f) = tree 1 []
    where
      tree p ms
        | p <= np   = Continuous ((), Decision p)
                      (\m -> if v p m then Just (tree (p+1) (m:ms)) else Nothing)
        | otherwise = Continuous ((), (Payoff . f . ByPlayer . reverse) ms)
                      (\_ -> Nothing)
