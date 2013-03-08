{-# LANGUAGE TypeFamilies #-}

-- | This module provides a generic representation of simultaneous move games
--   with an arbitrary payoff function.  This representation is intended for
--   games with a continuous domain of moves, whereas the module "Hagl.Normal"
--   supports the discrete case.  Any normal form game can be converted into
--   a simultaneous game, but the reverse is not true since we require the
--   set of available moves in a normal form game to be finite.
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
class IsSimultaneous s where
  toSimultaneous :: Eq mv => s mv -> Simultaneous mv

-- | The number of players that can play this game.
numPlayers :: (IsSimultaneous s, Eq mv) => s mv -> Int
numPlayers g = np
  where (Simultaneous np _ _) = toSimultaneous g

-- | Is this move valid for the indicated player?
isMoveValid :: (IsSimultaneous s, Eq mv) => s mv -> PlayerID -> mv -> Bool
isMoveValid g p m = valid p m
  where (Simultaneous _ valid _) = toSimultaneous g

-- | Get the payoff for a particular strategy profile.
getPayoff :: (IsSimultaneous s, Eq mv) => s mv -> Profile mv -> Payoff
getPayoff g ms = pay ms
  where (Simultaneous _ _ pay) = toSimultaneous g


--
-- Instances
--

instance IsSimultaneous Simultaneous where
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
