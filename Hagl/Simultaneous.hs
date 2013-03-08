{-# LANGUAGE FlexibleInstances,
             FunctionalDependencies,
             MultiParamTypeClasses,
             TypeFamilies #-}

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

-- | A general simultaneous move game.  First argument is the number of
--   players, second is the payoff function.
data Simultaneous mv = Simultaneous Int (Profile mv -> Payoff)

-- | The class of games that can be converted into a simultaneous move game.
--   Any game that instantiates 'Norm' (the class of normal form games)
--   automatically instantiates this type class.
class (Game g, Eq mv) => Simult g mv | g -> mv where
  toSimultaneous :: g -> Simultaneous mv

-- | The number of players that can play this game.
numPlayers :: Simult g mv => g -> Int
numPlayers g = np
  where (Simultaneous np _) = toSimultaneous g

-- | Get the payoff for a particular strategy profile.
getPayoff :: (Simult g mv, Eq mv) => g -> Profile mv -> Payoff
getPayoff g p = f p
  where (Simultaneous _ f) = toSimultaneous g


--
-- Instances
--

instance Eq mv => Simult (Simultaneous mv) mv where
  toSimultaneous = id

instance Game (Simultaneous mv) where
  
  type TreeType (Simultaneous mv) = Continuous
  type State    (Simultaneous mv) = ()
  type Move     (Simultaneous mv) = mv

  gameTree (Simultaneous np f) = tree 1 []
    where
      tree p ms
        | p <= np   = Continuous ((), Decision p) (\m -> Just (tree (p+1) (m:ms)))
        | otherwise = Continuous ((), (Payoff . f . ByPlayer . reverse) ms) (\_ -> Nothing)
