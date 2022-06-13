-- | This module provides a generic representation of simultaneous move games
--   with arbitrary payoff functions. This representation is intended for
--   games with a continuous domain of moves, whereas the module @Hagl.Normal@
--   supports the discrete case. Any normal form game can be converted into
--   a simultaneous game, but the reverse is not true because of the
--   discreteness requirement.
module Hagl.Simultaneous where

import Hagl.List
import Hagl.Payoff
import Hagl.Game


--
-- * Simultaneous games
--

-- | Pure strategy profile. One move per player.
type Profile mv = ByPlayer mv

-- | A general simultaneous move game.
data Simultaneous mv = Simultaneous
  { _numPlayers  :: Int                     -- ^ Number of players.
  , _isMoveValid :: PlayerID -> mv -> Bool  -- ^ Is this move valid?
  , _getPayoff   :: Profile mv -> Payoff    -- ^ Payoff function.
  }

instance Game (Simultaneous mv) where
  type State (Simultaneous mv) = ()
  type Move (Simultaneous mv) = mv
  initialState _ = ()
  runGame g = do
      ms <- mapM decide [1 .. _numPlayers g]
      return (
      

-- | Captures games that can be converted into a a simultaneous move game.
class Game g => IsSimultaneous g where
  toSimultaneous :: g -> Simultaneous mv

-- | The number of players that can play this game.
numPlayers :: IsSimultaneous g => g -> Int
numPlayers = _numPlayers . toSimultaneous

-- | Is this move valid for the indicated player?
isMoveValid :: IsSimultaneous g => g -> PlayerID -> Move g -> Bool
isMoveValid = _isMoveValid . toSimultaneous

-- | Get the payoff for a particular strategy profile.
getPayoff :: IsSimultaneous g mv => g -> Profile mv -> Payoff
getPayoff = _getPayoff . toSimultaneous


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
