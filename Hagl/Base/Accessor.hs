module Hagl.Base.Accessor where

import Control.Monad (liftM, liftM2)
import Data.Maybe    (fromMaybe)

import Hagl.Base.Game
import Hagl.Base.List
import Hagl.Base.Monad

--------------------
-- Data Accessors --
--------------------

-- | The game being played.
game :: GameM m g => m g
game = liftM _game getExec

-- | The players playing.
players :: GameM m g => m (ByPlayer (Player g))
players = liftM _players getExec

-- | Current location in the game tree.
location :: GameM m g => m (GameTree (State g) (Move g))
location = liftM _location getExec

-- | The current game state.
state :: GameM m g => m (State g)
state = liftM treeState location

-- | Transcript of all moves so far.
transcript :: GameM m g => m (Transcript (Move g))
transcript = liftM _transcript getExec
  
-- | Transcript of moves so far, separated by player.
move :: GameM m g => m (MoveSummary (Move g))
move = liftM2 summarize numPlayers transcript

-- | The total number of moves each player has played.
numMoves :: GameM m g => m (ByPlayer Int)
numMoves = liftM _numMoves getExec

-- | The index of the currently active player.
myIx :: GameM m g => m PlayerIx
myIx = location >>= return . (fromMaybe e) . playerIx
  where e = error "Internal error: myIx on non-decision node!"

-- | The currently active player.
me :: GameM m g => m (Player g)
me = liftM2 forPlayer myIx players

-- | The number of players playing the game.
numPlayers :: GameM m g => m Int
numPlayers = liftM dlength players
