{-# LANGUAGE FlexibleContexts, PatternGuards #-}
module Hagl.Base.Execution (step,finish,runGame) where

import Control.Monad.State hiding (State)
import Hagl.Base.Accessor
import Hagl.Base.Game
import Hagl.Base.List
import Hagl.Base.Monad

--------------------
-- Game Execution --
--------------------

-- | Process one node in the game tree.
step :: (Game g, Eq (Move g)) => ExecM g (Maybe Payoff)
step = location >>= processNode . treeNode

-- | Run the game to completion.
finish :: (Game g, Eq (Move g)) => ExecM g Payoff
finish = step >>= maybe finish return

-- | Execute a game with some given players, returning the payoff.
runGame :: (Game g, Eq (Move g)) => g -> [Player g] -> IO Payoff
runGame g ps = evalGame g ps finish


----------------------
-- Helper Functions --
----------------------

processNode :: (Game g, Eq (Move g)) => Node (State g) (Move g) -> ExecM g (Maybe Payoff)
processNode (Payoff p)      = givePayoff p
processNode (Internal d es) = do m <- decide d
                                 performMove m es
                                 recordMove d m
                                 return Nothing

decide :: Game g => Decision (Move g) -> ExecM g (Move g)
decide (Chance d)   = fromDist d
decide (Decision i) = do p <- liftM (forPlayer i) players
                         (m,p') <- runStrategy p
                         setPlayer i p'
                         return m

performMove :: (Game g, Eq (Move g)) => Move g -> [Edge (State g) (Move g)] -> ExecM g ()
performMove m es | Just t <- lookup m es = setLocation t
                 | otherwise             = fail "Invalid move!"

recordMove :: Game g => Decision (Move g) -> Move g -> ExecM g ()
recordMove d m = do e <- getExec
                    put e { _transcript = moved d m : _transcript e
                          , _numMoves   = numMoves' d (_numMoves e) }
  where numMoves' (Decision p) nm = setListElem (p-1) (forPlayer p nm + 1) nm
        numMoves' _            nm = nm

givePayoff :: Game g => Payoff -> ExecM g (Maybe Payoff)
givePayoff p = do e <- getExec
                  put e { _finalPayoff = Just p }
                  return (Just p)

-- setters

setPlayer :: Game g => PlayerIx -> Player g -> ExecM g ()
setPlayer i p = do e <- getExec
                   put e { _players = setListElem (i-1) p (_players e) }

setLocation :: Game g => GameTree (State g) (Move g) -> ExecM g ()
setLocation l = do e <- getExec
                   put e { _location = l }
