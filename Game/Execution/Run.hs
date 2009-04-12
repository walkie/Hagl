module Game.Execution.Run where

import Control.Monad.State
import Data.Maybe
import Game.Definition
import Game.Execution
import Game.Execution.Util
import Game.Util

--------------------
-- Game Execution --
--------------------

evalGame :: Game mv -> [Player mv] -> GameExec mv a -> IO a
evalGame g ps (GameExec f) = evalStateT f $ initState g ps

runGame :: Game mv -> [Player mv] -> GameExec mv a -> IO (ExecState mv)
runGame g ps (GameExec f) = execStateT f $ initState g ps

runStrategy :: Player mv -> GameExec mv (mv, Player mv)
runStrategy (Player n s m) = do (mv, s') <- runStateT (unS m) s
                                return (mv, Player n s' m)

step :: (Eq mv, Show mv) => GameExec mv ()
step = get >>= \state ->
    let t = _location state in case t of
      Decision t next ->
        let (ph, (p:pt)) = splitAt (t-1) $ _players state 
        in do (m, p') <- runStrategy p
              put state { _players = ph ++ p' : pt,
                          _location = fromMaybe (error ("No such move: " ++ show m)) 
                                                (lookup m next),
                          _transcript = DecisionEvent t m : _transcript state }
      Chance dist ->
        let expanded = expandDist dist
        in do i <- randomIndex expanded
              put state { _location = expanded !! i,
                          _transcript = ChanceEvent (branch dist i) : _transcript state }
      Payoff vs ->
        let transcript = PayoffEvent vs : _transcript state
            summary = summarize (_game state) transcript
            history = ByGame $ (transcript, summary) : asList (_history state)
        in put state { _location = tree $ _game state,
                       _transcript = [],
                       _history = history }

once :: (Eq mv, Show mv) => GameExec mv ()
once = do loc <- liftM _location get
          case loc of
            Payoff _ -> step
            _ -> step >> once 
                       
times :: (Eq mv, Show mv) => Int -> GameExec mv ()
times 0 = return ()
times n = once >> times (n-1)

---------------
-- Utilities --
---------------

initState :: Game mv -> [Player mv] -> ExecState mv
initState game ps = ExecState game ps (tree game) [] (ByGame [])

