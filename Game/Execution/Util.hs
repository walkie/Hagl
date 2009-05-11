module Game.Execution.Util where

import Control.Monad.State
import Data.List
import Game.Definition
import Game.Execution
import Game.Lists
import System.Random

-- Generate a string showing a set of players' scores.
scoreString :: Show m => [Player m] -> [Float] -> String 
scoreString ps vs = unlines ["  "++show p++": "++show v | (p,v) <- zip ps vs]

randomIndex :: MonadIO m => [a] -> m Int
randomIndex as = liftIO $ getStdRandom $ randomR (0, length as - 1)

summarize :: Game mv -> Transcript mv -> Summary mv
summarize g t = 
    let np = numPlayers g
        addmove i a as = take i as ++ ((a:(as!!i)) : drop i as)
        payoffs (PayoffEvent vs : es) = zipWith (+) vs (payoffs es)
        payoffs (e : es) = payoffs es
        payoffs [] = replicate np 0
        moves (DecisionEvent i m : es) = addmove (i-1) m (moves es)
        moves (e : es) = moves es
        moves [] = replicate np []
    in (ByPlayer (map ByTurn (moves t)), ByPlayer (payoffs t))
