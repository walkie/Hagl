
module Hagl.Iterated.Print where

import Hagl.Base
import Hagl.Iterated.Accessor


------------------------
-- Printing Functions --
------------------------

-- Print Transcript of the given game.
printTranscriptOfGame :: (GameM m g, Show (Move g)) => Int -> m ()
printTranscriptOfGame n = do
    printStrLn $ "Game " ++ show n ++ ":"
    -- print the transcript
    t  <- forGameM n transcripts
    ps <- players
    let mv (Just i,  m) = "  " ++ show (forPlayer i ps) ++ "'s move: " ++ show m
        mv (Nothing, m) = "  Chance: " ++ show m
    (printStr . unlines . map mv) (reverse t)
    -- maybe print the payoff
    p <- forGameM n payoff
    this <- gameNumber
    if this == n then return ()
                 else printStrLn $ "  Payoff: " ++ showPayoffAsList p

-- Print transcripts of completed games.
printTranscripts :: (GameM m g, Show (Move g)) => m ()
printTranscripts = do n <- numGames
                      mapM_ printTranscriptOfGame [1..n]
