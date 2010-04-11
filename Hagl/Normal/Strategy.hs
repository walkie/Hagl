
----------------
-- Strategies --
----------------

-- Select a move randomly, for use in strategies.
randomly :: Norm g => Strategy () g
randomly = do g <- game
              Just i <- playerIx
              randomlyFrom (moves g i)

