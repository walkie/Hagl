
--
-- * Generating game trees
--

-- | Build a tree for a state-based game.
stateGameTree :: (s -> PlayerID) -- ^ Whose turn is it?
              -> (s -> Bool)     -- ^ Is the game over?
              -> (s -> [mv])     -- ^ Available moves.
              -> (s -> mv -> s)  -- ^ Execute a move and return the new state.
              -> (s -> Payoff)   -- ^ Payoff for this (final) state.
              -> s               -- ^ The current state.
              -> Discrete s mv
stateGameTree who end moves exec pay = tree
  where tree s | end s     = Discrete (s, Payoff (pay s)) []
               | otherwise = Discrete (s, Decision (who s)) [(m, tree (exec s m)) | m <- moves s]


