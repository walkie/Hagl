module Hagl.Iterated.Selector where

import Control.Monad (liftM)

import Hagl.Base
import Hagl.Iterated.List

----------------------
-- ByGame Selection --
----------------------

everyGames' :: GameM m g => m (ByGame a) -> m [a]
everyGames' = liftM toList

firstGame's :: GameM m g => m (ByGame a) -> m a
firstGame's = liftM firstGame

thisGame's :: GameM m g => m (ByGame a) -> m a
thisGame's = liftM thisGame

completedGames' :: GameM m g => m (ByGame a) -> m [a]
completedGames' = liftM completedGames

lastGame's :: GameM m g => m (ByGame a) -> m a
lastGame's = liftM lastGame

lastGames' :: GameM m g => Int -> m (ByGame a) -> m [a]
lastGames' i = liftM (lastNGames i)
