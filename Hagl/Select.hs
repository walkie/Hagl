{-# LANGUAGE RankNTypes, TypeFamilies #-}
module Hagl.Select where

import Control.Monad

import Hagl.Core

class Selectable a where
  type List a
  type Elem a
  type Result a
  selectOne  :: (List a -> Elem a)   -> a -> Result a
  selectMany :: (List a -> [Elem a]) -> a -> [Result a]

instance Selectable (ByGame a) where
  type List   (ByGame a) = ByGame a
  type Elem   (ByGame a) = a
  type Result (ByGame a) = a
  selectOne  = ($)
  selectMany = ($)

instance Selectable (ByTurn a) where
  type List   (ByTurn a) = ByTurn a
  type Elem   (ByTurn a) = a
  type Result (ByTurn a) = a
  selectOne  = ($)
  selectMany = ($)

instance Selectable (ByPlayer a) where
  type List   (ByPlayer a) = ByPlayer a
  type Elem   (ByPlayer a) = a
  type Result (ByPlayer a) = a
  selectOne  = ($)
  selectMany = ($)

instance Selectable a => Selectable [a] where
  type List   [a] = List a
  type Elem   [a] = Elem a
  type Result [a] = [Result a]
  selectOne  = map . selectOne
  selectMany = map . selectMany

selectOneM :: (GameM m g, Selectable a) => (List a -> Elem a) -> m a -> m (Result a)
selectOneM = liftM . selectOne

selectManyM :: (GameM m g, Selectable a) => (List a -> [Elem a]) -> m a -> m [Result a]
selectManyM = liftM . selectMany

type SelectOne d =
  forall m g a b. (GameM m g, Selectable a, d b ~ List a, b ~ Elem a) => 
    m a -> m (Result a)

type SelectMany d =
  forall m g a b. (GameM m g, Selectable a, d b ~ List a, b ~ Elem a) => 
    m a -> m [Result a]

{-
testMy :: SelectOne ByPlayer
testMy x = do i <- myIx
              selectOneM (flip forPlayer i) x

testOur :: SelectMany ByPlayer
testOur = selectManyM toList

testEvery :: ByGameOrTurn d => d a -> SelectMany d
testEvery _ = selectManyM toList
-}

-- goal: 
-- execGame pd [mum, "Test" ::: return C `atFirstThen` (printLn (testMy (every game's payoff)) >> return C)] (times 5)
