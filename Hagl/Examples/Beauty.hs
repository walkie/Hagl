{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             TypeFamilies #-}

{- |

An implementation of the Keynesian p-Beauty Contest.
Also known as the Guessing Game or Guess 2/3 of the Average:
  <http://en.wikipedia.org/wiki/Guess_2/3_of_the_average>

>>> execGame (guess23 4) guessPlayers (once >> printTranscript)

-}
module Hagl.Examples.Beauty where

import Data.Function (on)
import Data.List
import Data.Maybe (fromJust)

import Hagl


--
-- * Game representation
--

-- | Players may guess any floating point number up to the maximum guess.
type Guess = Float

-- | A p-Beauty Contest.  Arguments: number of players, maximum guess, p.
data Beauty = Beauty Int Guess Float

-- | Determine the winner of the p-Beauty Contest.
beautyWinner :: Beauty -> Profile Guess -> Payoff
beautyWinner (Beauty np _ p) (ByPlayer guesses) = winner np (i+1)
  where target = p * (sum guesses) / fromIntegral (length guesses)
        delta  = abs . subtract target
        close  = minimumBy (compare `on` delta) guesses
        Just i = elemIndex close guesses

instance IsSimultaneous Beauty Guess where
  toSimultaneous g@(Beauty np max _) = Simultaneous np ok (beautyWinner g)
    where ok p m = p >= 1 && p <= np && m <= max

instance Game Beauty where
  type TreeType Beauty = Continuous
  type State    Beauty = ()
  type Move     Beauty = Guess
  gameTree = gameTree . toSimultaneous

--
-- * Examples
--

-- | Guess from 0-100, closest to 2/3 average wins.
guess23 :: Int -> Beauty
guess23 np = Beauty np 100 (2/3)

-- | Some /really/ simple players guessing game players.
guessPlayers :: [Player Beauty]
guessPlayers = ["A" ::: return 5, "B" ::: return 5, "C" ::: return 0, "D" ::: return 2]
