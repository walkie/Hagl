{-# LANGUAGE NoMonomorphismRestriction #-}

{- |

A simplistic model of the Cuban Missile Crisis.

From GHCi, print the game tree:

>>> crisis

Or run a simulation:

>>> evalGame crisis [khrushchev, kennedy] (finish >> printTranscript)
>>> evalGame crisis [khrushchev, kennedy] (step >> step >> printTranscript >> printMovesFromHere >> finish >> printTranscript)

-}

module Hagl.Examples.Crisis where

import Prelude hiding (last, print)

import Hagl

--
-- * Representation
--

-- | The Cuban Missle Crisis, as a game tree.
crisis = start
  where ussr = player 1
        usa  = player 2
        nukesInTurkey = pays [  -2,   1]
        nukesInCuba   = pays [   1,  -2]
        nuclearWar    = chance [(1, "Truly devastating war"), (4, "Devastating war")] [("Truly devastating war", pays [-100,-100]), ("Devastating war", pays [-20, -20])]
        noNukes       = pays [   0,   0]
        start = ussr ("Send Missiles to Cuba", usaResponse) 
                 <|> ("Do Nothing", nukesInTurkey)
        usaResponse = usa ("Do Nothing", nukesInTurkey <+> nukesInCuba)
                      <|> ("Blockade", ussrBlockadeCounter)
                      <|> ("Air Strike", ussrStrikeCounter)
        ussrBlockadeCounter = ussr ("Agree to Terms", noNukes) 
                               <|> ("Escalate", nuclearWar)
        ussrStrikeCounter = ussr ("Pull Out", nukesInTurkey)
                             <|> ("Escalate", nuclearWar)

--
-- * Players
--

-- | Nikita Khrushchev
khrushchev = "Khrushchev" :::
    play "Send Missiles to Cuba" `atFirstThen`
    do m <- his move `inThe` lastTurn's
       play $ case m of "Blockade" -> "Agree to Terms"
                        "Air Strike" -> "Pull Out"

-- | John F. Kennedy
kennedy = "Kennedy" ::: mixed [(2, "Blockade"), (1, "Air Strike")]
