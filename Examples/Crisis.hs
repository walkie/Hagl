{-# LANGUAGE NoMonomorphismRestriction #-}

{-

A simplistic model of the Cuban Missile Crisis.

From GHCi, print the game tree:
> crisis

Or run a simulation:
> execGame crisis [khrushchev, kennedy] (once >> printTranscript)
> execGame crisis [khrushchev, kennedy] (step >> step >> printLn (this game's moves) >> print location >> finish >> printTranscript)

-}

module Examples.Crisis where

import Prelude hiding (last, print)

import Hagl
import Hagl.Extensive
import Hagl.Searchable

--------------------------
-- Cuban Missile Crisis --
--------------------------

crisis = extensive start
  where ussr = player 1
        usa  = player 2
        nukesInTurkey = Payoff (ByPlayer [  -2,   1])
        nukesInCuba   = Payoff (ByPlayer [   1,  -2])
        nuclearWar    = Payoff (ByPlayer [-100,-100])
        noNukes       = Payoff (ByPlayer [   0,   0])
        start = ussr ("Send Missiles to Cuba", usaResponse) 
                 <|> ("Do Nothing", nukesInTurkey)
        usaResponse = usa ("Do Nothing", nukesInTurkey <+> nukesInCuba)
                      <|> ("Blockade", ussrBlockadeCounter)
                      <|> ("Air Strike", ussrStrikeCounter)
        ussrBlockadeCounter = ussr ("Agree to Terms", noNukes) 
                               <|> ("Escalate", nuclearWar)
        ussrStrikeCounter = ussr ("Pull Out", nukesInTurkey)
                             <|> ("Escalate", nuclearWar)

khrushchev = "Khrushchev" :::
    play "Send Missiles to Cuba" `atFirstThen`
    do m <- his move `inThe` last turn
       play $ case m of "Blockade" -> "Agree to Terms"
                        "Air Strike" -> "Pull Out"

kennedy = "Kennedy" ::: mixed [(2, "Blockade"), (1, "Air Strike")]
