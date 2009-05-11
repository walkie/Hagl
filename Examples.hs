{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}

import Data.List hiding (last)
import Hagl

import Control.Monad.State
import Prelude hiding (last,print)

------------------------
-- Prisoner's Dilemma --
------------------------

-- Game definition
data Dilemma = Cooperate | Defect deriving (Show, Eq)

pd = matrix [Cooperate, Defect] [[2,2],[0,3],[3,0],[1,1]]

-- Some simple players.
fink = "Fink" ::: pure Defect
mum = "Mum" ::: pure Cooperate
alt = "Alternator" ::: periodic [Cooperate, Defect]
dc = "(DC)*" ::: periodic [Cooperate, Defect]
ccd = "(CCD)*" ::: periodic [Cooperate, Cooperate, Defect]
randy = "Randy" ::: randomly
rr = "Russian Roulette" ::: mixed [(5, Cooperate), (1, Defect)]

-- The famous Tit-for-Tat.
tft = "Tit for Tat" ::: play Cooperate `atFirstThen` his (last game's move)
--titForTat = "Tit for Tat" ::: [play Cooperate] `thereafter` his (prev move)
--titForTat = "Tit for Tat" `plays` (initially (play Cooperate) `thereafter` his (prev move))
--titForTat = "Tit for Tat" `plays` (Cooperate `initiallyThen` his (prev move))

axelrod :: [Player Dilemma] -> IO ()
axelrod ps = roundRobin pd ps (times 100 >> printScore)

stately = Player "Stately Alternator" Cooperate $
  do m <- get
     put $ if m == Cooperate then Defect else Cooperate
     return m

mod3 = Player "Mod3 Cooperator" 0 $
  do i <- get
     put (i+1)
     return $ if i `mod` 3 == 0 then Cooperate else Defect
     
-- Suspicious Tit-for-Tat (like Tit-for-Tat but defect on first move)
suspicious = "Suspicious Tit-for-Tat" ::: play Defect `atFirstThen` his (last game's move)

-- Tit-for-Tat that only defects after two defects in a row.
titForTwoTats = "Tit-for-Two-Tats" :::
    do ms <- his `each` lastn 2 games' move
       return $ if ms == [Defect, Defect] then Defect else Cooperate

-- The Grim Trigger: Cooperates until opponent defects, then defects forever.
grim = "Grim Trigger" :::
    do ms <- his `each` every game's move
       return $ if Defect `elem` ms then Defect else Cooperate

{-
grim' = Player "Stately Grim Trigger" False
    (do m <- his (prev move)
        triggered <- get
        put (triggered || m == Defect)
        if triggered then play Defect else play Cooperate)

statelyGrim = Player "Grim Trigger" False $ get >>= trig
  where trig True = return Defect
        trig False = do m <- his (prev move)
                        if m == Cooperate then return Cooperate
                                          else put True >> return Defect
-}

grim' = Player "Stately Grim" False $ 
  play Cooperate `atFirstThen`
  do m <- his (last game's move)
     triggered <- update (|| m == Defect)
     if triggered then play Defect else play Cooperate

{-
-- If last move resulted in a "big" payoff, do it again, otherwise switch.
pavlov = "Pavlov" `plays`
    (randomly `atFirstThen`
     do p <- my (prev payoff)
        m <- my (prev move)
        return $ if p > 1 then m else
          if m == Cooperate then Defect else Cooperate)
-}

-- Made-up strategy: Pick randomlyly until we have a lead, then
-- preserve it by repeatedly choosing Defect.
preserver = "Preserver" :::
    randomly `atFirstThen`
    do me <- my score
       he <- his score
       if me > he then return Defect else randomly

a -! f = (liftM2 f) a
(!-) = ($)

(?) :: Monad m => m Bool -> (m a, m a) -> m a
mb ? (t,f) = mb >>= \b -> if b then t else f

preserver2 = "Preserver" :::
    randomly `atFirstThen`
    (my score -! (>) !- his score ? (return Defect, randomly))

-- Running from GHCi:
-- > runGame pd [titForTat, pavlov] (times 10 >> printTranscript >> printScores)
-- > roundRobin pd [titForTat, titForTwoTats, grim, suspicious, pavlov] (times 100 >> printScore)

-------------------------
-- Rock Paper Scissors --
-------------------------

data RPS = Rock | Paper | Scissors deriving (Enum, Eq, Show)

rps = zerosum [Rock .. Scissors] [0,-1, 1,
                                  1, 0,-1,
                                 -1, 1, 0]

-- Some simple players
rocky = "Stalone" ::: pure Rock
rotate = "RPS" ::: periodic [Rock, Paper, Scissors]
tricky = "Tricky" ::: [play Rock, play Paper] `thereafter` play Scissors
-- can reuse randy from above!

-- If last move resulted in a "big" payoff, do it again, otherwise switch.
pavlov = "Pavlov" :::
    randomly `atFirstThen`
    do p <- my (last game's payoff)
       m <- my (last game's move)
       if p > 0 then return m else randomly

-- Play the move that will beat the move the opponent has played most.
frequency = "Huckleberry" :::
    do ms <- his `each` every game's move
       let r = length $ filter (Rock ==) ms
           p = length $ filter (Paper ==) ms
           s = length $ filter (Scissors ==) ms
           x = maximum [r,p,s]
        in return $ if x == r then Paper else 
                    if x == p then Scissors
                              else Rock

--------------------------
-- Cuban Missile Crisis --
--------------------------

crisis = extensive start
  where ussr = player 1
        usa  = player 2
        nukesInTurkey = Payoff [  -2,   1]
        nukesInCuba   = Payoff [   1,  -2]
        nuclearWar    = Payoff [-100,-100]
        noNukes       = Payoff [   0,   0]
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

-- To run, e.g.
-- > runGame crisis [khrushchev, kennedy] (once >> printTranscript)

{-
nuclearWar    = Payoff [-100,-100]
nukesInCuba   = Payoff [   1,  -1]
nukesInTurkey = Payoff [  -1,   1]
usaLooksGood  = Payoff [   0,   1]
ussrLooksGood = Payoff [   1,   0]

start = ussr ("Send Missiles to Cuba", usaResponse) 
         <|> ("Do Nothing", nukesInTurkey)

usaResponse = usa ("Do Nothing", nukesInTurkey <+> nukesInCuba <+> ussrLooksGood)
              <|> ("Blockade", ussrBlockadeCounter)
              <|> ("Invade", ussrInvasionCounter)

ussrBlockadeCounter = ussr ("Agree to Terms", usaLooksGood) 
                       <|> ("Escalate", nuclearWar)

ussrInvasionCounter = ussr ("Pull Out", nukesInTurkey <+> usaLooksGood) 
                       <|> ("Escalate", nuclearWar)

crisis = extensive start
-}

------------------------
-- Two Person Auction --
------------------------

auction v = Game 2 Perfect $ bid 1 0
  where bid p b = Decision p [(0, win (other p) b), (b+1, bid (other p) (b+1))]
        other 1 = 2
        other 2 = 1
        win 1 b = Payoff [v - fromInteger b, 0]
        win 2 b = Payoff [0, v - fromInteger b]
        
------------------
-- Dice Rolling --
------------------

die = Game 1 Perfect $ Chance [(1, Payoff [a]) | a <- [1..6]]
roll n = runGame die ["Total" ::: return ()] (times n >> printScore)

-----------------
-- Tic Tac Toe --
-----------------

data Square = X | O | Empty deriving (Eq, Show)
type Board = [Square]
type Move = Int

mark 1 = X
mark 2 = O

empty :: Board -> [Int]
empty = elemIndices Empty

end :: Board -> PlayerIx -> Bool
end b p = win b p || null (empty b)

avail :: Board -> PlayerIx -> [Move]
avail b _ = empty b

exec :: Board -> PlayerIx -> Move -> Board
exec b p m = take m b ++ mark p : drop (m+1) b

pay :: Board -> PlayerIx -> [Float]
pay b p | win b p = winner 2 p
        | otherwise = tie 2

win :: Board -> PlayerIx -> Bool
win b p = let h = chunk 3 b
              v = transpose h
              d = map (map (b !!)) [[0,4,8],[2,4,6]]
          in or $ map (and . map (mark p ==)) (h ++ v ++ d)

ticTacToe = takeTurns 2 end avail exec pay (replicate 9 Empty)

-- A Minimax Player
mm = "Minimaxi" ::: minimax

--------------------
-- The Match Game --   -- Try to force your opponent to take the last match.
--------------------

-- Create a new match game:
--   * Number of start matches.
--   * List of moves (# of matches to take).
-- e.g. matches 15 [1,2,3] -- 15 matches, can take 1-3 each turn
matches n ms = takeTurns 2 end moves exec pay n
  where end n _ = n <= 0
        moves n _ = [m | m <- ms, n-m >= 0]
        exec n _ m = n-m
        pay _ = winner 2

-- Problem: No way to get state from the strategy of a state game!!!

matchman = "Match Man" :::
    do n <- numMatches
       g <- game
       let ms = availMoves (tree g)
        in maybe randomly play (find (\m -> gcd (n-m) 4 == 4) ms)

numMatches = do (Perfect loc) <- location
                return (count loc)
  where count (Decision _ ((m,b):_)) = m + count b
        count (Payoff _) = 0
