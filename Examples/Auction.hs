{-# LANGUAGE TypeFamilies #-}

{-

An implementation of the Dollar Auction:
  http://en.wikipedia.org/wiki/Dollar_auction

Example experiments from GHCi:
> execGame Auction [p1, p2, p3, dim] (once >> printTranscript)
> execGame Auction [p1, p2, p3, dim] (times 100 >> printScore)

(The included strategies are not necessarily good ones and are
merely included as examples...)

-}
module Examples.Auction where

import Hagl

type Cents = Int

data Bid = Bid Cents | Pass deriving (Eq, Show)

data Auction = Auction

data HighBids = HighBids {
  highBidder :: PlayerIx, highBid :: Cents,
  nextBidder :: PlayerIx, nextBid :: Cents
}

instance Game Auction where
  type Move Auction = Bid
  type State Auction = HighBids
  initState _ = HighBids 0 0 0 0
  runGame = numPlayers >>= bid 1

bid p n = do
    h <- gameState
    if highBidder h == p then return (pay n h) else do
    b <- decide p
    update h p b
    bid (nextPlayer n p) n
  where update h p (Bid d) 
          | d < highBid h = fail "Bid is too low!"
          | otherwise = putGameState (HighBids p d (highBidder h) (highBid h))
        update h p Pass = return ()

pay :: Int -> HighBids -> Payoff
pay n h = ByPlayer [fromIntegral (val p) | p <- [1..n]]
  where val p | p == highBidder h = 100 - highBid h
              | p == nextBidder h =   0 - nextBid h
              | otherwise         =   0

-- Players

p1 :: Player Auction
p1 = "Penny" ::: do 
    h <- gameState
    mixed [(4, Bid (highBid h + 1)), (1, Pass)]

p2 :: Player Auction
p2 = "Nickel" ::: do
    h <- gameState
    return $ if highBid h < 50 then Bid (highBid h + 5) else Pass

p3 :: Player Auction
p3 = "Quarter" ::: do
    h <- gameState
    mixed [(1, Bid (highBid h + 25)), (3, Pass)]

dim :: Player Auction
dim = "Diminishing returns" ::: do
    s <- gameState
    let high = highBid s
    mixed [(high, Pass), (100-high, Bid (high+1))]
