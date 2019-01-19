{-# LANGUAGE TypeFamilies #-}

{- |

A linearized implementation of the Dollar Auction:
  <http://en.wikipedia.org/wiki/Dollar_auction>

Example experiments from GHCi:

>>> execGame (DollarAuction 4) [penny, nickel, quarter, dim] (once >> printTranscript)
>>> execGame (DollarAuction 4) [penny, nickel, quarter, dim] (times 100 >> printScore)

(The included strategies are not good ones, just examples...)

-}
module Hagl.Examples.Auction where

import Hagl


--
-- * Bids
--

-- | A bid is a value associated with a particular player.
type Bid = (PlayerID, Cents)

-- | The value of a bid.  1 cent = .01 dollar.
type Cents = Int

-- | A move is either a bid, or a pass if the player chooses not to bid.
data BidOrPass =
    Bid Cents -- ^ Bid the given number of cents.
  | Pass      -- ^ Do not bid.
  deriving (Eq, Show)

-- | The player associated with a bid.
bidder :: Bid -> PlayerID
bidder = fst

-- | The value of a bid.
bidValue :: Bid -> Cents
bidValue = snd


--
-- * Game representation
--

-- | A dollar auction game for the given number of players.
data DollarAuction = DollarAuction Int

-- | The current game state is the current two highest bids.
type HighBids = (Bid,Bid)

-- | The initial state.
initBids = ((0,0),(0,0))

-- | Return the value of the highest bid.
highBid :: HighBids -> Cents
highBid = snd . fst

-- | Return the ID of the player with the highest bid.
highBidder :: HighBids -> PlayerID
highBidder = fst . fst

-- | Return the value of the next highest bid.
nextBid :: HighBids -> Cents
nextBid = snd . snd

-- | Return the ID of the player with the next highest bid.
nextBidder :: HighBids -> PlayerID
nextBidder = fst . snd


-- Game instance.
instance Game DollarAuction where
  type TreeType DollarAuction = Continuous
  type Move  DollarAuction = BidOrPass
  type State DollarAuction = HighBids

  gameTree (DollarAuction n) = takeTurnsC n end up pay 1 initBids
    where
      -- Is the auction over?
      end p s = p == highBidder s
      -- Update the high bids.
      up  p ((hp,hb),_) (Bid b) | b > hb = ((p,b),(hp,hb))
      up  _ s _ = s
      -- Generate a payoff for the end of the game.
      pay _ ((hp,hb),(np,nb)) = ByPlayer [fromIntegral (val p) | p <- [1..n]]
        where val p | p == hp   = 100 - hb
                    | p == np   =   0 - nb
                    | otherwise =   0


--
-- * Some example players
--

-- | A player that increases the bid by 1 cent with 80% probability.
penny :: Player DollarAuction
penny = "Penny" ::: do 
    h <- gameState
    mixed [(4, Bid (highBid h + 1)), (1, Pass)]

-- | A player that increseases the bid by 5 cents as long as the highest bid
--   is below 50 cents.
nickel :: Player DollarAuction
nickel = "Nickel" ::: do
    h <- gameState
    return $ if highBid h < 50 then Bid (highBid h + 5) else Pass

-- | A player that increases the bid by 25 cents with 25% probability.
quarter :: Player DollarAuction
quarter = "Quarter" ::: do
    h <- gameState
    mixed [(1, Bid (highBid h + 25)), (3, Pass)]

-- | A player that increases the bid by 1 cent with a probability
--   determined by a diminishing returns function.
dim :: Player DollarAuction
dim = "Diminishing Returns" ::: do
    s <- gameState
    let high = highBid s
    mixed [(high, Pass), (100-high, Bid (high+1))]
