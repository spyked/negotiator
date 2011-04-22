module Negotiation where

import Data.List
import System.Random

{-
    Offer type class definition.
-}
class Eq o => Offer o where
    offerSet :: [o]

    -- generic implementations
    rankOfferBy :: (o -> o -> Ordering) -> o -> Double
    rankOfferBy cmp o = ordr / len
        where
        sortedOffers = sortBy cmp offerSet
        ordr = fromIntegral $ orderOffer o sortedOffers
        len = fromIntegral $ length sortedOffers

    orderOffer :: o -> [o] -> Int
    orderOffer = orderOffer' 1
        where
        orderOffer' _ _ [] = error "orderOffer: Offer not found"
        orderOffer' n o (o' : os) = if o == o' 
            then n else orderOffer' (n + 1) o os

{-
    Negotiator agent type class definition.

    Any agent that can generate and decide upon offers is considered
    to be a Negotiator.
-}
class Negotiator a where
    genOffer :: Offer o => Negotiation a o -> o
    decide :: (Offer o, RandomGen g) => g -> Negotiation a o -> Decision o

{-
    Type definitions.
-}
type Time = Int

-- The variables defining a negotiation.
data (Negotiator a, Offer o) => Negotiation a o = Negotiation {
    negOffer :: o,
    negNumber :: Int,
    negTime :: Time,
    negAgentA :: a o,
    negAgentB :: a o
    }

-- Any decision that a Negotiator can make at a given moment.
data Offer o => Decision o = 
      Accept
    | Propose o
    | EndSession
    | OptOut
    deriving Show

