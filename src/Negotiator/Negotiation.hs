module Negotiator.Negotiation where

import Data.List
import System.Random

{-
    Offer type class definition.
-}
class (Read o, Show o, Eq o) => Offer o where
    offerSet :: [o]

rankOfferBy :: Offer o => (o -> o -> Ordering) -> o -> [o] -> Double
rankOfferBy cmp o set = ind / card
    where
    sortedSet = sortBy cmp set
    ind = fromIntegral . (+ 1) . maybe err id $ elemIndex o sortedSet
    card = fromIntegral $ length set
    err = error "Offer not in set."
   
{-
    Negotiator agent type class definition.

    Any agent that can generate and decide upon offers is considered
    to be a Negotiator.
-}
class Negotiator a where
    genOffer :: Offer o => a o -> Negotiation o -> IO o
    decide :: Offer o => a o -> Negotiation o -> IO (Decision o)
    update :: Offer o => a o -> Negotiation o -> IO (a o)

{-
    Type definitions.
-}
type Time = Int

-- The variables defining a negotiation.
data Offer o => Negotiation o = Negotiation {
    negDecision :: Decision o,
    negSQO :: o,
    negNumber :: Int,
    negTime :: Time,
    negMaxNumber :: Int,
    negDeadline :: Time
    } deriving (Show, Read)

-- Any decision that a Negotiator can make at a given moment.
data Offer o => Decision o = 
      Initiate
    | Accept
    | Propose o
    | EndSession
    | OptOut
    deriving (Show, Read, Eq)

data Offer o => Outcome o = Agree o | Disagree deriving Show

