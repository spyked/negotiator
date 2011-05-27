module Negotiator.Negotiation where

import Data.List
import System.Random
import Control.Monad.State

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
    }

-- Any decision that a Negotiator can make at a given moment.
data Offer o => Decision o = 
      Initiate
    | Accept
    | Propose o
    | EndSession
    | OptOut
    deriving (Show, Read, Eq)

moveAgent :: (Negotiator a, Offer o) => a o -> Negotiation o -> 
                                        IO (Decision o, a o)
moveAgent ag neg = do
    d' <- decide ag neg
    -- TODO: update
    return (d', ag)

-- Generic function definitions for negotiations
move :: (Negotiator a, Negotiator b, Offer o) =>
        Negotiation o -> a o -> b o -> IO (Negotiation o, a o, b o)
move neg@(Negotiation d sqo num t maxnum maxt) aa ab 
    | num `mod` 2 == 0 = do
    (d',aa') <- moveAgent aa neg
    let sqo' = updateSqo d'
    return (Negotiation d' sqo' (num' d') (t' d') maxnum maxt, aa', ab)

    | otherwise = do
    (d',ab') <- moveAgent ab neg
    let sqo' = updateSqo d'
    return (Negotiation d' sqo' (num' d') (t' d') maxnum maxt, aa, ab')

    where
    num' d' = if num + 1 == maxnum || d' == EndSession then 0 else num + 1
    t' d' = if num + 1 == maxnum || d' == EndSession then t + 1 else t
    updateSqo Accept = case d of
        Propose o -> o
        _ -> sqo
    updateSqo _ = sqo

negotiation :: (Negotiator a, Negotiator b, Offer o) => 
                StateT (Negotiation o, a o, b o) IO o
negotiation = do
    (neg, aa, ab) <- get
    
    if negTime neg == negDeadline neg || 
       negDecision neg `elem` [Accept, OptOut]
        then return $ negSQO neg
        else (lift $ move neg aa ab) >>= put >> negotiation

