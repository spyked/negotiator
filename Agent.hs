module Agent (Agent(..)) where

import System.Random
import Control.Applicative
import Control.Arrow
import Data.List
import Negotiation

-- Simple description of an Agent
data Offer o => Agent o = Agent {
    offerRank :: o -> Double,
    offerUtility :: o -> Time -> Double
    }

-- Agent negotiation strategy.
instance Negotiator Agent where
    genOffer = qo
    decide = agentDecide

-- Qualitative offer
qo :: Offer o => Negotiation Agent o -> o
qo (Negotiation _ _ t a b) = maximumBy cmp $ subSet
    where
    set = offerSet
    clusterSize = let 
        f = 5
        s = floor $ 100/f
        in if s == 0 then 1 else s
    subSet = takePosDivBy clusterSize set
    cmp = curry $ uncurry compare . (minO *** minO)

    uA = offerUtility a
    uB = offerUtility b

    sumA = sum $ map (flip uA $ t) subSet
    sumB = sum $ map (flip uB $ t) subSet

    luA o = uA o t / sumA
    luB o = uB o t / sumB

    alpha o = uA o t
    beta o = (luA o + luB o) * uB o t
    minO = uncurry min . (alpha &&& beta)

-- Decision strategy - incomplete
agentDecide :: (Offer o, RandomGen g) => g -> Negotiation Agent o -> Decision o
agentDecide gen ng@(Negotiation o_opp _ t a b) 
    | uA o_opp >= uA o_q = Accept
    | otherwise = Propose o_q
    where
    o_q = qo ng

    uA = flip (offerUtility a) t
    uB = flip (offerUtility b) t

takePosDivBy :: Int -> [a] -> [a]
takePosDivBy = takePosDivides' 0
    where
    takePosDivides' _ _ [] = []
    takePosDivides' pos n (x : xs)
        | pos `mod` n == 0 = x : next
        | otherwise = next
        where next = takePosDivides' (pos + 1) n xs


