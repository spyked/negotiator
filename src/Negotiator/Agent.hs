module Negotiator.Agent (Agent(..), QOAgent(..)) where

import System.Random
import Control.Applicative
import Control.Arrow
import Data.List
import Negotiator.Negotiation

-- Simple description of an Agent
data Offer o => Agent o = Agent {
    offerUtility :: o -> Time -> Double
    }

data Offer o => QOAgent o = QOAgent {
    offerSubSet :: [o],
    self :: Agent o,
    opponent :: Agent o,
    possibleAdversaries :: [Agent o]
    }

-- Agent negotiation strategy.
instance Negotiator QOAgent where
    genOffer = qo
    decide = agentDecide
    update = undefined

-- Qualitative offer
qo :: Offer o => QOAgent o -> Negotiation o -> IO o
qo (QOAgent subSet a b _) neg = return $ maximumBy cmp $ subSet
    where
    t = negTime neg
    cmp = curry $ uncurry compare . (minO *** minO)

    uA = offerUtility a
    uB = offerUtility b

    sumA = sum $ map (flip uA $ 0) subSet
    sumB = sum $ map (flip uB $ 0) subSet

    luA o = uA o 0 / sumA
    luB o = uB o 0 / sumB

    alpha o = uA o t
    beta o = (luA o + luB o) * uB o t
    minO = uncurry min . (alpha &&& beta)

-- fraction of the total offer set to include in search
setPercentage :: Double
setPercentage = 10

-- Decision strategy - incomplete
agentDecide :: Offer o => QOAgent o -> Negotiation o -> IO (Decision o)
agentDecide ag@(QOAgent _ a b _) neg 
    | negDecision neg == Accept = return Accept
    | negDecision neg == OptOut = return OptOut
    | otherwise = do
    let o_opp = case negDecision neg of
            Propose o -> o
            _ -> negSQO neg
    o_q <- qo ag neg
    if uA o_opp >= uA o_q
        then return Accept
        else return $ Propose o_q
    where
    t = negTime neg
    uA = flip (offerUtility a) t
    uB = flip (offerUtility b) t

