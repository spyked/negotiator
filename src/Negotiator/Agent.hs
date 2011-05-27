module Negotiator.Agent (Agent(..), QOAgent(..)) where

import System.Random
import Control.Applicative
import Control.Arrow
import Data.List
import Negotiator.Negotiation

-- Simple model of an Agent
data Offer o => Agent o = Agent {
    agentID :: String,
    offerUtility :: o -> Time -> Double,
    offerRank :: o -> [o] -> Double
    }

instance Offer o => Show (Agent o) where
    show = agentID

-- QOAgent: the actual Negotiator
data Offer o => QOAgent o = QOAgent {
    offerSubSet :: [o],
    utilityThresh :: Double,
    self :: Agent o,
    opponent :: Agent o,
    possibleAdversaries :: [(Agent o, Double)] -- (agent, utility)
    }

-- Agent negotiation strategy.
instance Negotiator QOAgent where
    genOffer = qo
    decide = agentDecide
    update = agentUpdate

-- Qualitative offer
qo :: Offer o => QOAgent o -> Negotiation o -> IO o
qo (QOAgent subSet _ a b _) neg = return $ maximumBy cmp $ subSet
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
agentDecide ag@(QOAgent subSet thresh a b _) neg 
    | negDecision neg == Accept = return Accept
    | negDecision neg == OptOut = return OptOut
    | otherwise = do
    o_q <- qo ag neg

    doDecide o_q
    where
    t = negTime neg
    o_opp = case negDecision neg of
            Propose o -> o
            _ -> negSQO neg

    uA = flip (offerUtility a) t
    uB = flip (offerUtility b) t
    rA = flip (offerRank a) $ subSet `union` [o_opp]
    rB = flip (offerRank b) $ subSet `union` [o_opp]

    doDecide o_q
        | uA o_opp >= uA o_q = return Accept
        | abs (uB o_opp - uB o_q) <= thresh = return $ Propose o_q
        | otherwise = do -- speculate
        p <- randomRIO (0,1)
        let r = (rA o_opp + rB o_opp) * 0.5
        putStrLn $ "rank iz: " ++ show r
        return $ if p <= r then Accept else Propose o_q

luce :: Offer o => o -> [o] -> Agent o -> Double
luce o subSet a = uA o 0 / sumA
    where
    uA = offerUtility a
    sumA = sum $ map (flip uA $ 0) subSet

agentUpdate :: Offer o => QOAgent o -> Negotiation o -> IO (QOAgent o)
agentUpdate (QOAgent subSet thr a b ags) neg = return $
    QOAgent subSet thr a b' ags'
    where
    o = case negDecision neg of
        Propose off -> off
        _ -> negSQO neg
    lus = map (luce o subSet . fst) ags -- p(offer | type)
    po = sum $ zipWith (*) lus $ map snd ags  -- p(offer)

    bayesUpdate ((ag,pt),pot) = (ag,pot * pt / po)
    ags' = map bayesUpdate $ zip ags lus
    b' = fst $ maximumBy (\ (_, p1) (_, p2) -> compare p1 p2) ags'

