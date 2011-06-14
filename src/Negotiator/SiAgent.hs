module Negotiator.SiAgent (mkSiAgent) where

import Negotiator.Negotiation
import Negotiator.Agent
import Negotiator.SiOffer
import Negotiator.SiOpponents
import Negotiator.Util

qoThreshold :: Double
qoThreshold = 1

genericRank :: UtilityFunc -> SiOffer -> [SiOffer] -> Double
genericRank util = rankOfferBy cmp
    where
    cmp o1 o2 = compare (util o1 0) (util o2 0)

rawAgentTypes :: [UtilityFunc]
rawAgentTypes = [
    oppU1,
    oppU2,
    oppU3
    ]

mkAgentModel :: UtilityFunc -> String -> Agent SiOffer
mkAgentModel util id = Agent id util $ genericRank util

siMe :: Agent SiOffer
siMe = Agent "me" myU $ genericRank myU

mkSiAgent :: QOAgent SiOffer
mkSiAgent = 
    QOAgent subset qoThreshold siMe (head agentModels) agentPs
    where
    numModels = length rawAgentTypes
    agentIDs = map (\ n -> "agent" ++ show n) [0 .. numModels - 1]
    agentModels = zipWith mkAgentModel rawAgentTypes agentIDs

    p = 1 / fromIntegral numModels
    agentPs = zip agentModels $ take numModels $ iterate id p

    clusterSize = let 
        s = floor $ 100 / setPercentage
        in if s == 0 then 1 else s
    subset = takePosDivBy clusterSize offerSet

-- fraction of the total offer set to include in search
setPercentage :: Double
setPercentage = 10

