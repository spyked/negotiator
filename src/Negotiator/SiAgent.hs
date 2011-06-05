module Negotiator.SiAgent (SiOffer(..), mkSiAgent) where

import Control.Applicative ((<*>), pure)
import Negotiator.Negotiation
import Negotiator.Agent
import Negotiator.Util

data SiOffer = SiOffer {
    siCpu :: Int,
    siRam :: Int,
    siInterconnect :: Int,
    siDsp :: Int,
    siSensor :: Int
    } deriving (Eq, Ord)

type UtilityFunc = SiOffer -> Time -> Double

-- type class instances
instance Read SiOffer where
    readsPrec _ str = [(SiOffer c r i d s, rest) | 
        (c, r1) <- reads str,
        (r, r2) <- reads r1,
        (i, r3) <- reads r2,
        (d, r4) <- reads r3,
        (s,rest) <- reads r4]

instance Show SiOffer where
    show (SiOffer c r i d s) = 
        init . foldl (++) "" $ map ((++ " ") . show) [c,r,i,d,s]

instance Offer SiOffer where
    offerSet = [SiOffer c r i d s | 
                    c <- [0 .. lc],
                    r <- [0 .. lr],
                    i <- [0 .. li],
                    d <- [0 .. ld],
                    s <- [0 .. ls],
                    not (c == 0 || r == 0 || i == 0 || d == 0 || s == 0)
               ]
        where
        [lc,lr,li,ld,ls] = limits

-- offer limits: all offers are multiples of $100.
limits :: [Int]
limits = [16,14,25,15,6]

maxU :: Double
maxU = 10

qoThreshold :: Double
qoThreshold = 1

offerToV5 :: SiOffer -> Vector5
offerToV5 (SiOffer c r i d s) = Vector5 (
    fromIntegral c, fromIntegral r, fromIntegral i, fromIntegral d,
    fromIntegral s)

myU :: UtilityFunc
myU o t = v5dotp v5myWeights v5vals + 2 * (fromIntegral t)
    where
    v5myWeights = v5fromList [2/16,2/14,2/25,2/15,2/6]
    v5vals = offerToV5 o

oppU :: Double -> [Double] -> UtilityFunc
oppU discount interest o t = maxU - 
    sum' (zipWith (*) weights values) - discount * (fromIntegral t)
    where
    weights = zipWith (*) interest [1/16,1/14,1/25,1/15,1/6]
    values = map fromIntegral $ 
        [siCpu, siRam, siInterconnect, siDsp, siSensor] <*> pure o

oppU1 :: UtilityFunc
oppU1 o t = maxU - v5dotp weights values - 2 * (fromIntegral t)
    where
    weights = Vector5 (0.6 / 16,0.6 / 14,0.6 / 25,0.6 / 15,0.6 / 6)
    values = offerToV5 o

oppU2 :: UtilityFunc
oppU2 o t = maxU - v5dotp weights values - 0.8 * (fromIntegral t)
    where
    weights = Vector5 (0.1 / 16,0.5 / 14,0.4 / 25,1.0 / 15,1.0 / 6)
    values = offerToV5 o

oppU3 :: UtilityFunc
oppU3 o t = maxU - v5dotp weights values - 0.5 * (fromIntegral t)
    where
    weights = Vector5 (1.2 / 16,1.1 / 14,0.4 / 25,0.2 / 15,0.1 / 6)
    values = offerToV5 o

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

