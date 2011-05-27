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
    } deriving (Show, Eq, Ord)

type UtilityFunc = SiOffer -> Time -> Double

-- type class instances
instance Read SiOffer where
    readsPrec _ str = [(SiOffer c r i d s, rest)]
        where
        l = map read $ words str
        rest = concat $ drop 5 $ words str
        c = l !! 0
        r = l !! 1
        i = l !! 2
        d = l !! 3
        s = l !! 4

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

funcs :: [SiOffer -> Int]
funcs = [siCpu, siRam, siInterconnect, siDsp, siSensor]

maxU :: Double
maxU = 10

qoThreshold :: Double
qoThreshold = 1

myU :: UtilityFunc
myU o t = sum (zipWith (*) weights values) + 2 * (fromIntegral t)
    where
    weights = zipWith (/) [2,2,2,2,2] $ map fromIntegral limits
    values = map fromIntegral $ funcs <*> pure o

oppU :: Double -> [Double] -> UtilityFunc
oppU discount interest o t = 
    maxU - sum (zipWith (*) weights values) - discount * (fromIntegral t)
    where
    weights = zipWith (/) interest $ map fromIntegral limits
    values = map fromIntegral $ funcs <*> pure o

genericRank :: UtilityFunc -> SiOffer -> [SiOffer] -> Double
genericRank util = rankOfferBy cmp
    where
    cmp o1 o2 = compare (util o1 0) (util o2 0)

rawAgentTypes :: [(Double, [Double])]
rawAgentTypes = [
    (1, [0.6,0.6,0.6,0.6,0.6]),
    (0.8, [0.1,0.5,0.4,1.0,1.0]),
    (1, [1.2,1.1,0.4,0.2,0.1])
    ]

mkAgentModel :: (Double, [Double]) -> String -> Agent SiOffer
mkAgentModel raw id = Agent id util $ genericRank util
    where
    util = uncurry oppU $ raw

siMe :: Agent SiOffer
siMe = Agent "me" myU $ genericRank myU

mkSiAgent :: QOAgent SiOffer
mkSiAgent = 
    QOAgent subset qoThreshold siMe (head agentModels) agentPs
    where
    numModels = length rawAgentTypes
    agentIDs = map (\ n -> "agent" ++ show n) [0 .. numModels - 1]
    agentModels = map (uncurry mkAgentModel) $ zip rawAgentTypes agentIDs
    p = 1 / fromIntegral numModels
    agentPs = zip agentModels $ take numModels $ iterate id p

    clusterSize = let 
        s = floor $ 100 / setPercentage
        in if s == 0 then 1 else s
    subset = takePosDivBy clusterSize offerSet

-- fraction of the total offer set to include in search
setPercentage :: Double
setPercentage = 10
