module Negotiator.SiOpponents (rawAgentTypes, rawAgentWeights, myU) where

import Control.Applicative ((<*>), pure)
import Negotiator.SiOffer
import Negotiator.Util

-- max utility
maxU :: Double
maxU = 76

rawAgentTypes :: [UtilityFunc]
rawAgentTypes = [
    a0U,
    a1U,
    a2U,
    a3U,
    a4U,
    a5U
    ]

rawAgentWeights :: [Vector5]
rawAgentWeights = [
    a0Weights,
    a1Weights,
    a2Weights,
    a3Weights,
    a4Weights
    ]

-- agent utility
myU :: UtilityFunc
myU o t = v5dotp v5myWeights v5vals + 2 * (fromIntegral t)
    where
    v5myWeights = Vector5 (1,1,1,1,1)
    v5vals = offerToV5 o

-- generic opponent utility (deprecated)
oppU :: Double -> [Double] -> UtilityFunc
oppU discount interest o t = maxU - 
    sum' (zipWith (*) weights values) - discount * (fromIntegral t)
    where
    weights = zipWith (*) interest [1/16,1/14,1/25,1/15,1/6]
    values = map fromIntegral $ 
        [siCpu, siRam, siInterconnect, siDsp, siSensor] <*> pure o

-- some opponent utilities (deprecated)
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

-- some more opponent utilities
a0U :: UtilityFunc
a0U o t = maxU - v5dotp a0Weights (offerToV5 o) + 1.9 * (fromIntegral t)

a1U :: UtilityFunc
a1U o t = maxU - v5dotp a1Weights (offerToV5 o) + 1.9 * (fromIntegral t)

a2U :: UtilityFunc
a2U o t = maxU - v5dotp a2Weights (offerToV5 o) + 1.9 * (fromIntegral t)

a3U :: UtilityFunc
a3U o t = maxU - v5dotp a3Weights (offerToV5 o) + 1.9 * (fromIntegral t)

a4U :: UtilityFunc
a4U o t = maxU - v5dotp a4Weights (offerToV5 o) + 1.9 * (fromIntegral t)

a5U :: UtilityFunc
a5U o t = maxU - v5dotp a5Weights (offerToV5 o) + 1.9 * (fromIntegral t)

a0Weights = Vector5 (1 / 16, 1 / 14, 23 / 25, 7 / 15, 3 / 6)
a1Weights = Vector5 (14 / 16, 11 / 14, 14 / 25, 0.5 / 15, 0.1 / 6)
a2Weights = Vector5 (7 / 16, 6 / 14, 1.5 / 25, 13.6 / 15, 4.5 / 6)
a3Weights = Vector5 (2 / 16, 3 / 14, 5 / 25, 12 / 15, 5.2 / 6)
a4Weights = Vector5 (13.5 / 16, 12 / 14, 0.3 / 25, 13.2 / 15, 4 / 6)
a5Weights = Vector5 (8 / 16, 7 / 14, 12.5 / 25, 7.5 / 15, 3 / 6)
