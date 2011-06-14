module Negotiator.SiOpponents where

import Control.Applicative ((<*>), pure)
import Negotiator.SiOffer
import Negotiator.Util

maxU :: Double
maxU = 10

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

