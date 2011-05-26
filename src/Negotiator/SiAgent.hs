module Negotiator.SiAgent (SiOffer(..)) where

import Negotiator.Negotiation
import Negotiator.Util

data SiOffer = SiOffer {
    siCpu :: Int,
    siRam :: Int,
    siInterconnect :: Int,
    siDsp :: Int,
    siSensor :: Int
    } deriving (Show, Eq, Ord)

-- offer limits: all offers are multiples of $100.
limits :: [Int]
limits = [10,20,25,15,6]


