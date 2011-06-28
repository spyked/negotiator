module Negotiator.SiOffer where

import Negotiator.Negotiation
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

-- offer limits: all offers are multiples of some sum. or not.
limits :: [Int]
limits = [16,14,25,15,6]

maxSi :: SiOffer
maxSi = SiOffer 16 14 25 15 6

-- utility functions
offerToV5 :: SiOffer -> Vector5
offerToV5 (SiOffer c r i d s) = Vector5 (
    fromIntegral c, fromIntegral r, fromIntegral i, fromIntegral d,
    fromIntegral s)

