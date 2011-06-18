module Negotiator.SiTFT (TFTAgent(..),mkSiTFT) where

-- tit-for-tat implementation for SiOffer
import System.Random (randomRIO)
import Negotiator.TFTAgent
import Negotiator.SiOffer
import Negotiator.Util (applyOnIndex)

mkSiTFT :: TFTAgent SiOffer
mkSiTFT = TFTAgent {
    tftComplement = siComplement,
    tftRaise = siRaise,
    tftRawUtility = siSum,
    tftRawMax = 76
    }

-- complementary offer
siComplement :: SiOffer -> IO SiOffer
siComplement (SiOffer c r i d s) = return $ SiOffer c' r' i' d' s'
    where
    [c',r',i',d',s'] = zipWith (-) [17,15,26,16,7] [c,r,i,d,s]

-- raised offer
siRaise :: SiOffer -> IO SiOffer
siRaise = siRaiseTimes 10

-- raise offer n times
siRaiseTimes :: Int -> SiOffer -> IO SiOffer
siRaiseTimes n o 
    | n == 0 = return o
    | n > 0 = siRaiseOnce o >>= siRaiseTimes (n - 1)
    | otherwise = error "negative number"

-- raise once, randomly. or attempt to
siRaiseOnce :: SiOffer -> IO SiOffer
siRaiseOnce o = do
    i <- randomRIO (0,4) :: IO Int
    if o == maxSi -- assuming offers are <= maxSi
        then return o
        else return $ tryRaise i o

tryRaise :: Int -> SiOffer -> SiOffer
tryRaise index (SiOffer c r i d s) = SiOffer c' r' i' d' s'
    where
    [c',r',i',d',s'] = doTryRaise index $ zip limits [c,r,i,d,s]
    raiseNum (max,n) = if n >= max then (max, max) else (max, n + 1)
    doTryRaise n tups = let
        result = applyOnIndex n raiseNum tups
        tryNext = (n + 1) `mod` 5
        in if result == tups 
           then doTryRaise tryNext tups else map snd result
    

-- raw utility measure
siSum :: SiOffer -> Double
siSum (SiOffer c r i d s) = fromIntegral $ sum [c,r,i,d,s]

