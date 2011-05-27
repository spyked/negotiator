module Negotiator.ExampleAgent where

import Control.Applicative
import Debug.Trace (trace)
import Negotiator.Negotiation
import Negotiator.Util
import Negotiator.Agent
import Negotiator.Plot

-- Simple example scenario - doesn't really do anything
data ExampleOffer = ExampleOffer {
    apples :: Int,
    pears :: Int,
    carrots :: Int
    } deriving (Show, Eq, Ord)

instance Read ExampleOffer where
    readsPrec _ s = [(ExampleOffer a p c, rest)]
        where
        l = map read $ words s
        rest = concat $ tail $ tail $ tail $ words s
        a = l !! 0
        p = l !! 1
        c = l !! 2

instance Offer ExampleOffer where
    offerSet = [ExampleOffer a p c | a <- [0..20], p <- [0..50], 
            c <- [0..60], not (a == 0 || p == 0 || c == 0) ]
    
exampleUtility :: ExampleOffer -> Time -> Double
exampleUtility o t = (sum $ zipWith (*) weights values) 
                     + 2 * (fromIntegral t)
    where
    values = map fromIntegral $ [apples, pears, carrots] <*> pure o
    weights = zipWith (/) [3.3,3.3,3.3] [20,50,60]

exampleUtility2 :: ExampleOffer -> Time -> Double
exampleUtility2 o t = 10 + 
    (sum $ zipWith (*) weights values) - 1 * (fromIntegral t)
    where
    values = map fromIntegral $ [apples, pears, carrots] <*> pure o
    weights = zipWith (/) [1.2,1.2,1.2] [-20,-50,-60]

exampleRank :: (ExampleOffer -> Time -> Double) ->
                ExampleOffer -> [ExampleOffer] -> Double
exampleRank util = rankOfferBy cmp
    where
    cmp o1 o2 = compare (util o1 0) (util o2 0)

exampleLuce :: (ExampleOffer -> Time -> Double) ->
                ExampleOffer -> [ExampleOffer] -> Double
exampleLuce util o subSet = trace "bla" $ util o 0 / sumUtils
    where
    sumUtils = sum $ map (flip util $ 0) subSet

exampleAgent :: QOAgent ExampleOffer
exampleAgent = QOAgent subset 1
                       (Agent "me" exampleUtility 
                              (exampleRank exampleUtility))
                       (Agent "him" exampleUtility2 
                              (exampleRank exampleUtility2))
                       []
    where
    clusterSize = let 
        s = floor $ 100 / setPercentage
        in if s == 0 then 1 else s
    subset = takePosDivBy clusterSize offerSet

-- fraction of the total offer set to include in search
setPercentage :: Double
setPercentage = 10

plot :: IO ()
plot = cleanFile "plot.txt"
    >> writeOptions "plot.txt"
    >> appendFile "plot.txt" "plot \"-\" with points pt 0.5 ps 1.5,\\\n"
    >> appendFile "plot.txt" "\"-\" with points pt 0.5 ps 1.5\n"
    >> writeFunc "plot.txt" (flip exampleUtility2 $ 0) offerSet
    >> writeFunc "plot.txt" (flip exampleUtility $ 0) offerSet
