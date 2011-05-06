module ExampleAgent where

import Control.Applicative
import Negotiation
import Agent

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
    offerSet = [ExampleOffer a p c | a <- [0..100], p <- [0..50], 
            c <- [0..60], not (a == 0 && p == 0 && c == 0) ]
    
exampleUtility :: ExampleOffer -> Time -> Double
exampleUtility o t = (sum $ zipWith (*) weights values) 
                     - 2 * (fromIntegral t)
    where
    values = map fromIntegral $ [apples, pears, carrots] <*> pure o
    weights = zipWith (/) [2.4,6.6,1] [200,50,60]

exampleUtility2 :: ExampleOffer -> Time -> Double
exampleUtility2 o t = 10 + 
    (sum $ zipWith (*) weights values) - 2 * (fromIntegral t)
    where
    values = map fromIntegral $ [apples, pears, carrots] <*> pure o
    weights = zipWith (/) [1.3,5,3.7] [-200,-50,-60]

exampleRank :: ExampleOffer -> Double
exampleRank = rankOfferBy cmp
    where
    cmp o1 o2 = compare (exampleUtility o1 0) (exampleUtility o2 0)

exampleRank2 :: ExampleOffer -> Double
exampleRank2 = rankOfferBy cmp
    where
    cmp o1 o2 = compare (exampleUtility2 o1 0) (exampleUtility2 o2 0)

exampleAgent :: QOAgent ExampleOffer
exampleAgent = QOAgent (Agent exampleRank exampleUtility)
                       (Agent exampleRank2 exampleUtility2)
                       []

