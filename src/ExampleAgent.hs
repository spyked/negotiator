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

instance Offer ExampleOffer where
    offerSet = [ExampleOffer a p c | a <- [0..200], p <- [0..50], c <- [0..60],
            not (a == 0 && p == 0 && c == 0) ]
    
exampleUtility :: ExampleOffer -> Time -> Double
exampleUtility o t = (sum $ zipWith (*) weights values) - 2 * (fromIntegral t)
    where
    values = map fromIntegral $ [apples, pears, carrots] <*> pure o
    weights = zipWith (/) [2.4,6.6,1] [200,50,60]

exampleUtility2 :: ExampleOffer -> Time -> Double
exampleUtility2 o t = 10 + 
    (sum $ zipWith (*) weights values) - 2 * (fromIntegral t)
    where
    values = map fromIntegral $ [apples, pears, carrots] <*> pure o
    weights = zipWith (/) [1.7,6,2.3] [-200,-50,-60]

exampleRank :: ExampleOffer -> Double
exampleRank = rankOfferBy cmp
    where
    cmp o1 o2 = compare (exampleUtility o1 0) (exampleUtility o2 0)

exampleRank2 :: ExampleOffer -> Double
exampleRank2 = rankOfferBy cmp
    where
    cmp o1 o2 = compare (exampleUtility2 o1 0) (exampleUtility2 o2 0)

exampleAgent :: Agent ExampleOffer
exampleAgent = Agent exampleRank exampleUtility

exampleAgent2 :: Agent ExampleOffer
exampleAgent2 = Agent exampleRank2 exampleUtility2

