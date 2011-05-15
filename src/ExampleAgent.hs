module ExampleAgent where

import Control.Applicative
import Negotiation
import Agent
import Plot

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

exampleAgent :: QOAgent ExampleOffer
exampleAgent = QOAgent (Agent exampleUtility)
                       (Agent exampleUtility2)
                       []

plot :: IO ()
plot = cleanFile "plot.txt"
    >> writeOptions "plot.txt"
    >> appendFile "plot.txt" "plot \"-\" with points pt 0.5 ps 1.5,\\\n"
    >> appendFile "plot.txt" "\"-\" with points pt 0.5 ps 1.5\n"
    >> writeFunc "plot.txt" (flip exampleUtility2 $ 0) offerSet
    >> writeFunc "plot.txt" (flip exampleUtility $ 0) offerSet
