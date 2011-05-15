module HumanAgent (HumanAgent(..)) where

import System.IO (hFlush, stdout)
import Negotiation

data Offer o => HumanAgent o = HumanAgent

instance Negotiator HumanAgent where
    genOffer = readOffer
    decide = readDecision
    update me _ = return me

readOffer :: Offer o => HumanAgent o -> Negotiation o -> IO o
readOffer _ _ = getLine >>= return . read

readDecision :: Offer o => HumanAgent o -> Negotiation o -> IO (Decision o)
readDecision _ neg = do
    putStrLn $ "t = " ++ show t ++ ", offer count = " ++ show no
    putStrLn $ "Opponent's decision was: " ++ show (negDecision neg)
    putStr "Your decision is: "
    hFlush stdout
    str <- getLine
    return $ read str
    where
    no = negNumber neg
    t = negTime neg
