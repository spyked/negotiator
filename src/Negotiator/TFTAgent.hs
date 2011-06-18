module Negotiator.TFTAgent (TFTAgent(..), tftuA, tftuB) where

-- tit-for-tat agent
import System.Random (randomRIO)
import Negotiator.Negotiation

-- agent description
data Offer o => TFTAgent o = TFTAgent {
    tftComplement :: o -> IO o,
    tftRaise :: o -> IO o,
    tftRawUtility :: o -> Double,
    tftRawMax :: Double
    }

-- Negotiator instance
instance Negotiator TFTAgent where
    genOffer = tftOffer
    decide = tftDecide
    update = tftUpdate

-- self and opponent utilities
tftuA :: Offer o => TFTAgent o -> o -> Double
tftuA ag offer = tftRawUtility ag offer / tftRawMax ag

tftuB :: Offer o => TFTAgent o -> o -> Double
tftuB ag offer = (max - raw) / max
    where
    max = tftRawMax ag
    raw = tftRawUtility ag offer

-- Negotiator implementations
tftOffer :: Offer o => TFTAgent o -> Negotiation o -> IO o
tftOffer ag neg 
    | uA o < tftThresh = tftComplement ag o
    | otherwise = tftRaise ag o
    where
    uA = tftuA ag
    o = case negDecision neg of
        Propose smth -> smth
        _ -> negSQO neg

tftDecide :: Offer o => TFTAgent o -> Negotiation o -> IO (Decision o)
tftDecide ag neg 
    | uA o >= acceptanceThresh = return Accept
    | t == maxt - 1 = if uA o >= tftThresh 
        then return Accept
        else return OptOut
    | negDecision neg == OptOut = return OptOut
    | negDecision neg == EndSession = return EndSession
    | uA o < tftThresh = tftOffer ag neg >>= return . Propose
    | otherwise = rollDice
    where
    -- variables
    t = negTime neg
    maxt = negDeadline neg
    uA = tftuA ag
    o = case negDecision neg of
        Propose smth -> smth
        _ -> negSQO neg

    -- dice roll: a 20-sided die
    rollDice = do
        p <- randomRIO (0,1) :: IO Double
        if p < 0.05
            then return $ Accept
            else tftOffer ag neg >>= return . Propose

tftUpdate :: Offer o => TFTAgent o -> Negotiation o -> IO (TFTAgent o)
tftUpdate ag _ = return ag

-- some constants
tftThresh :: Double
tftThresh = 0.4

acceptanceThresh :: Double
acceptanceThresh = 0.95
