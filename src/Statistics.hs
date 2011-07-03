module Statistics where

import System.FilePath ((</>))
import System.Directory (getDirectoryContents)
import Negotiator.Negotiation
import Negotiator.SiOffer (SiOffer)
import WebBargain.Logger
import WebBargain.State (wsNegotiation)

data WhatAgent = Computer | Human deriving (Show, Eq)

-- conversion from Int to WhatAgent and back
waToInt :: WhatAgent -> Int
waToInt Computer = 1
waToInt Human = 0

intToWa :: Int -> WhatAgent
intToWa i = if i `mod` 2 == 0 then Human else Computer

-- read all logs from a given directory
readLogsFromDir :: FilePath -> IO [Log]
readLogsFromDir path = getDirectoryContents path 
    >>= return . filterDot
    >>= mapM (readLog . (path </>))

filterDot :: [FilePath] -> [FilePath]
filterDot = filter (\ s -> s /= "." && s /= "..")

-- filter negotiations by decision
negsEndedWith :: Decision SiOffer -> [Log] -> [Log]
negsEndedWith dec logs = filter (logEndsWith dec) logs
    where
    logEndsWith dec log = 
        (negDecision . wsNegotiation . logeWebState) (last log) == dec

-- how many rounds?
negEndedAfter :: Log -> Int
negEndedAfter log = negTime . wsNegotiation . logeWebState $ last log

-- mean number of rounds
meanNumOfRounds :: [Log] -> Double
meanNumOfRounds logs = fromIntegral roundSum / fromIntegral (length logs)
    where
    roundSum = sum $ map negEndedAfter logs

-- who ended negotiation?
whoEndedNeg :: Log -> WhatAgent
whoEndedNeg = intToWa . length

getEndedBy :: WhatAgent -> [Log] -> [Log]
getEndedBy who = filter (\ log -> whoEndedNeg log == who)

-- negs that ended with SQO
sqoNegs :: [Log] -> [Log]
sqoNegs logs = let logs' = negsEndedWith Accept logs
    in filter (\ log -> length log == 13) logs'

-- mean utilies
meanUtility :: WhatAgent -> [Log] -> Double
meanUtility who logs = sum endUtils / fromIntegral (length logs)
    where
    selectedUtility = if who == Human 
        then logeOppUtility else logeSelfUtility
    endUtils = map (selectedUtility . last) logs

meanUtilitySum :: [Log] -> Double
meanUtilitySum logs = sum endUtils / fromIntegral (length logs)
    where
    human = map (logeOppUtility . last) logs
    comp = map (logeSelfUtility . last) logs
    endUtils = zipWith (+) human comp
