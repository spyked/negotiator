module Main where

import System.IO
import System.Random (randomRIO)
import Negotiator.Agent
import Negotiator.SiAgent
import Negotiator.Negotiation
import Control.Monad.State
import WebBargain.Logger
import WebBargain.State

data AvAEntry = AvAEntry {
    entryA :: LogEntry,
    entryB :: LogEntry
    } deriving (Show, Read)

type AvALog = [AvAEntry]

{- 
    A commandline interface for negotiation.
-}

initNegotiation :: Negotiation SiOffer
initNegotiation = Negotiation
    Initiate (SiOffer 10 7 15 10 4) 0 0 2 5

mkSiOpponent :: IO (QOAgent SiOffer)
mkSiOpponent = do
    r <- randomRIO (0, length advs - 1)
    let self' = fst $ advs !! r
        opp' = self
        advs' = [(self,1)]
    return $ QOAgent ss thr self' opp' advs'
    where
    (QOAgent ss thr self opp advs) = mkSiAgent

moveAgent :: (Negotiator a, Offer o) => a o -> Negotiation o -> 
                                        IO (Decision o, a o)
moveAgent ag neg = do
    d' <- decide ag neg
    ag' <- update ag neg
    return (d', ag')

-- Generic function definitions for negotiations
move :: (Negotiator a, Negotiator b, Offer o) =>
        Negotiation o -> a o -> b o -> FilePath 
                                    -> IO (Negotiation o, a o, b o, FilePath)
move neg@(Negotiation d sqo num t maxnum maxt) aa ab fp
    | num `mod` 2 == 0 = do
    (d',aa') <- moveAgent aa neg
    let sqo' = updateSqo d'
    return (Negotiation d' sqo' (num' d') (t' d') maxnum maxt, aa', ab, fp)

    | otherwise = do
    (d',ab') <- moveAgent ab neg
    let sqo' = updateSqo d'
    return (Negotiation d' sqo' (num' d') (t' d') maxnum maxt, aa, ab', fp)

    where
    num' d' = if num + 1 == maxnum || d' == EndSession 
              then 0 else num + 1
    t' d' = if num + 1 == maxnum || d' == EndSession 
            then t + 1 else t
    updateSqo Accept = case d of
        Propose o -> o
        _ -> sqo
    updateSqo _ = sqo

negotiation :: StateT (Negotiation SiOffer, QOAgent SiOffer, 
                                            QOAgent SiOffer,
                                            FilePath) IO (Outcome SiOffer)
negotiation = do
    (neg, aa, ab, fp) <- get
    let statea = mkQOState neg aa
        stateb = mkQOState neg ab
    lea <- lift $ makeLogEntry statea
    leb <- lift $ makeLogEntry stateb
    lift $ appendFile fp $ show (AvAEntry lea leb) ++ "\n"
    
    if negTime neg == negDeadline neg || 
       negDecision neg == Accept
        then return . Agree $ negSQO neg
        else if negDecision neg == OptOut
            then return $ Disagree
            else (lift $ move neg aa ab fp) >>= put >> negotiation

cliNegotiation = do
    (path, handle) <- openTempFile logFileDir logFileName
    hClose handle
    opp <- mkSiOpponent
    runStateT negotiation 
        (initNegotiation, mkSiAgent, opp, path) >>= return . fst
main = cliNegotiation

logFileDir :: FilePath
logFileDir = "logs"

logFileName :: String
logFileName = "ava.log"
