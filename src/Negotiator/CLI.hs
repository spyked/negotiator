module Negotiator.CLI where

import Negotiator.HumanAgent
import Negotiator.SiAgent
import Negotiator.Negotiation
import Control.Monad.State


{- 
    A commandline interface for negotiation.
-}

initNegotiation :: Negotiation SiOffer
initNegotiation = Negotiation
    Initiate (SiOffer 16 14 25 15 6) 0 0 2 5

cliNegotiation = runStateT negotiation 
    (initNegotiation, HumanAgent, mkSiAgent) >>= return . fst

moveAgent :: (Negotiator a, Offer o) => a o -> Negotiation o -> 
                                        IO (Decision o, a o)
moveAgent ag neg = do
    d' <- decide ag neg
    ag' <- update ag neg
    return (d', ag')

-- Generic function definitions for negotiations
move :: (Negotiator a, Negotiator b, Offer o) =>
        Negotiation o -> a o -> b o -> IO (Negotiation o, a o, b o)
move neg@(Negotiation d sqo num t maxnum maxt) aa ab 
    | num `mod` 2 == 0 = do
    (d',aa') <- moveAgent aa neg
    let sqo' = updateSqo d'
    return (Negotiation d' sqo' (num' d') (t' d') maxnum maxt, aa', ab)

    | otherwise = do
    (d',ab') <- moveAgent ab neg
    let sqo' = updateSqo d'
    return (Negotiation d' sqo' (num' d') (t' d') maxnum maxt, aa, ab')

    where
    num' d' = if num + 1 == maxnum || d' == EndSession 
              then 0 else num + 1
    t' d' = if num + 1 == maxnum || d' == EndSession 
            then t + 1 else t
    updateSqo Accept = case d of
        Propose o -> o
        _ -> sqo
    updateSqo _ = sqo

negotiation :: (Negotiator a, Negotiator b, Offer o) => 
                StateT (Negotiation o, a o, b o) IO (Outcome o)
negotiation = do
    (neg, aa, ab) <- get
    
    if negTime neg == negDeadline neg || 
       negDecision neg == Accept
        then return . Agree $ negSQO neg
        else if negDecision neg == OptOut
            then return $ Disagree
            else (lift $ move neg aa ab) >>= put >> negotiation

