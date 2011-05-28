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
