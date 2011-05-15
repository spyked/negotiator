module CLI where

import HumanAgent
import ExampleAgent
import Negotiation
import Control.Monad.State


{- 
    A commandline interface for negotiation.
-}

initNegotiation :: Negotiation ExampleOffer
initNegotiation = Negotiation
    Initiate (ExampleOffer 20 50 60) 0 0 2 5

cliNegotiation = runStateT negotiation 
    (initNegotiation, HumanAgent, exampleAgent) >>= return . fst
