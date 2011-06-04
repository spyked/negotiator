module WebBargain.State where

import Negotiator.Negotiation
import Negotiator.Agent

data Offer o => WebState o = WebState {
    wsNegotiation :: Negotiation o,
    wsOpponentID :: String,
    wsProbabilities :: [Double]
    } deriving (Show, Read)

-- gets a stateful wsQOAgent from the initial one plus a WebState
wsQOAgent :: Offer o => QOAgent o -> WebState o -> QOAgent o
wsQOAgent (QOAgent ss thr me _ advs) (WebState _ oppID probs) = 
    QOAgent ss thr me opp' advs'
    where
    agTypes = map fst advs
    opp' = case findAgent oppID agTypes of
        Just ag -> ag
        Nothing -> head agTypes -- not really ok
    advs' = zipWith (\ (ag,_) p -> (ag,p) ) advs probs -- update ps

