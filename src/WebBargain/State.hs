module WebBargain.State where

import Debug.Trace
import Negotiator.Negotiation
import Negotiator.Agent

data Offer o => WebState o = WebState {
    wsNegotiation :: Negotiation o,
    wsOpponentID :: String,
    wsProbabilities :: [Double]
    }

instance Offer o => Show (WebState o) where
    show (WebState n o p) = "(" ++ show n ++ "," ++ o ++ "," 
                         ++ show p ++ ")"

instance Offer o => Read (WebState o) where
    readsPrec _ = readParen True 
        (\ inp -> [(WebState n o p, r) | 
                    (n, ',' : r') <- reads inp,
                    (o, ',' : r'') <- lex r',
                    (p, r) <- reads r'' ]
        )

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

