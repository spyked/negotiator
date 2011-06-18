module WebBargain.State where

import Negotiator.Negotiation
import Negotiator.Agent
import Negotiator.SiAgent (SiOffer, mkSiAgent)

type SiState = WebState SiOffer

-- SiAgent alias
initialSiAgent :: QOAgent SiOffer
initialSiAgent = mkSiAgent

data Offer o => WebState o = QOState {
    wsNegotiation :: Negotiation o,
    wsOpponentID :: String,
    wsProbabilities :: [Double]
    }

instance Offer o => Show (WebState o) where
    show (QOState n o p) = "QO (" ++ show n ++ "," ++ o ++ "," 
                         ++ show p ++ ")"

instance Offer o => Read (WebState o) where
    readsPrec _ str = case lex str of
        [("QO",rest)] -> readParen True 
            (\ inp -> [(QOState n o p, r) | 
                        (n, ',' : r') <- reads inp,
                        (o, ',' : r'') <- lex r',
                        (p, r) <- reads r'' ]
            ) rest
        _ -> error "no parse"

isQOState :: Offer o => WebState o -> Bool
isQOState (QOState _ _ _) = True
isQOState _ = False

-- gets a stateful wsQOAgent from the initial one plus a WebState
wsQOAgent :: Offer o => QOAgent o -> WebState o -> QOAgent o
wsQOAgent (QOAgent ss thr me _ advs) (QOState _ oppID probs) = 
    QOAgent ss thr me opp' advs'
    where
    agTypes = map fst advs
    opp' = case findAgent oppID agTypes of
        Just ag -> ag
        Nothing -> head agTypes -- not really ok
    advs' = zipWith (\ (ag,_) p -> (ag,p) ) advs probs -- update ps

mkQOState :: Offer o => Negotiation o -> QOAgent o -> WebState o
mkQOState neg (QOAgent _ _ _ opp advs) = 
    QOState neg (agentID opp) (map snd advs)
