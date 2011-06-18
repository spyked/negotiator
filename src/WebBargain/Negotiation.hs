module WebBargain.Negotiation 
    (negotiationSession, serveOffer, serveHumanName, serveTime,
    negotiationProtocol) where

import Control.Monad
import Happstack.Server
import System.FilePath ((</>))
import WebBargain.State
import WebBargain.Protocol
import WebBargain.Util
import Negotiator.Negotiation
import Negotiator.Agent
import Negotiator.SiAgent

-- initialization parameters
initialNegotiation :: Negotiation SiOffer
initialNegotiation = Negotiation {
    negDecision     = Initiate,
    negSQO          = SiOffer 16 14 25 15 6,
    negNumber       = 0,
    negTime         = 0,
    negMaxNumber    = 2,
    negDeadline     = 5
    }

initialQOState :: SiState
initialQOState = QOState {
    wsNegotiation   = initialNegotiation,
    wsOpponentID    = agentID . opponent $ initialSiAgent,
    wsProbabilities = map snd . possibleAdversaries $ initialSiAgent
    }

-- serve nickname 
serveHumanName :: ServerPart Response
serveHumanName = do
    name <- lookCookieValue "username"
    ok . toResponse $ name

-- how session data is delivered to the user
negotiationSession :: ServerPart Response
negotiationSession = msum 
    [
        do methodM POST
           decodeBody postPolicy
           username <- look "username"
           let offer = negSQO initialNegotiation
           addCookie Session (mkCookie "username" username)
           addCookie Session (mkCookie "offer" $ show offer)
           addCookie Session (mkCookie "state" $ show initialQOState)
           serveSession
           -- if method is POST, create a new session with
           -- the specified data.
    ,   do name <- lookCookieValue "username" 
           serveSession
           -- cookie lookup used as a guard to check that we have 
           -- an active session/cookie.
    ,   do serveNoSession
    ]
    where
    serveSession = serveFile htmlContent (siteDir </> "session.html")

-- serve the current offer of the automated negotiator
serveOffer :: ServerPart Response
serveOffer = do 
    offer <- lookCookieValue "offer"
    ok . toResponse $ offer

-- serve the current negotiation round
serveTime :: ServerPart Response
serveTime = msum
    [
        do state <- readCookieValue "state" :: ServerPartT IO SiState
           ok . toResponse . show $ negTime $ wsNegotiation state
    ,   do ok $ toResponse "-42"
    ]

-- handle protocol requests from the client part
negotiationProtocol :: ServerPart Response
negotiationProtocol = msum
    [
        dir "accept" $ protocolAccept,
        dir "propose" $ protocolPropose,
        dir "end-round" $ protocolEndSession,
        dir "opt-out" $ protocolOptOut
    ]

