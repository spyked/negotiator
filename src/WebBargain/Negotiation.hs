module WebBargain.Negotiation 
    (negotiationSession, serveOffer, serveHumanName, 
    negotiationProtocol) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Happstack.Server
import System.FilePath ((</>))
import WebBargain.State
import WebBargain.Util
import Negotiator.Negotiation
import Negotiator.Agent
import Negotiator.SiAgent

-- initialization parameters
initialSiAgent :: QOAgent SiOffer
initialSiAgent = mkSiAgent

initialNegotiation :: Negotiation SiOffer
initialNegotiation = Negotiation {
    negDecision     = Initiate,
    negSQO          = SiOffer 16 14 25 15 6,
    negNumber       = 0,
    negTime         = 0,
    negMaxNumber    = 2,
    negDeadline     = 5
    }

initialState :: WebState SiOffer
initialState = WebState {
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
           --addCookie Session (mkCookie "username" username)
           addCookie Session (mkCookie "offer" $ show offer)
           addCookie Session (mkCookie "state" $ show initialState)
           --addCookie Session (mkCookie "sicookie" $ show cookie)
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
    postPolicy = defaultBodyPolicy "/tmp/" 0 1024 1024

-- serve the current offer of the automated negotiator
serveOffer :: ServerPart Response
serveOffer = msum
    [
        do offer <- lookCookieValue "offer"
           ok . toResponse $ offer
    ,   do ok $ toResponse "no_offer"
    ]

negotiationProtocol :: ServerPart Response
negotiationProtocol = serve404 -- not defined yet

