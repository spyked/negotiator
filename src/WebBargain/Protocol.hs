module WebBargain.Protocol
    (protocolAccept, protocolPropose, protocolEndSession,
    protocolOptOut) where

import Happstack.Server
import Control.Monad
import Control.Monad.Trans (liftIO)
import Text.Html hiding ((</>))
import WebBargain.State
import WebBargain.Util
import Negotiator.Negotiation
import Negotiator.SiAgent

-- server part for individual protocol primitives
-- respond to accepted offer
protocolAccept :: ServerPart Response
protocolAccept = protocolEndNegotiation Accept

-- respond to proposed offer
protocolPropose :: ServerPart Response
protocolPropose = do
    methodM POST
    decodeBody postPolicy 
    postReq <- look "proposal"
    let offer = read postReq :: SiOffer
    protocolDecide $ Propose offer

-- respond to end session
protocolEndSession :: ServerPart Response
protocolEndSession = protocolDecide EndSession

-- respond to opt-out: basically identical to accept
protocolOptOut :: ServerPart Response
protocolOptOut = protocolEndNegotiation OptOut

-- generic end negotiation
protocolEndNegotiation :: Decision SiOffer -> ServerPart Response
protocolEndNegotiation d = do
    name <- lookCookieValue "username"
    state <- readCookieValue "state" :: ServerPartT IO SiState
    -- negotiation is over - expire all cookies
    expireAllCookies
    let time = negTime . wsNegotiation $ state
        render = renderHistoryLine time name d
    ok . toResponse $ render

-- generic negotiation decision
protocolDecide :: Decision SiOffer -> ServerPart Response
protocolDecide d = do
    name <- lookCookieValue "username"
    state <- readCookieValue "state" :: ServerPartT IO SiState
    let agent = wsQOAgent initialSiAgent state
        neg@(Negotiation _ sqo n t mn mt) = wsNegotiation state
        neg' = Negotiation d sqo n t mn mt
        humanRender = renderHistoryLine t name d
        t' = t + 1
    if t == mt
        -- negotiation time has expired
        then do
        let state' = mkQOState (Negotiation Accept sqo n t mn mt) agent
            render = humanRender ++ renderSQO t "Computer"
        addCookie Session (mkCookie "offer" $ show sqo)
        addCookie Session (mkCookie "state" $ show state')
        ok . toResponse $ "s" ++ render -- s for "stopped"

        -- negotiation running
        else case d of
            -- human has decided to end session
            EndSession -> do
                let state' = mkQOState (Negotiation d sqo n t' mn mt) 
                             agent
                    render = humanRender
                offer <- liftIO $ genOffer agent neg'
                addCookie Session (mkCookie "offer" $ show offer)
                addCookie Session (mkCookie "state" $ show state')
                ok . toResponse $ "r" ++ render -- r for "running"

            -- human has proposed an offer
            Propose offer -> do
                d' <- liftIO $ decide agent neg'
                agent' <- liftIO $ update agent neg'
                let state' = mkQOState (Negotiation d' sqo n t' mn mt) 
                             agent'
                    render = humanRender ++ 
                             renderHistoryLine t "Computer" d'
                    run = if d' == Accept then "s" else "r"
                    offer' = case d' of
                        Accept -> offer
                        Propose o -> o
                        _ -> sqo -- this should never match
                addCookie Session (mkCookie "offer" $ show offer')
                addCookie Session (mkCookie "state" $ show state')
                ok . toResponse $ run ++ render

            _ -> error "Bad match"

-- some useful html rendering functions
renderHistoryLine :: Offer o => Time -> String -> Decision o -> String
renderHistoryLine time player decision = 
    prettyHtml $ tr << concatHtml [htime, hplayer, hdecision]
    where
    htime = td << show time
    hplayer = td << (bold << player)
    hdecision = td << stringToHtml (case decision of
        Initiate    -> "Negotiation initiated."
        Accept      -> "Accepts opponent's previous offer."
        Propose o   -> "Proposes " ++ show o ++ " to opponent."
        EndSession  -> "Adjourns negotiation."
        OptOut      -> "Opts out of the negotiation.")

renderSQO :: Time -> String -> String
renderSQO time player =
    prettyHtml $ tr << concatHtml [htime, hplayer, hdecision]
    where
    htime = td << show time
    hplayer = td << (bold << player)
    hdecision = td << stringToHtml 
                "Time has expired, agreement is on Status Quo Offer."

