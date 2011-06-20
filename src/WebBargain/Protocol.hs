module WebBargain.Protocol
    (protocolAccept, protocolPropose, protocolEndSession,
    protocolOptOut) where

import Happstack.Server
import Control.Monad
import Control.Monad.Trans (liftIO)
import Text.Html hiding ((</>))
import WebBargain.State
import WebBargain.Logger
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
    logfile <- lookCookieValue "logfile"
    -- append to log
    liftIO $ makeLogEntry state >>= appendLog logfile
    -- negotiation is over - expire all cookies
    expireAllCookies
    let time = negTime . wsNegotiation $ state
        render = renderHistoryLine time name d
    ok . toResponse $ render

{- generic negotiation decisions -}
-- negotiation time has expired
decideDeadline :: Decision SiOffer -> String -> SiState 
                                             -> ServerPart Response
decideDeadline d humanRender state = do
    logfile <- lookCookieValue "logfile"
    let agent = wsQOAgent initialSiAgent state
        state' = case state of
           QOState _ _ _ -> mkQOState acceptNeg $ 
                            wsQOAgent initialSiAgent state
           TFTState _ -> mkTFTState acceptNeg initialTFTAgent
        render = humanRender ++ renderSQO t "Computer"
    -- log
    liftIO $ makeLogEntry state' >>= appendLog logfile
    -- add cookies
    addCookie Session (mkCookie "offer" $ show sqo)
    addCookie Session (mkCookie "state" $ show state')
    ok . toResponse $ "s" ++ render -- s for "stopped"
    where
    neg@(Negotiation _ sqo n t mn mt) = wsNegotiation state
    acceptNeg = Negotiation Accept sqo n t mn mt

-- negotiation running
-- TODO: lots of duplicate code here, find some way to merge
decideRun :: Decision SiOffer -> String -> SiState
                                        -> ServerPart Response

-- human has decided to end session
decideRun EndSession humanRender state@(QOState _ _ _) = do
    logfile <- lookCookieValue "logfile"
    let agent = wsQOAgent initialSiAgent state
        state' = mkQOState (Negotiation EndSession sqo n t' mn mt) agent
        render = humanRender
    offer <- liftIO $ genOffer agent neg'
    -- log
    liftIO $ makeLogEntry state' >>= appendLog logfile
    -- cookies
    addCookie Session (mkCookie "offer" $ show offer)
    addCookie Session (mkCookie "state" $ show state')
    ok . toResponse $ "r" ++ render -- r for "running"
    where 
    neg@(Negotiation _ sqo n t mn mt) = wsNegotiation state
    neg' = Negotiation EndSession sqo n t mn mt
    t' = t + 1
decideRun EndSession humanRender state@(TFTState _) = do
    logfile <- lookCookieValue "logfile"
    let agent = initialTFTAgent
        state' = mkTFTState (Negotiation EndSession sqo n t' mn mt) agent
        render = humanRender
    offer <- liftIO $ genOffer agent neg'
    -- log
    liftIO $ makeLogEntry state' >>= appendLog logfile
    -- cookies
    addCookie Session (mkCookie "offer" $ show offer)
    addCookie Session (mkCookie "state" $ show state')
    ok . toResponse $ "r" ++ render -- r for "running"
    where 
    neg@(Negotiation _ sqo n t mn mt) = wsNegotiation state
    neg' = Negotiation EndSession sqo n t mn mt
    t' = t + 1

-- human has proposed an offer
decideRun (Propose offer) humanRender state@(QOState _ _ _) = do
    logfile <- lookCookieValue "logfile"
    let agent = wsQOAgent initialSiAgent state
    d' <- liftIO $ decide agent neg'
    agent' <- liftIO $ update agent neg'
    let sqo' = if d' == Accept then offer else sqo
        state' = mkQOState (Negotiation d' sqo' n t' mn mt) agent'
        render = humanRender ++ renderHistoryLine t "Computer" d'
        run = if d' == Accept then "s" else "r"
        offer' = case d' of
            Accept -> offer
            Propose o -> o
            _ -> sqo -- this should never match
    -- log
    liftIO $ makeLogEntry state' >>= appendLog logfile
    -- cookies
    addCookie Session (mkCookie "offer" $ show offer')
    addCookie Session (mkCookie "state" $ show state')
    ok . toResponse $ run ++ render
    where 
    neg@(Negotiation _ sqo n t mn mt) = wsNegotiation state
    neg' = Negotiation (Propose offer) sqo n t mn mt
    t' = t + 1
decideRun (Propose offer) humanRender state@(TFTState _) = do
    logfile <- lookCookieValue "logfile"
    let agent = initialTFTAgent
    d' <- liftIO $ decide agent neg'
    agent' <- liftIO $ update agent neg'
    let sqo' = if d' == Accept then offer else sqo
        state' = mkTFTState (Negotiation d' sqo' n t' mn mt) agent'
        render = humanRender ++ renderHistoryLine t "Computer" d'
        run = if d' == Accept then "s" else "r"
        offer' = case d' of
            Accept -> offer
            Propose o -> o
            _ -> sqo -- this should never match
    -- log
    liftIO $ makeLogEntry state' >>= appendLog logfile
    -- cookies
    addCookie Session (mkCookie "offer" $ show offer')
    addCookie Session (mkCookie "state" $ show state')
    ok . toResponse $ run ++ render
    where 
    neg@(Negotiation _ sqo n t mn mt) = wsNegotiation state
    neg' = Negotiation (Propose offer) sqo n t mn mt
    t' = t + 1

-- something's not right if we get here
decideRun _ _ _ = error "bad match" 

{-------------------------------------------------------------------}

-- main decision server part
protocolDecide :: Decision SiOffer -> ServerPart Response
protocolDecide d = do
    name <- lookCookieValue "username"
    state <- readCookieValue "state" :: ServerPartT IO SiState
    logfile <- lookCookieValue "logfile"
    let neg@(Negotiation _ sqo n t mn mt) = wsNegotiation state
        neg' = Negotiation d sqo n t mn mt
        humanRender = renderHistoryLine t name d
        humanState = stateFromNegotiation neg' state
    -- side effect: log
    liftIO $ makeLogEntry humanState >>= appendLog logfile
    -- decide whether to run or stop
    if t == mt
        then decideDeadline d humanRender state
        else decideRun d humanRender state

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

