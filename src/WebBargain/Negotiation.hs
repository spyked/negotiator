module WebBargain.Negotiation (negotiationSession) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Happstack.Server
import System.FilePath ((</>))
import WebBargain.State
import WebBargain.Util
import Negotiator.SiAgent

negotiationSession :: FilePath -> ServerPart Response
negotiationSession siteDir = msum 
    [
        do methodM POST
           decodeBody postPolicy
           username <- look "username"
           -- TODO: initialize state
           addCookie Session (mkCookie "username" username)
           serveSession
    ,   do username <- lookCookieValue "username" 
           serveSession
           -- TODO: handle state
    ,   do serveFile htmlContent (siteDir </> "nosession.html")
    ]
    where
    serveSession = serveFile htmlContent (siteDir </> "session.html")
    postPolicy = defaultBodyPolicy "/tmp/" 0 256 256

