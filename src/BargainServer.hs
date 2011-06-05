module Main where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Happstack.Server
import System.FilePath ((</>))
import WebBargain.Util
import WebBargain.Negotiation

-- serve spin button data
spinButtonData :: ServerPart Response
spinButtonData = msum
    [ 
    dir "ui.spinner.js" $ serveFile jsContent (siteDir </> "ui.spinner.js")
    ]

loopbackCookie :: ServerPart Response
loopbackCookie = do
    req <- askRq
    ok . toResponse $ show req
    --cookie <- lookCookie "sicookie"
    --ok . toResponse $ show cookie

-- main loop
main :: IO ()
main = simpleHTTP nullConf $ msum
    [ 
        dir "session" $ negotiationSession,
        dir "name" $ serveHumanName,
        dir "cookie" $ loopbackCookie,
        --dir "offer" $ serveOffer,
        --dir "messages" $ negotiationProtocol,
        dir "index" $ serveFile htmlContent (siteDir </> "index.html"),
        nullDir >> serveFile htmlContent (siteDir </> "index.html"),
        spinButtonData,
        anyPath $ serve404
    ]

