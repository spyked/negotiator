module Main where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Happstack.Server
import System.FilePath ((</>))
import WebBargain.Util
import WebBargain.Negotiation

siteDir :: String
siteDir = "site"

-- serve spin button data
spinButtonData :: ServerPart Response
spinButtonData = msum
    [ 
    dir "ui.spinner.js" $ serveFile jsContent (siteDir </> "ui.spinner.js")
    ]

-- serve various resources
getHumanName :: ServerPart Response
getHumanName = do
    name <- lookCookieValue "username"
    ok $ toResponse name

-- main loop
main :: IO ()
main = simpleHTTP nullConf $ msum
    [ 
        dir "session" $ negotiationSession siteDir,
        dir "name" $ getHumanName,
        dir "index" $ serveFile htmlContent (siteDir </> "index.html"),
        nullDir >> serveFile htmlContent (siteDir </> "index.html"),
        spinButtonData,
        anyPath $ serveFile htmlContent (siteDir </> "404.html") >>= notFound
    ]

