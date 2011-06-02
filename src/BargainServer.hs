module Main where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Happstack.Server
import System.FilePath ((</>))

siteDir :: String
siteDir = "site"

-- content types
htmlContent :: Monad m => FilePath -> m String
htmlContent = asContentType "text/html"

jsContent :: Monad m => FilePath -> m String
jsContent = asContentType "text/javascript"

cssContent :: Monad m => FilePath -> m String
cssContent = asContentType "text/css"

gifContent :: Monad m => FilePath -> m String
gifContent = asContentType "image/gif"

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

negotiationSession :: ServerPart Response
negotiationSession = msum 
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

-- main loop
main :: IO ()
main = simpleHTTP nullConf $ msum
    [ 
        dir "session" $ negotiationSession,
        dir "name" $ getHumanName,
        dir "index" $ serveFile htmlContent (siteDir </> "index.html"),
        nullDir >> serveFile htmlContent (siteDir </> "index.html"),
        spinButtonData,
        anyPath $ serveFile htmlContent (siteDir </> "404.html") >>= notFound
    ]
