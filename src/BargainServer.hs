module Main where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Happstack.Server
import System.FilePath ((</>))

siteDir :: String
siteDir = "site"

htmlContent :: Monad m => FilePath -> m String
htmlContent = asContentType "text/html"

jsContent :: Monad m => FilePath -> m String
jsContent = asContentType "text/javascript"

cssContent :: Monad m => FilePath -> m String
cssContent = asContentType "text/css"

gifContent :: Monad m => FilePath -> m String
gifContent = asContentType "image/gif"

negotiationSession :: ServerPart Response
negotiationSession = do
    -- liftIO $ print "bla"
    serveFile htmlContent (siteDir </> "session.html")

getHumanName :: ServerPart Response
getHumanName = ok $ toResponse "DeepThought"

spinButtonData :: ServerPart Response
spinButtonData = msum
    [ 
    dir "ui.spinner.js" $ serveFile jsContent (siteDir </> "ui.spinner.js")
    ]


main :: IO ()
main = simpleHTTP nullConf $ msum
    [ 
        dir "session" $ negotiationSession,
        dir "name" $ getHumanName,
            nullDir >> serveFile htmlContent (siteDir </> "index.html"),
        spinButtonData,
        anyPath $ serveFile htmlContent (siteDir </> "404.html") >>= notFound
    ]
