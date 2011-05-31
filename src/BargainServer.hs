module Main where

import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Text.Html as H
import Happstack.Server
import System.FilePath ((</>))

siteDir :: String
siteDir = "site"

htmlContent :: Monad m => FilePath -> m String
htmlContent = asContentType "text/html"

negotiationSession :: ServerPart Response
negotiationSession = do
    -- liftIO $ print "bla"
    serveFile htmlContent (siteDir </> "session.html")

getHumanName :: ServerPart Response
getHumanName = ok $ toResponse "DeepThought"

main :: IO ()
main = simpleHTTP nullConf $ msum
    [ 
        dir "session" $ negotiationSession,
        dir "name" $ getHumanName,
        nullDir >> serveFile htmlContent (siteDir </> "index.html"),
        anyPath $ serveFile htmlContent (siteDir </> "404.html") >>= notFound
    ]
