module WebBargain.Util where

import Happstack.Server (asContentType, serveFile, notFound,
                        ServerPart, Response, expireCookie, BodyPolicy,
                        defaultBodyPolicy)
import System.FilePath ((</>))

-- site directory
siteDir :: String
siteDir = "site"

-- default policy for post requests
postPolicy :: BodyPolicy
postPolicy = defaultBodyPolicy "/tmp/" 0 1024 1024

-- content types
htmlContent :: Monad m => FilePath -> m String
htmlContent = asContentType "text/html"

jsContent :: Monad m => FilePath -> m String
jsContent = asContentType "text/javascript"

cssContent :: Monad m => FilePath -> m String
cssContent = asContentType "text/css"

gifContent :: Monad m => FilePath -> m String
gifContent = asContentType "image/gif"

-- serve a neat 404 message
serve404 :: ServerPart Response
serve404 = serveFile htmlContent (siteDir </> "404.html") >>= notFound

-- served when the negotiation session isn't initialized
serveNoSession :: ServerPart Response
serveNoSession = serveFile htmlContent $ siteDir </> "nosession.html"

-- called when session ends
expireAllCookies :: ServerPart ()
expireAllCookies = do
    expireCookie "username"
    expireCookie "offer"
    expireCookie "state"


