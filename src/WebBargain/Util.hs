module WebBargain.Util where

import Happstack.Server (asContentType)

-- content types
htmlContent :: Monad m => FilePath -> m String
htmlContent = asContentType "text/html"

jsContent :: Monad m => FilePath -> m String
jsContent = asContentType "text/javascript"

cssContent :: Monad m => FilePath -> m String
cssContent = asContentType "text/css"

gifContent :: Monad m => FilePath -> m String
gifContent = asContentType "image/gif"

