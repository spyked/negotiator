module Main where

import Control.Monad
import Happstack.Server

htmlContent :: Monad m => FilePath -> m String
htmlContent = asContentType "text/html"

main :: IO ()
main = simpleHTTP nullConf $ msum
    [ 
        nullDir >> serveFile htmlContent "index.html"
    ]
