module WebBargain.Logger (makeLog, appendLog, makeLogEntry) where

import System.IO (openTempFile, appendFile, hClose)
import System.Locale (defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime)
import WebBargain.State
import Negotiator.SiOffer (SiOffer)

data LogEntry = LogEntry {
    logeDate :: String,
    logeWebState :: WebState SiOffer,
    logeSelfUtility :: Double,
    logeOppUtility :: Double
    } deriving (Show, Read)
type Log = [LogEntry]

-- create a log file for a given user name
makeLog :: String -> IO FilePath
makeLog username = do
    (path, handle) <- openTempFile logDir $ username ++ ".log"
    hClose handle
    return path

-- append log entry to the log file
appendLog :: FilePath -> LogEntry -> IO ()
appendLog path entry = appendFile path $ show entry

-- make a log entry
makeLogEntry :: WebState SiOffer -> IO LogEntry
makeLogEntry state@(QOState neg _ _) = undefined
makeLogEntry (TFTState neg) = undefined

-- path where log is supposed to be saved
logDir :: FilePath
logDir = "/home/spyked/neglogs"

-- current-time-to-string function
getCurrentFormattedTime :: IO String
getCurrentFormattedTime = do
    time <- getZonedTime
    return $ formatTime defaultTimeLocale format time
    where
    format = "%Y-%m-%d, %k:%M:%S"

