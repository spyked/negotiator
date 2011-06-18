module WebBargain.Logger (makeLog, appendLog, makeLogEntry) where

import System.IO (openTempFile, appendFile, hClose)
import System.Locale (defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime)
import WebBargain.State
import Negotiator.Negotiation
import Negotiator.SiOffer (SiOffer)
import Negotiator.TFTAgent (tftuA, tftuB)
import Negotiator.Agent (self, opponent, offerUtility)

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
appendLog path entry = appendFile path $ show entry ++ "\n"

-- make a log entry
makeLogEntry :: WebState SiOffer -> IO LogEntry
makeLogEntry state@(QOState neg _ _) = do
    date <- getCurrentFormattedTime
    return $ LogEntry date state ua ub
    where
    o = case negDecision neg of
        Propose smth -> smth
        _ -> negSQO neg
    t = negTime neg

    -- agents 
    qoagent = wsQOAgent initialSiAgent state
    a = self qoagent
    b = opponent qoagent

    -- utilities
    ua = offerUtility a o t
    ub = offerUtility b o t

makeLogEntry state@(TFTState neg) = do
    date <- getCurrentFormattedTime
    return $ LogEntry date state ua ub
    where
    o = case negDecision neg of
        Propose smth -> smth
        _ -> negSQO neg
    ua = tftuA initialTFTAgent o
    ub = tftuB initialTFTAgent o

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

