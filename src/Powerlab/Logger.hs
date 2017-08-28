module Powerlab.Logger (
  Logger, time_fmt_namer, new_logger
  )where

import Data.Time
import Data.Time.Format
import System.IO

data Logger a = Logger {
  logNamer :: UTCTime -> String
  , logFile :: Maybe Handle
  , interesting :: a -> Bool
  , write :: a -> Handle -> IO ()
  }

time_fmt_namer :: String -> UTCTime -> String
time_fmt_namer = formatTime defaultTimeLocale

new_logger :: (UTCTime -> String) -> (a -> Bool) -> (a -> Handle -> IO ()) -> Logger a
new_logger namer filt writer = Logger{logNamer = namer
                                     , logFile = Nothing
                                     , interesting = filt
                                     , write = writer }

log_item :: Logger a -> a -> IO ()
log_item logger@Logger{logNamer = namer
                      , logFile = file
                      , interesting = filt
                      , write = wf} a = return ()
