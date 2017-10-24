module Powerlab.Logger (
  Logger, newLogger, logItem
  )where

import Data.Time
import System.IO

data Logger a = Logger {
  logNamer :: UTCTime -> String
  , logFile :: Maybe Handle
  , interesting :: UTCTime -> a -> Bool
  , write :: UTCTime -> a -> Handle -> IO ()
  }

newLogger :: (UTCTime -> String) -> (UTCTime -> a -> Bool) -> (UTCTime -> a -> Handle -> IO ()) -> Logger a
newLogger namer filt writer = Logger{logNamer = namer
                                     , logFile = Nothing
                                     , interesting = filt
                                     , write = writer }

logItem :: Logger a -> UTCTime -> a -> IO (Logger a)
logItem logger@Logger{interesting = filt} t a =
  if filt t a then withOpenLogger logger t a else closeLogger logger

logItem' :: Logger a -> UTCTime -> a -> IO ()
logItem' Logger{logFile = Just f, write = w} t a = w t a f
logItem' Logger{logFile = Nothing} _ _ = pure ()

withOpenLogger :: Logger a -> UTCTime -> a -> IO (Logger a)
withOpenLogger logger@Logger{logFile = Just _} t a = logItem logger t a >> return logger
withOpenLogger logger@Logger{logNamer = namer} t a = do
  let fn = namer t
  f <- openFile fn WriteMode
  let rv = logger {logFile = Just f}
  logItem' rv t a
  return rv

closeLogger :: Logger a -> IO (Logger a)
closeLogger logger@Logger{logFile = Nothing} = return logger
closeLogger logger@Logger{logFile = Just h} = hClose h >> return logger {logFile = Nothing}
