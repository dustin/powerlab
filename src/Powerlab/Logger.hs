module Powerlab.Logger (
  Logger, new_logger, log_item
  )where

import Data.Time
import System.IO

data Logger a = Logger {
  logNamer :: UTCTime -> String
  , logFile :: Maybe Handle
  , interesting :: UTCTime -> a -> Bool
  , write :: UTCTime -> a -> Handle -> IO ()
  }

new_logger :: (UTCTime -> String) -> (UTCTime -> a -> Bool) -> (UTCTime -> a -> Handle -> IO ()) -> Logger a
new_logger namer filt writer = Logger{logNamer = namer
                                     , logFile = Nothing
                                     , interesting = filt
                                     , write = writer }

log_item :: Logger a -> UTCTime -> a -> IO (Logger a)
log_item logger@Logger{interesting = filt} t a =
  if (filt t a) then with_open_logger logger t a else close_logger logger

log_item' :: Logger a -> UTCTime -> a -> IO ()
log_item' Logger{logFile = Just f, write = w} t a = w t a f

with_open_logger :: Logger a -> UTCTime -> a -> IO (Logger a)
with_open_logger logger@Logger{logFile = Just _} t a = log_item logger t a >> return logger
with_open_logger logger@Logger{logNamer = namer} t a = do
  let fn = namer t
  f <- openFile fn WriteMode
  let rv = logger {logFile = Just f}
  log_item' rv t a
  return rv

close_logger :: Logger a -> IO (Logger a)
close_logger logger@Logger{logFile = Nothing} = return logger
close_logger logger@Logger{logFile = Just h} = hClose h >> return logger {logFile = Nothing}
