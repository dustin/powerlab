{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import WaiAppStatic.Types (unsafeToPiece)
import Network.Wai.Application.Static (StaticSettings(..)
                                      , staticApp
                                      , ssIndices
                                      , defaultFileServerSettings
                                      , defaultWebAppSettings )


import Data.Time
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import System.Environment
import System.IO
import System.Console.GetOpt
import Control.Monad
import qualified Data.ByteString.Lazy as B

import Powerlab
import Powerlab.Serial
import Powerlab.Logger
import MiniJSON
import qualified Powerlab.Status as St

data TSRec = TSRec { ts :: !UTCTime, st :: !St.Status }

instance ToJSON TSRec where

  toJSON (TSRec ts st) = object ["ts" .= ts, "st" .= st]

data State = State { current :: !(Maybe TSRec), recent :: ![TSRec] }

-- Application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app :: Application -> TVar State -> Application
app stat tv request respond = case rawPathInfo request of
  "/status"   -> status respond tv
  "/statuses" -> statuses respond tv
  _           -> stat request respond

get :: TVar State -> IO State
get tv = atomically $ readTVar tv

status :: (Response -> IO ResponseReceived) -> TVar State -> IO (ResponseReceived)
status respond tv = do
  st <- get tv
  respond $ responseLBS status200 [("Content-Type", "application/json")] $ encode $ current st

statuses :: (Response -> IO ResponseReceived) -> TVar State -> IO (ResponseReceived)
statuses respond tv = do
  st <- get tv
  respond $ responseLBS status200 [("Content-Type", "application/json")] $ encode $ recent st

setState :: UTCTime -> St.Status -> TVar State -> STM ()
setState t st tv = do
  State _ l <- readTVar tv
  let tst = TSRec t st
  writeTVar tv (State (Just tst) (tst:take 3600 l))

log_writer :: UTCTime -> St.Status -> Handle -> IO ()
log_writer t st h = B.hPut h $ encode $ TSRec t st

updater :: TVar State -> Logger St.Status -> FilePath -> IO ()
updater tv lf serial =
  withPort serial (\st -> do
                      putStrLn $ "Updating with " ++ (show $ encode st)
                      now <- getCurrentTime
                      log_item lf now st
                      atomically $ setState now st tv)

newState :: STM (TVar State)
newState = newTVar $ State Nothing []

data Options = Options  { optPort :: Int
                        , optStatic :: FilePath
                        , optSerial :: FilePath
                        , optLogfile :: String } deriving Show

startOptions :: Options
startOptions = Options  { optPort = 8080
                        , optStatic = "static"
                        , optSerial = ""
                        , optLogfile = "/tmp/%Y%m%dT%H%M%S" }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "p" ["port"]
    (ReqArg
      (\arg opt -> return opt { optPort = read arg }) "port")
    "Port Number",
    Option "s" ["static"]
    (ReqArg
     (\arg opt -> return opt { optStatic = arg }) "static")
    "Path to static files",
    Option "S" ["serial"]
    (ReqArg
     (\arg opt -> return opt { optSerial = arg }) "serial")
    "Path to serial port",
    Option "L" ["logfile"]
    (ReqArg
     (\arg opt -> return opt { optSerial = arg }) "logfile")
    "Path to timestamped logfile"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (opts, misc, errors) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return startOptions) opts

  let statApp = staticApp $ (defaultWebAppSettings (optStatic opts)) {ssIndices = [unsafeToPiece "index.html"]}

  tv <- atomically newState
  let lf = new_logger (time_fmt_namer $ optLogfile opts) (const . const True) log_writer
  forkFinally (updater tv lf (optSerial opts)) (\x -> error $ "updater fatality: " ++ (show x))

  putStrLn $ "http://localhost:" ++ (show $ optPort opts)
  run (optPort opts) $ app statApp tv
