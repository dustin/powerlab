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
import System.Console.GetOpt
import Control.Monad
import qualified Data.ByteString.Lazy as B

import Powerlab
import Powerlab.Serial
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

index :: Response
index = responseLBS
  status200
  [("Content-Type", "text/plain")]
  "Hello, Web!"

setState :: UTCTime -> St.Status -> TVar State -> STM ()
setState t st tv = do
  State _ l <- readTVar tv
  let tst = TSRec t st
  writeTVar tv (State (Just tst) (tst:take 3 l))

updater :: TVar State -> FilePath -> IO ()
updater tv serial = do
  withPort serial (\st -> do
                      now <- getCurrentTime
                      atomically $ setState now st tv)

newState :: STM (TVar State)
newState = do x <- newTVar $ State Nothing []; return x

data Options = Options  { optPort :: Int
                        , optStatic :: FilePath
                        , optSerial :: FilePath } deriving Show

startOptions :: Options
startOptions = Options  { optPort = 8080
                        , optStatic = "static"
                        , optSerial = "" }

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
    "Path to serial port"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (opts, misc, errors) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return startOptions) opts

  let statApp = staticApp $ (defaultWebAppSettings (optStatic opts)) {ssIndices = [unsafeToPiece "index.html"]}

  tv <- atomically newState
  forkFinally (updater tv (optSerial opts)) (\x -> error $ "updater fatality: " ++ (show x))

  putStrLn $ "http://localhost:" ++ (show $ optPort opts)
  run (optPort opts) $ app statApp tv
