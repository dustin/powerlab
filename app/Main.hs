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
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import System.Environment
import System.Console.GetOpt
import Control.Monad

import Powerlab
import MiniJSON
import qualified Powerlab.Status as St

data TSRec = TSRec { ts :: UTCTime, st :: St.Status }

instance ToJSON TSRec where

  toJSON (TSRec ts st) = object ["ts" .= ts, "st" .= st]

data State = State { current :: Maybe TSRec, recent :: [TSRec] }

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

update :: TVar State -> IO ()
update tv = do
  putStrLn "updating..."
  now <- getCurrentTime
  let d = B.pack [
        0x52, 0x61, 0x6d, 0x0, 0x0, 0x6f, 0xc7, 0xa0, 0xc7, 0xa0,
        0xc7, 0x90, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
        0x1f, 0x74, 0x3, 0x84, 0x43, 0x5c, 0x4, 0x26, 0x7, 0x6, 0x0,
        0xa, 0x0, 0x0, 0x4, 0x15, 0x0, 0x0, 0x0, 0x0, 0x3, 0x13, 0x3,
        0x13, 0x0, 0x0, 0x0, 0x82, 0x7, 0xe1, 0x0, 0x0, 0x80, 0x0,
        0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
        0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xc7, 0xb0, 0xc7, 0xb0, 0xc7,
        0xa0, 0x80, 0x60, 0x0, 0x0, 0x0, 0x0, 0x41, 0x50, 0x0, 0x0,
        0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
        0x0, 0x0, 0x0, 0x7, 0xff, 0x82, 0x4, 0x36, 0x0, 0x0, 0x0, 0x0,
        0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x3, 0x84, 0x0, 0x0,
        0x7, 0x8, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x3, 0x6,
        0x0, 0x1, 0x1, 0x2, 0x0, 0x2, 0x0, 0xc, 0x0, 0x0, 0x0, 0x0,
        0x0, 0x32, 0x3]
  parsed <- try $ evaluate $ St.parse d :: IO (Either SomeException St.Status)
  case parsed of
    Left ex -> putStrLn $ "exception parsing data: " ++ (show ex)
    Right newState -> atomically $ setState now newState tv

updater :: TVar State -> IO ()
updater tv = do
  update tv
  threadDelay 5000000
  updater tv

newState :: STM (TVar State)
newState = do x <- newTVar $ State Nothing []; return x

data Options = Options  { optPort :: Int
                        , optStatic :: FilePath } deriving Show

startOptions :: Options
startOptions = Options  { optPort = 8080
                        , optStatic = "static"}

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "p" ["port"]
    (ReqArg
      (\arg opt -> return opt { optPort = read arg }) "port")
    "Port Number",
    Option "s" ["static"]
    (ReqArg
     (\arg opt -> return opt { optStatic = arg }) "static")
    "Path to static files"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (opts, misc, errors) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return startOptions) opts

  let statApp = staticApp $ (defaultWebAppSettings (optStatic opts)) {ssIndices = [unsafeToPiece "index.html"]}

  tv <- atomically newState
  forkIO $ updater tv
  putStrLn $ "http://localhost:" ++ (show $ optPort opts)
  run (optPort opts) $ app statApp tv
