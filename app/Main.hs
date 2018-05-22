{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import WaiAppStatic.Types (unsafeToPiece)
import Network.Wai.Application.Static (StaticSettings(..)
                                      , staticApp
                                      , ssIndices
                                      , defaultWebAppSettings )


import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Safe (tryAny)
import Control.Monad (forever)
import Data.Semigroup ((<>))
import Data.Time
import Options.Applicative
import System.IO
import qualified Data.ByteString.Lazy as B

import Powerlab.Serial
import Powerlab.Logger
import Data.Aeson (ToJSON(..), object, (.=), encode)
import qualified Powerlab.Status as St

data TSRec = TSRec { _ts :: !UTCTime, _st :: !St.Status }

instance ToJSON TSRec where

  toJSON (TSRec ts st) = object ["ts" .= ts, "st" .= st]

data State = State { current :: !(Maybe TSRec), recent :: ![TSRec] }

-- Application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app :: Application -> TVar State -> Application
app stat tv request respond = case rawPathInfo request of
  "/status"   -> status respond tv
  "/rawstatus"   -> rawstatus respond tv
  "/statuses" -> statuses respond tv
  _           -> stat request respond

get :: TVar State -> IO State
get tv = atomically $ readTVar tv

status :: (Response -> IO ResponseReceived) -> TVar State -> IO ResponseReceived
status respond tv = do
  st <- get tv
  respond $ responseLBS status200 [("Content-Type", "application/json")] $ encode $ current st

rawstatus :: (Response -> IO ResponseReceived) -> TVar State -> IO ResponseReceived
rawstatus respond tv = do
  c <- get tv
  let b = maybe B.empty (St.unwrap <$> _st) (current c)
  respond $ responseLBS status200 [("Content-Type", "application/octet-stream")] b

statuses :: (Response -> IO ResponseReceived) -> TVar State -> IO ResponseReceived
statuses respond tv = do
  st <- get tv
  respond $ responseLBS status200 [("Content-Type", "application/json")] $ encode $ recent st

setState :: UTCTime -> St.Status -> TVar State -> STM ()
setState t st tv = do
  State _ l <- readTVar tv
  let tst = TSRec t st
  writeTVar tv (State (Just tst) (tst:take 3600 l))

logWriter :: UTCTime -> St.Status -> Handle -> IO ()
logWriter t st h = B.hPut h $ encode $ TSRec t st

updater :: TVar State -> Logger St.Status -> FilePath -> IO ()
updater tv lf serial = forever $ do
  putStrLn $ "Opening " <> serial
  tryAny $
    withPort serial (\st -> do
                        putStrLn $ "Updating with " ++ show (encode st)
                        now <- getCurrentTime
                        _ <- logItem lf now st
                        atomically $ setState now st tv)

newState :: STM (TVar State)
newState = newTVar $ State Nothing []

data Options = Options  { _optPort :: Int
                        , _optStatic :: FilePath
                        , _optSerial :: FilePath
                        , _optLogfile :: String }

options :: Parser Options
options = Options
  <$> option auto (long "port" <> showDefault <> value 8080 <> help "listen port")
  <*> strOption (long "static" <> showDefault <> value "static" <> help "path to static web content")
  <*> strOption (long "serial" <> help "path to serial port")
  <*> strOption (long "logfile" <> showDefault <> value "/tmp/%Y%m%dT%H%M%S" <> help "path to timestamped log file")

main' :: Options -> IO ()
main' (Options oPort oStat oSer oLog) = do
  let statApp = staticApp $ (defaultWebAppSettings oStat) {ssIndices = [unsafeToPiece "index.html"]}

  tv <- atomically newState
  let lf = newLogger (formatTime defaultTimeLocale oLog) (const . const True) logWriter

  putStrLn $ "http://localhost:" ++ show oPort

  race_ (updater tv lf oSer) (run oPort $ app statApp tv)

main :: IO ()
main =  main' =<< execParser opts
  where opts = info (options <**> helper)
               ( fullDesc <> progDesc "Monitor the powerlab.")
