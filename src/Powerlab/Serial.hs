module Powerlab.Serial (
  withPort
  ) where

import qualified Powerlab.Status as St
import Control.Concurrent
import Control.Monad

import System.Hardware.Serialport
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B

type MessageHandler = St.Status -> IO ()

statusReq :: B.ByteString
statusReq = B.pack ['R', 'a', 'm', '\0']

readMatch :: Int -> SerialPort -> String -> IO Bool
readMatch 0 _ _ = return False
readMatch _ _ [] = return True
readMatch ttl s l@(x:xs) = do
  c <- recv s 1
  if B.head c == x then readMatch (pred ttl) s xs else readMatch (pred ttl) s l

readFull :: SerialPort -> Int -> IO B.ByteString
readFull _ 0 = return B.empty
readFull s l = do
  r <- recv s l
  rest <- readFull s $ l - B.length r
  return $ r `B.append` rest

readStatus :: SerialPort -> MessageHandler -> IO (Either String St.Status)
readStatus s _ = do
  matched <- readMatch 8 s (B.unpack statusReq)
  d <- if matched then readFull s (fromEnum St.statusLen) else return B.empty
  let pkt = LB.fromStrict $ statusReq `B.append` d
  return $! St.parse pkt

loop :: MessageHandler -> SerialPort -> IO ()
loop h s = forever $ do
  _ <- send s statusReq
  r <- readStatus s h :: IO (Either String St.Status)
  case r of
    Left ex -> putStrLn $ "exception parsing data from serial: " ++ show ex
    Right st -> h st
  threadDelay 1000000

withPort :: FilePath -> MessageHandler -> IO ()
withPort path handler =
  withSerial path defaultSerialSettings {
  commSpeed = CS19200
  , timeout = 50 } $ loop handler
