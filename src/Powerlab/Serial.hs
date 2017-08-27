module Powerlab.Serial (
  withPort
  ) where

import Powerlab
import qualified Powerlab.Status as St
import Control.Exception.Base
import Control.Concurrent
import Control.Monad

import System.Hardware.Serialport
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B

type MessageHandler = St.Status -> IO ()

statusReq = B.pack ['R', 'a', 'm', '\0']

read_match :: Int -> SerialPort -> String -> IO Bool
read_match 0 _ _ = return False
read_match _ s [] = return True
read_match ttl s l@(x:xs) = do
  c <- recv s 1
  if (B.head c) == x then read_match (pred ttl) s xs else read_match (pred ttl) s l

read_full :: SerialPort -> Int -> IO B.ByteString
read_full _ 0 = return B.empty
read_full s l = do
  r <- recv s l
  rest <- read_full s $ l - B.length r
  return $ r `B.append` rest

readStatus :: SerialPort -> MessageHandler -> IO (Either String St.Status)
readStatus s h = do
  matched <- read_match 8 s (B.unpack statusReq)
  d <- if matched then read_full s (fromEnum St.statusLen) else evaluate B.empty
  let pkt = (LB.fromStrict $ statusReq `B.append` d)
  return $ St.parse pkt

loop :: MessageHandler -> SerialPort -> IO ()
loop h s = do
  send s $ statusReq
  r <- readStatus s h :: IO (Either String St.Status)
  case r of
    Left ex -> putStrLn $ "exception parsing data from serial: " ++ (show ex)
    Right st -> h st
  threadDelay 1000000
  loop h s

withPort :: FilePath -> MessageHandler -> IO ()
withPort path handler =
  withSerial path defaultSerialSettings {
  commSpeed = CS19200
  , timeout = 50 } $ loop handler
