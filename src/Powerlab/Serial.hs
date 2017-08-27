module Powerlab.Serial (
  withPort
  ) where

import Powerlab
import qualified Powerlab.Status as St
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

readStatus :: SerialPort -> MessageHandler -> IO ()
readStatus s h = do
  matched <- read_match 8 s (B.unpack statusReq)
  when matched $ return ()

  d <- recv s (fromEnum St.statusLen)
  h $ St.parse (LB.fromStrict $ statusReq `B.append` d)

loop :: MessageHandler -> SerialPort -> IO ()
loop h s = do
  send s $ statusReq
  readStatus s h
  threadDelay 1000000
  loop h s

withPort :: FilePath -> MessageHandler -> IO ()
withPort path handler =
  withSerial path defaultSerialSettings {
  commSpeed = CS19200
  , timeout = 50 } $ loop handler
