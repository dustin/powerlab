module Powerlab.Serial (
  withPort
  ) where

import Powerlab
import qualified Powerlab.Status as St
import Control.Concurrent

import System.Hardware.Serialport
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B

type MessageHandler = St.Status -> IO ()

statusReq = B.pack ['R', 'a', 'm', '\0']

readStatus :: SerialPort -> MessageHandler -> IO ()
readStatus s h = do
  d <- recv s (fromEnum St.statusLen)
  h $ St.parse (LB.fromStrict d)

loop :: MessageHandler -> SerialPort -> IO ()
loop h s = do
  send s $ statusReq
  cmd <- recv s 4
  if cmd == statusReq then ( readStatus s h ) else (error $ "misread: " ++ (show cmd))
  threadDelay 1000000
  loop h s

withPort :: FilePath -> MessageHandler -> IO ()
withPort path handler =
  withSerial path defaultSerialSettings {
  commSpeed = CS19200
  , timeout = 50 } $ loop handler
