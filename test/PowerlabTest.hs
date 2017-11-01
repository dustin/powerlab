module PowerlabTest (tests) where

import Powerlab

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC

import Test.Tasty
import Test.Tasty.HUnit

e :: String -> B.ByteString
e = B.fromStrict . BC.pack

testShortPkt :: Assertion
testShortPkt = fl (verifyPkt B.empty 4) @?= "invalid length: 0 want 8"
  where fl (Left x) = x
        fl _ = error "not from left"

testBadCRC :: Assertion
testBadCRC = fl (verifyPkt (e "hello") 1) @?= "computed crc = 4742 wanted 27759"
  where fl (Left x) = x
        fl _ = error "not from left"

tests :: TestTree
tests = testGroup "PowerLab" [
  testCase "empty" testShortPkt,
  testCase "bad crc" testBadCRC
  ]
