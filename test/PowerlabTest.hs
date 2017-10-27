module PowerlabTest (tests) where

import Powerlab

import Data.Time
import Data.Either
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC

import Test.HUnit (Assertion, (@?=))
import Test.HUnit.Approx (assertApproxEqual)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit

e :: String -> B.ByteString
e = B.fromStrict . BC.pack

testShortPkt :: Assertion
testShortPkt = fl (verifyPkt B.empty 4) @?= "invalid length"
  where fl (Left x) = x
        fl _ = error "not from left"

testBadCRC :: Assertion
testBadCRC = fl (verifyPkt (e "hello") 1) @?= "computed crc = 4742 wanted 27759"
  where fl (Left x) = x
        fl _ = error "not from left"

tests :: [Test]
tests = [
  testCase "empty" testShortPkt,
  testCase "bad crc" testBadCRC
  ]
