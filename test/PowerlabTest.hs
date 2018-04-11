module PowerlabTest (tests) where

import Powerlab

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC
import Data.Either (fromLeft)

import Test.Tasty
import Test.Tasty.HUnit

e :: String -> B.ByteString
e = B.fromStrict . BC.pack

testShortPkt :: Assertion
testShortPkt = fromLeft (error "not from left") (verifyPkt B.empty 4) @?= "invalid length: 0 want 8"

testBadCRC :: Assertion
testBadCRC = fromLeft (error "not from left") (verifyPkt (e "hello") 1) @?= "computed crc = 4742 wanted 27759"

tests :: TestTree
tests = testGroup "PowerLab" [
  testCase "empty" testShortPkt,
  testCase "bad crc" testBadCRC
  ]
