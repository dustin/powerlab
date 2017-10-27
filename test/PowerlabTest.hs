module PowerlabTest (tests) where

import Powerlab

import Data.Time
import Data.Either
import qualified Data.ByteString.Lazy as B

import Test.HUnit (Assertion, assertBool)
import Test.HUnit.Approx (assertApproxEqual)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit

testShortPkt :: Assertion
testShortPkt = assertBool "empty" $ fl (verifyPkt B.empty 4) == "invalid length"
  where fl (Left x) = x
        fl _ = error "not from left"

tests :: [Test]
tests = [testCase "" testShortPkt]
