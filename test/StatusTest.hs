{-# OPTIONS_GHC -Wno-orphans #-}

module StatusTest (tests, exemplar) where

import Powerlab
import qualified Powerlab.Status as St

import Data.Time
import Data.Either
import Data.Semigroup ((<>))
import Data.Maybe (isJust)
import Data.Aeson (decode, encode, Value)
import Data.Word (Word8)
import Data.Binary.Put (runPut, putWord16be)
import qualified Data.ByteString.Lazy as B

import Test.QuickCheck
import Test.QuickCheck.Arbitrary ()
import Test.HUnit.Approx (assertApproxEqual)
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

exemplar :: B.ByteString
exemplar = B.pack [
  82, 97, 109, 0,-- 'R', 'a', 'm', 0,
  1, 0x3a, -- Version
  0xd1, 0xff, 0xb8, 0xff, -- cells 1, 2
  0, 0, 0, 0, -- cells 3, 4
  0, 0, 0, 0, -- cells 5, 6
  0, 0, 0, 0, -- cells 7, 8
  0x1f, 0xff, -- pwm type
  0x32, 0xc2, -- charge current
  0x41, 0x66, -- supply volts with current
  0x04, 0x17, -- supply volts
  0x07, 0x26, -- CPU Temp
  0x07, 0x08, -- charge sec
  0x12, 0x48, -- fast amps
  0xc9, 0x8d, -- output postive
  0, 0x26, 0xa0, 0xc0, -- mAh in
  0x1, 0xf4, -- avg cell
  0x1, 0x2c, -- start avg
  0x12, 0x48, -- avg amps
  0x80, 0x20, -- status flags
  0x92, 0, -- rx status
  0, 0, -- unused
  0x40, 0, -- status2
  0x2, 0x2, 0, 0, -- ir 1, ir 2
  0, 0, 0, 0, -- ir 3, ir 4
  0, 0, 0, 0, -- ir 5, ir 6
  0, 0, 0, 0, -- ir 7, ir 8
  0x12, 0x48, -- VRamps -- 68
  0xee, 0xf5, -- NiCdFallbackV
  0, 0, -- unused
  0xcd, 0x74, -- MaxCell volts -- 74-75 -- Volts = 16bit / 12797
  0x11, 0, -- status6 -- 76-77
  0, 0, -- ChgMin -- 78-79
  0, 0, -- supply amps -- 80-81
  0, 0, -- battery pos -- 82-83
  0, 0, 0, 0, -- AHr Out -- 84-87
  0, 0, -- unused -- 88-89
  0, 0, -- RegenVoltSet -- 90-91
  0, 0, -- DischAmpsSet -- 92-93
  0, 0, -- DischargePWM -- 94-95
  0, 0, 0, 0, -- unused -- 96-99
  0, 0, -- BatNeg -- 100-101
  0, 0, -- unused -- 102-103
  0, 0, -- start supply volts -- 104-105
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -- unused -- 106-115
  0, 0, -- SlowAvgAmps -- 116-117
  0, 0, -- PresetSetAmps -- 118-119
  0, 0, -- SlavesFound -- 120-121
  0, 0, -- unused -- 122-123
  0, 0, 0, 0, 0, 0, 0, 0, -- Bal*PWM -- 124-131
  3,    -- DetectedCellCount -- 132
  0,    -- Mode -- 133
  0,    -- ErrorCode -- 134
  1,    -- Chemistry -- 135
  6,    -- Packs -- 136
  0,    -- Preset
  0,    -- unused
  0,    -- Screen Number
  0, 0, -- unused
  0,       -- CycleNumber
  0,       -- PowerReductionReason
  0, 0, 0, -- unused
  0x8c, 0xa0 -- Checksum
  ]

exemplarSt :: St.Status
exemplarSt = case St.parse exemplar of
               Left ex -> error ex
               Right st -> st

capturedSt :: St.Status
capturedSt = case St.parse capturedExemplar of
               Left ex -> error ex
               Right st -> st

instance Arbitrary St.Status where
  arbitrary = do
    let stuff = [lit [82, 97, 109, 0],                    -- "Ram\0"
                 lit [1, 0x3a],                           -- version
                 someBytes 4,                             -- cells 1, 2
                 someBytes 4,                             -- cells 3, 4
                 someBytes 4,                             -- cells 5, 6
                 someBytes 4,                             -- cells 7, 8
                 someBytes 2,                             -- pwm type
                 someBytes 2,                             -- charge current
                 someBytes 2,                             -- supply volts with current
                 someBytes 2,                             -- supply volts
                 someBytes 2,                             -- CPU Temp
                 someBytes 2,                             -- charge sec
                 someBytes 2,                             -- fast amps
                 someBytes 2,                             -- output postive
                 cho (0, 127), someBytes 3,               -- mAh in
                 someBytes 2,                             -- avg cell
                 someBytes 2,                             -- start avg
                 someBytes 2,                             -- avg amps
                 someBytes 2,                             -- status flags
                 someBytes 2,                             -- rx status
                 lit [0, 0],                              -- unused
                 someBytes 2,                             -- status2
                 someBytes 4,                             -- ir 1, ir 2
                 someBytes 4,                             -- ir 3, ir 4
                 someBytes 4,                             -- ir 5, ir 6
                 someBytes 4,                             -- ir 7, ir 8
                 someBytes 2,                             -- VRamps -- 68
                 someBytes 2,                             -- NiCdFallbackV
                 lit [0, 0],                              -- unused
                 someBytes 2,                             -- MaxCell volts -- 74-75 -- Volts = 16bit / 12797
                 someBytes 2,                             -- status6 -- 76-77
                 someBytes 2,                             -- ChgMin -- 78-79
                 someBytes 2,                             -- supply amps -- 80-81
                 someBytes 2,                             -- battery pos -- 82-83
                 cho (0, 127), someBytes 3,               -- AHr Out -- 84-87
                 lit [0, 0],                              -- unused -- 88-89
                 someBytes 2,                             -- RegenVoltSet -- 90-91
                 someBytes 2,                             -- DischAmpsSet -- 92-93
                 someBytes 2,                             -- DischargePWM -- 94-95
                 lit [0, 0, 0, 0],                        -- unused -- 96-99
                 someBytes 2,                             -- BatNeg -- 100-101
                 lit [0, 0],                              -- unused -- 102-103
                 someBytes 2,                             -- start supply volts -- 104-105
                 lit [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],      -- unused -- 106-115
                 someBytes 2,                             -- SlowAvgAmps -- 116-117
                 someBytes 2,                             -- PresetSetAmps -- 118-119
                 someBytes 2,                             -- SlavesFound -- 120-121
                 lit [0, 0],                              -- unused -- 122-123
                 someBytes 8,                             -- Bal*PWM -- 124-131
                 cho (1, 6),                              -- DetectedCellCount -- 132
                 cho (1, 9),                              -- Mode -- 133
                 oneOf [0, 1, 6, 7, 8, 9, 10, 11, 99],     -- ErrorCode -- 134
                 cho (1, 11),                             -- Chemistry -- 135
                 cho (1, 255),                            -- Packs -- 136
                 someBytes 1,                             -- Preset
                 lit [0],                                 -- unused
                 someBytes 1,                             -- Screen Number
                 lit [0, 0],                              -- unused
                 someBytes 1,                             -- CycleNumber
                 cho (0, 16),                             -- PowerReductionReason
                 lit [0, 0, 0]] :: [Gen [Word8]]          -- unused
    ws <- sequence stuff
    let bs = B.pack $ concat ws
    let c = (bytes.crc16) (B.drop 4 bs)
    pure $ must.St.parse $ bs <> c
      where lit :: [Word8] -> Gen [Word8]
            lit = sequence . map pure
            someBytes :: Int -> Gen [Word8]
            someBytes n = vectorOf n arbitrary
            cho :: (Word8,Word8) -> Gen [Word8]
            cho x = vectorOf 1 (choose x)
            oneOf :: [Word8] -> Gen [Word8]
            oneOf v = vectorOf 1 (elements v)
            bytes w = runPut (putWord16be w)
            must (Left x) = error x
            must (Right x) = x

approxl :: (Fractional a, Ord a) => [a] -> [a] -> Bool
approxl [] [] = True
approxl (a:as) (b:bs)
  | abs (a - b) > 0.001 = False
  | otherwise = approxl as bs
approxl _ _ = error "Length mismatch"

ε :: Double
ε = 0.001

duration :: Integer -> DiffTime
duration = secondsToDiffTime

exemplarTests :: Assertion
exemplarTests = mapM_ id [
  St.version exemplarSt @?= "3.14",
  assertApproxEqual "cell(1)" ε 4.2 $ St.cell exemplarSt 1,
  assertBool "cells" (approxl [4.2, 3.7, 0] $ St.cells exemplarSt),
  assertBool "IRs" (approxl [10.3, 0, 0] $ St.irs exemplarSt),
  assertApproxEqual "avg amps" ε (1.3*6) $ St.avgAmps exemplarSt,
  assertApproxEqual "avg cell" ε 50 $ St.avgCell exemplarSt,
  assertApproxEqual "bat neg" ε 0.080 $ St.batteryNeg capturedSt,
  assertApproxEqual "bat pos" ε 1.306 $ St.batteryPos capturedSt,
  assertApproxEqual "CPU temp" ε 36.962 $ St.cpuTemp exemplarSt,
  assertBool "charging" $ not $ St.chargeComplete exemplarSt,
  St.chemistry capturedSt @?= St.LiPo,
  St.powerReductionReason exemplarSt @?= St.FullPowerAllowed,
  St.chargeDuration exemplarSt @?= (duration 30 * 60),
  St.mode capturedSt @?= St.Charging,
  St.mode exemplarSt @?= St.Ready,
  St.syncPwmDrive exemplarSt @?= St.Buck,
  St.slavesFound exemplarSt @?= [],
  assertApproxEqual "charge current" ε (1.3*6) $ St.chargeCurrent exemplarSt,
  assertApproxEqual "charge current 2" ε 0.5402 $ St.chargeCurrent capturedSt,
  assertApproxEqual "supply V with A" ε 12 $ St.supplyVoltsWithCurrent exemplarSt,
  assertApproxEqual "supply V with A 2" ε 12.359 $ St.supplyVoltsWithCurrent capturedSt,
  assertApproxEqual "supply V" ε 12.1786 $ St.supplyVolts capturedSt,
  assertApproxEqual "supply A" ε 0 $ St.supplyAmps capturedSt,
  assertApproxEqual "slow avg amps" ε 0 $ St.slowAvgAmps capturedSt,
  assertApproxEqual "discharge amp set" ε 0 $ St.dischargeAmpSet exemplarSt,
  assertApproxEqual "fast amps" ε (1.3*6) $ St.fastAmps exemplarSt,
  assertApproxEqual "max cell" ε 4.11 $ St.maxCell exemplarSt,
  assertApproxEqual "nicd fallback v" ε 0.67 $ St.nicdFallbackV exemplarSt,
  assertApproxEqual "out positive" ε (4.2*3) $ St.outPositive exemplarSt,
  assertApproxEqual "preset set amps" ε 1.5 $ St.presetSetAmps capturedSt,
  assertApproxEqual "regen volt set" ε 0 $ St.regenVoltSet exemplarSt,
  assertApproxEqual "start avg" ε 30 $ St.startAvg exemplarSt,
  assertApproxEqual "start supply volts" ε 12.362 $ St.startSupplyVolts capturedSt,
  St.cycleNum exemplarSt @?= 0,
  St.packs exemplarSt @?= 6,
  St.mahIn exemplarSt @?= 1172,
  St.mahOut exemplarSt @?= 0,
  St.dischargePwm exemplarSt @?= 0,
  St.errorCode exemplarSt @?= 0,
  St.preset capturedSt @?= 2,
  St.screenNum capturedSt @?= 2,
  St.statusFlags exemplarSt @?= (True, False, True),
  St.rxStatus exemplarSt @?= (True, True, False, True),
  assertBool "not high temp" $ not $ St.highTemp capturedSt,
  assertBool "high temp" $ St.highTemp exemplarSt
  ]

capturedExemplar :: B.ByteString
capturedExemplar = B.pack [
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

propArbitaryJSON :: St.Status -> Bool
propArbitaryJSON x = isJust (decode (encode x) :: Maybe Value)

propRoundTrip :: (Show a, Read a, Eq a) => a -> Bool
propRoundTrip x = x == (read.show) x

tests :: TestTree
tests = testGroup "Status" [
  testCase "verify exemplar" (isRight (verifyPkt exemplar St.statusLen) @?= True),
  testCase "verify captured" (isRight (verifyPkt capturedExemplar St.statusLen) @?= True),
  testCase "validate exemplar" exemplarTests,

  testProperty "arbitary JSON" propArbitaryJSON,
  testProperty "round trip status string" (propRoundTrip :: St.Status -> Bool)
  ]
