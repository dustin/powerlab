module StatusTest (tests, exemplar) where

import Powerlab
import qualified Powerlab.Status as St

import Data.Word
import Data.List
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)

import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.Framework (defaultMainWithOpts, interpretArgsOrExit, testGroup)
import Test.Framework.Runners.Options
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

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
  0,    -- Chemistry -- 135
  6,    -- Packs -- 136
  0,    -- Preset
  0,    -- unused
  0,    -- Screen Number
  0, 0, -- unused
  0,       -- CycleNumber
  0,       -- PowerReductionReason
  0, 0, 0, -- unused
  0x09, 0xf5 -- Checksum
  ]

exemplarSt = St.parse exemplar

approxl [] [] = True
approxl (a:as) (b:bs)
  | abs (a - b) > 0.001 = False
  | otherwise = approxl as bs

exemplarTests = [
  testCase "version" $ St.version exemplarSt @?= "3.14",
  testCase "cell(1)" $ assertApproxEqual "cell(1)" 0.001 4.2 $ St.cell exemplarSt 1,
  testCase "cells"   $ assertBool "cells" (approxl [4.2, 3.7, 0] $ St.cells exemplarSt),
  testCase "IRs"     $ assertBool "IRs" (approxl [10.3, 0, 0] $ St.iRs exemplarSt)
                ]

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

tests = [
  testCase "verify exemplar" (verify_pkt exemplar St.statusLen @?= True),
  testCase "verify captured" (verify_pkt capturedExemplar St.statusLen @?= True),
  testGroup "validate exemplar" exemplarTests
  ]
