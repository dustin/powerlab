module Powerlab.Status (
  Status, statusLen
  , parse, unwrap
  , Chemistry(..)
  , PWMType(..)
  , Mode(..)
  , PowerReductionReason(..)
  , version, cell, cells, ir, irs, vr_amps, avg_amps, avg_cell, battery_neg, battery_pos
  , detected_cell_count, cpu_temp, status_flags, charge_complete, chemistry
  , power_reduction_reason
  ) where

import Data.Bits (shiftL, (.&.))
import Data.Int
import Data.Time.Clock
import Data.Word

import Powerlab

import qualified Data.ByteString.Lazy as B

data PowerReductionReason = FullPowerAllowed
                          | InputCurrentLimit
                          | SixtyAmpInputCurrentLimitReached
                          | CellSumErrorCharge
                          | SupplyNoise
                          | HighTemp
                          | LowInputVoltage
                          | ConstantVoltageOutput
                          | InternalMax100WDischarge
                          | HighTempDischarge
                          | RegenMaxAmpsReached
                          | HighTempDischarge11
                          | CellSumErrorDischarge
                          | RegenVoltLimitReached
                          | DischargeReduced
                          | Reduce
                          | SupplyLow
                            deriving(Show, Enum, Eq)

data PWMType = Buck | Boost deriving (Show, Eq)

data Chemistry = LiPo
               | LiIon
               | A123
               | LiMang
               | LiCo
               | NiCd
               | NiMh
               | Pb
               | LiFE
               | Primary
               | PowerSupply
               deriving(Show, Enum, Eq)

data Mode = Unknown         -- = Mode(-1)
          | Ready           -- = Mode(0)
          | DetectingPack   -- = Mode(1)
          | Charging        -- = Mode(6)
          | TrickleCharging -- = Mode(7)
          | Discharging     -- = Mode(8)
          | Monitoring      -- = Mode(9)
          | HaltForSafety   -- = Mode(10)
          | PackCoolDown    -- = Mode(11)
          | SystemStopError -- = Mode(99)
          deriving(Show, Eq, Enum)

newtype Status = Status B.ByteString

instance PktWrap Status where
  unwrap (Status b) = b

statusLen = 149 :: Int64

parse b
    | verify_pkt b statusLen = Status b
    | otherwise = error "invalid packet"

{-
data Status = Status {
                     , chargeCurrent :: Double
                     , chargeDuration :: DiffTime
                     , computeCRC :: Word16
                     , cycleNum :: Int
                     , dischAmpSet :: Double
                     , dischargePWM :: Int
                     , errorCode :: Int
                     , fastAmps :: Double
                     , highTemp :: Bool
                     , mAhIn :: Int
                     , mAhOut :: Int
                     , maxCell :: Double
                     , mode :: Mode
                     , niCdFallbackV :: Double
                     , outPositive :: Double
                     , packs :: Int
                     , preset :: Int
                     , presetSetAmps :: Double
                     , regenVoltSet :: Double
                     , screenNum :: Int
                     , slavesFound :: [Int]
                     , slowAvgAmps :: Double
                     , startAvg :: Double
                     , startSupplyVolts :: Double
                     , supplyAmps :: Double
                     , supplyVolts :: Double
                     , supplyVoltsWithCurrent :: Double
                     , syncPWMDrive :: PWMType
  }
            deriving(Show)
-}

version :: Status -> String
version xs = let v = read2 0 (unwrap xs) in (show $ v `div` 100) ++ "." ++ (show $ v `mod` 100)

cell :: Status -> Int -> Double
cell st n
  | n <= 0 || n > 8 = error "invalid cell number"
  | otherwise = let x = read2f (2* (toEnum . fromEnum)n) st in x * 5.12 / 65535

detected_cell_count :: Status -> Int
detected_cell_count st = fromEnum $ read1 132 st

cells :: Status -> [Double]
cells st = map (cell st) [1..(detected_cell_count st)]

ir :: Status -> Int -> Double
ir st n
  | n <= 0 || n > 8 = error "invalid cell number"
  | otherwise = let x = read2f (50 + (2* (toEnum . fromEnum)n)) st in
                  x / 6.3984 / (vr_amps st)

vr_amps :: Status -> Double
vr_amps = (/ 600) . read2f 68

irs :: Status -> [Double]
irs st = map (ir st) [1..(detected_cell_count st)]

avg_amps :: Status -> Double
avg_amps = (/ 600) . read2f 42

avg_cell :: Status -> Double
avg_cell = (/ 10) . read2f 38

battery_neg :: Status -> Double
battery_neg = (* (46.96 / 4095)) . read2f 100

battery_pos :: Status -> Double
battery_pos = (/ 12797) . read2f 82

cpu_temp :: Status -> Double
cpu_temp st = (2.5*(read2f 26 st)/4095 - 0.986) / 0.00355

-- Weird one-based bit thing from the powerlab spec
bit :: Word16 -> Int -> Bool
bit b n = (b .&. (1 `shiftL` (16 - n))) /= 0

-- safetyCharge, chargeComplete, reduceAmps
status_flags :: Status -> (Bool, Bool, Bool)
status_flags st = let b = read2 44 st in (bit b 1, bit b 8, bit b 11)

charge_complete :: Status -> Bool
charge_complete st = let (_, r, _) = status_flags st in r

chemistry :: Status -> Chemistry
chemistry st = (toEnum $ (fromEnum $ read1 135 st) - 1)

power_reduction_reason :: Status -> PowerReductionReason
power_reduction_reason st = (toEnum $ fromEnum $ read1 143 st)
