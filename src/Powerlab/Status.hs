module Powerlab.Status (
  Status, statusLen
  , parse, unwrap
  , Chemistry
  , PWMType
  , Mode
  , version, cell, cells, ir, irs, vr_amps, avg_amps, avg_cell, battery_neg, battery_pos
  , detected_cell_count, cpu_temp
  ) where

import Data.Word
import Data.Int
import Data.Time.Clock

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
                            deriving(Show, Eq)

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
               deriving(Show, Eq)

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
                     , chargeComplete :: Bool
                     , chargeCurrent :: Double
                     , chargeDuration :: DiffTime
                     , chemistry :: Chemistry
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
                     , powerReductionReason :: PowerReductionReason
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
