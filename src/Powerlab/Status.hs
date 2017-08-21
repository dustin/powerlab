module Powerlab.Status (
  Status, statusLen
  , parse, unwrap
  , Chemistry
  , PWMType
  , Mode
  , version, cell, cells, iR, iRs, vRAmps, avgAmps, avgCell, batteryNeg, batteryPos
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
                     , cPUTemp :: Double
                     , cRC :: Word16
                     , cellVoltages :: [Double]
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

detectedCellCount :: Status -> Int
detectedCellCount st = fromEnum $ read1 132 st

cells :: Status -> [Double]
cells st = map (cell st) [1..(detectedCellCount st)]

iR :: Status -> Int -> Double
iR st n
  | n <= 0 || n > 8 = error "invalid cell number"
  | otherwise = let x = read2f (50 + (2* (toEnum . fromEnum)n)) st in
                  x / 6.3984 / (vRAmps st)

vRAmps :: Status -> Double
vRAmps = (/ 600) . read2f 68

iRs :: Status -> [Double]
iRs st = map (iR st) [1..(detectedCellCount st)]

avgAmps :: Status -> Double
avgAmps = (/ 600) . read2f 42

avgCell :: Status -> Double
avgCell = (/ 10) . read2f 38

batteryNeg :: Status -> Double
batteryNeg = (* (46.96 / 4095)) . read2f 100

batteryPos :: Status -> Double
batteryPos = (/ 12797) . read2f 82
