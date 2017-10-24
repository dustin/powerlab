{-# LANGUAGE OverloadedStrings #-}

module Powerlab.Status (
  Status, statusLen
  , parse, unwrap
  , Chemistry(..)
  , PWMType(..)
  , ToJSON, toJSON, object, (.=)
  , Mode(..)
  , PowerReductionReason(..)
  , version, cell, cells, ir, irs, vrAmps, avgAmps, avgCell, batteryNeg, batteryPos
  , detectedCellCount, cpuTemp, statusFlags, chargeComplete, chemistry
  , powerReductionReason, chargeDuration, mode, syncPwmDrive, slavesFound
  , chargeCurrent, supplyVoltsWithCurrent, supplyVolts, supplyAmps, cycleNum
  , slowAvgAmps, packs, mahIn, mahOut, dischargeAmpSet, dischargePwm, errorCode
  , fastAmps, highTemp, maxCell, nicdFallbackV, outPositive, preset
  , presetSetAmps, regenVoltSet, screenNum, startAvg, startSupplyVolts, rxStatus
  ) where

import Powerlab
import MiniJSON

import Data.Bits (shiftL, (.&.))
import Data.Int
import Data.Time.Clock
import Data.Word

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

instance ToJSON PowerReductionReason where toJSON a = toJSON (show a)

data PWMType = Buck | Boost deriving (Show, Eq)

instance ToJSON PWMType where toJSON a = toJSON (show a)

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

instance ToJSON Chemistry where
  toJSON a = toJSON (show a)

data Mode = Unknown
          | Ready
          | DetectingPack
          | Charging
          | TrickleCharging
          | Discharging
          | Monitoring
          | HaltForSafety
          | PackCoolDown
          | SystemStopError
          deriving(Show, Eq)

instance ToJSON Mode where toJSON a = toJSON (show a)

mode' :: Word8 -> Mode
mode' 0  = Ready
mode' 1  = DetectingPack
mode' 6  = Charging
mode' 7  = TrickleCharging
mode' 8  = Discharging
mode' 9  = Monitoring
mode' 10 = HaltForSafety
mode' 11 = PackCoolDown
mode' 99 = SystemStopError
mode' _  = Unknown

newtype Status = Status B.ByteString

instance PktWrap Status where
  unwrap (Status b) = b

statusLen :: Int64
statusLen = 149

parse :: B.ByteString -> Either String Status
parse b = case verifyPkt b statusLen of
            Left x -> Left x
            Right True -> Right $ Status b
            Right False -> error "Right false is unhandled in status parsing"

-- pico is 1/thismany
pico :: Integer
pico = 1000000000000

instance ToJSON Status where
  toJSON st =
    object ["avg_amps"                   .= avgAmps st,
            "avg_cell"                   .= avgCell st,
            "battery_neg"                .= batteryNeg st,
            "battery_pos"                .= batteryPos st,
            "cells"                      .= cells st,
            "charge_complete"            .= chargeComplete st,
            "charge_current"             .= chargeCurrent st,
            "charge_sec"                 .= (diffTimeToPicoseconds (chargeDuration st) `div` pico),
            "charge_time"                .= show (chargeDuration st),
            "chemistry"                  .= chemistry st,
            "cpu_temp"                   .= cpuTemp st,
            "cycle_num"                  .= cycleNum st,
            "detected_cell_count"        .= detectedCellCount st,
            "discharge_amp_set"          .= dischargeAmpSet st,
            "discharge_pwm"              .= dischargePwm st,
            "error_code"                 .= errorCode st,
            "fast_amps"                  .= fastAmps st,
            "high_temp"                  .= highTemp st,
            "irs"                        .= irs st,
            "mah_in"                     .= mahIn st,
            "mah_out"                    .= mahOut st,
            "max_cell"                   .= maxCell st,
            "mode"                       .= mode st,
            "nicd_fallback_v"            .= nicdFallbackV st,
            "out_positive"               .= outPositive st,
            "packs"                      .= packs st,
            "power_reduction_reason"     .= powerReductionReason st,
            "preset"                     .= preset st,
            "preset_set_amps"            .= presetSetAmps st,
            "regen_volt_set"             .= regenVoltSet st,
            "screen_num"                 .= screenNum st,
            "slaves_found"               .= slavesFound st,
            "slow_avg_amps"              .= slowAvgAmps st,
            "start_avg"                  .= startAvg st,
            "start_supply_volts"         .= startSupplyVolts st,
            "supply_amps"                .= supplyAmps st,
            "supply_volts"               .= supplyVolts st,
            "supply_volts_with_current"  .= supplyVoltsWithCurrent st,
            "sync_pwm_drive"             .= syncPwmDrive st,
            "version"                    .= version st,
            "vr_amps"                    .= vrAmps st,
            "safety_charge"              .= (\st' -> let (r, _, _) = statusFlags st' in r) st,
            "reduce_amps"                .= (\st' -> let (_, _, r) = statusFlags st' in r) st,
            "discharge_running"          .= (\st' -> let (r, _, _, _) = rxStatus st' in r) st,
            "regenerative_discharge"     .= (\st' -> let (_, r, _, _) = rxStatus st' in r) st,
            "charge_running"             .= (\st' -> let (_, _, r, _) = rxStatus st' in r) st,
            "balancers_running"          .= (\st' -> let (_, _, _, r) = rxStatus st' in r) st
           ]

version :: Status -> String
version xs = let v = read2 0 (unwrap xs) in show (v `div` 100) ++ "." ++ show (v `mod` 100)

cell :: Status -> Int -> Double
cell st n
  | n <= 0 || n > 8 = error "invalid cell number"
  | otherwise = let x = read2f (2* (toEnum . fromEnum)n) st in x * 5.12 / 65535

detectedCellCount :: Status -> Int
detectedCellCount st = fromEnum $ read1 132 st

cells :: Status -> [Double]
cells st = map (cell st) [1..(detectedCellCount st)]

ir :: Status -> Int -> Double
ir st n
  | n <= 0 || n > 8 = error "invalid cell number"
  | otherwise = let x = read2f (50 + (2* (toEnum . fromEnum)n)) st in
                  x / 6.3984 / vrAmps st

vrAmps :: Status -> Double
vrAmps = (/ 600) . read2f 68

irs :: Status -> [Double]
irs st = map (ir st) [1..(detectedCellCount st)]

avgAmps :: Status -> Double
avgAmps = (/ 600) . read2f 42

avgCell :: Status -> Double
avgCell = (/ 10) . read2f 38

batteryNeg :: Status -> Double
batteryNeg = (* (46.96 / 4095)) . read2f 100

batteryPos :: Status -> Double
batteryPos = (/ 12797) . read2f 82

cpuTemp :: Status -> Double
cpuTemp st = (2.5*(read2f 26 st)/4095 - 0.986) / 0.00355

-- Weird one-based bit thing from the powerlab spec
bit :: Word16 -> Int -> Bool
bit b n = (b .&. (1 `shiftL` (16 - n))) /= 0

-- safetyCharge, chargeComplete, reduceAmps
statusFlags :: Status -> (Bool, Bool, Bool)
statusFlags st = let b = read2 44 st in (bit b 1, bit b 8, bit b 11)

-- discharge running, regenerative discharge, charge running, balancers running
rxStatus :: Status -> (Bool, Bool, Bool, Bool)
rxStatus st = let b = read2 46 st in (bit b 1, bit b 4, bit b 6, bit b 7)

chargeComplete :: Status -> Bool
chargeComplete st = let (_, r, _) = statusFlags st in r

chemistry :: Status -> Chemistry
chemistry st = toEnum $ fromEnum (read1 135 st) - 1

powerReductionReason :: Status -> PowerReductionReason
powerReductionReason st = toEnum $ fromEnum $ read1 143 st

chargeDuration :: Status -> DiffTime
chargeDuration st = secondsToDiffTime $ toEnum . fromEnum $ read2 28 st

mode :: Status -> Mode
mode = mode' . read1 133

syncPwmDrive :: Status -> PWMType
syncPwmDrive st
  | x > 8192 = Boost
  | otherwise = Buck
    where x = read2 18 st

slavesFound :: Status -> [Int]
slavesFound st = filter (bit b) [1..16] where b = read2 120 st

chargeCurrent :: Status -> Double
chargeCurrent = (/ 1666) . read2f 20

supplyVoltsWithCurrent :: Status -> Double
supplyVoltsWithCurrent st = (read2f 22 st) * 46.96 / 4095 / 16

supplyVolts :: Status -> Double
supplyVolts st = (read2f 24 st) * 46.96 / 4095.0

supplyAmps :: Status -> Double
supplyAmps = (/ 150) . read2f 80

cycleNum :: Status -> Int
cycleNum st = fromEnum $ read1 142 st

slowAvgAmps :: Status -> Double
slowAvgAmps = (/ 600) . read2f 116

packs :: Status -> Int
packs = fromEnum . read1 136

mahIn :: Status -> Int
mahIn = (`div` 2160) . fromEnum . read4 34

mahOut :: Status -> Int
mahOut = (`div` 2160) . fromEnum . read4 84

dischargeAmpSet :: Status -> Double
dischargeAmpSet = (/ 600) . read2f 92

dischargePwm :: Status -> Int
dischargePwm st = fromEnum $ read2 94 st

errorCode :: Status -> Int
errorCode st = fromEnum $ read1 134 st

fastAmps :: Status -> Double
fastAmps = (/ 600) . read2f 30

highTemp :: Status -> Bool
highTemp st = bit (read2 50 st) 2

maxCell :: Status -> Double
maxCell = (/ 12797) . read2f 74

nicdFallbackV :: Status -> Double
nicdFallbackV st = (read2f 70 st) / 12797 - maxCell st

outPositive :: Status -> Double
outPositive = (/ 4095) . read2f 32

preset :: Status -> Int
preset st = fromEnum $ read1 137 st

presetSetAmps :: Status -> Double
presetSetAmps = (/ 600) . read2f 118

regenVoltSet :: Status -> Double
regenVoltSet st = (read2f 90 st) * 46.96 / 4095

screenNum :: Status -> Int
screenNum st = fromEnum $ read1 139 st

startAvg :: Status -> Double
startAvg = (/ 10) . read2f 40

startSupplyVolts :: Status -> Double
startSupplyVolts st = (read2f 104 st) * 46.96 / 4095
