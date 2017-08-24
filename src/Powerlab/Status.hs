{-# LANGUAGE OverloadedStrings #-}

module Powerlab.Status (
  Status, statusLen
  , parse, unwrap
  , Chemistry(..)
  , PWMType(..)
  , ToJSON, toJSON, object, (.=)
  , Mode(..)
  , PowerReductionReason(..)
  , version, cell, cells, ir, irs, vr_amps, avg_amps, avg_cell, battery_neg, battery_pos
  , detected_cell_count, cpu_temp, status_flags, charge_complete, chemistry
  , power_reduction_reason, charge_duration, mode, sync_pwm_drive, slaves_found
  , charge_current, supply_volts_with_current, supply_volts, supply_amps, cycle_num
  , slow_avg_amps, packs, mah_in, mah_out, discharge_amp_set, discharge_pwm, error_code
  , fast_amps, high_temp, max_cell, nicd_fallback_v, out_positive, preset
  , preset_set_amps, regen_volt_set, screen_num, start_avg, start_supply_volts, rx_status
  ) where

import Powerlab
import MiniJSON

import Data.Bits (shiftL, (.&.))
import Data.Int
import Data.Time.Clock
import Data.Word
import Data.Time
import Data.Text (Text)

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

statusLen = 149 :: Int64

parse b
    | verify_pkt b statusLen = Status b
    | otherwise = error "invalid packet"

instance ToJSON Status where
  toJSON st =
    object ["avg_amps" .= avg_amps st,
            "avg_cell" .= avg_cell st,
            "battery_neg" .= battery_neg st,
            "battery_pos" .= battery_pos st,
            "cells" .= cells st,
            "charge_complete" .= charge_complete st,
            "charge_current" .= charge_current st,
            "charge_sec" .= ((fromEnum $ charge_duration st) `div` 1000000000000),
            "charge_time" .= (show $ charge_duration st),
            "chemistry" .= chemistry st,
            "cpu_temp" .= cpu_temp st,
            "cycle_num" .= cycle_num st,
            "detected_cell_count" .= detected_cell_count st,
            "discharge_amp_set" .= discharge_amp_set st,
            "discharge_pwm" .= discharge_pwm st,
            "error_code" .= error_code st,
            "fast_amps" .= fast_amps st,
            "high_temp" .= high_temp st,
            "irs" .= irs st,
            "mah_in" .= mah_in st,
            "mah_out" .= mah_out st,
            "max_cell" .= max_cell st,
            "mode" .= mode st,
            "nicd_fallback_v" .= nicd_fallback_v st,
            "out_positive" .= out_positive st,
            "packs" .= packs st,
            "power_reduction_reason" .= power_reduction_reason st,
            "preset" .= preset st,
            "preset_set_amps" .= preset_set_amps st,
            "regen_volt_set" .= regen_volt_set st,
            "screen_num" .= screen_num st,
            "slaves_found" .= slaves_found st,
            "slow_avg_amps" .= slow_avg_amps st,
            "start_avg" .= start_avg st,
            "start_supply_volts" .= start_supply_volts st,
            "supply_amps" .= supply_amps st,
            "supply_volts" .= supply_volts st,
            "supply_volts_with_current" .= supply_volts_with_current st,
            "sync_pwm_drive" .= sync_pwm_drive st,
            "version" .= version st,
            "vr_amps" .= vr_amps st,
            "safety_charge" .= (\st -> let (r, _, _) = status_flags st in r) st,
            "reduce_amps" .= (\st -> let (_, _, r) = status_flags st in r) st,
            "discharge_running" .= (\st -> let (r, _, _, _) = rx_status st in r) st,
            "regenerative_discharge" .= (\st -> let (_, r, _, _) = rx_status st in r) st,
            "charge_running" .= (\st -> let (_, _, r, _) = rx_status st in r) st,
            "balancers_running" .= (\st -> let (_, _, _, r) = rx_status st in r) st
           ]

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

-- discharge running, regenerative discharge, charge running, balancers running
rx_status :: Status -> (Bool, Bool, Bool, Bool)
rx_status st = let b = read2 46 st in (bit b 1, bit b 4, bit b 6, bit b 7)

charge_complete :: Status -> Bool
charge_complete st = let (_, r, _) = status_flags st in r

chemistry :: Status -> Chemistry
chemistry st = (toEnum $ (fromEnum $ read1 135 st) - 1)

power_reduction_reason :: Status -> PowerReductionReason
power_reduction_reason st = (toEnum $ fromEnum $ read1 143 st)

dt_secs = 1000000000000

charge_duration :: Status -> NominalDiffTime
charge_duration st = toEnum $ (fromEnum $ read2 28 st) * dt_secs

mode :: Status -> Mode
mode = mode' . read1 133

sync_pwm_drive :: Status -> PWMType
sync_pwm_drive st
  | x > 8192 = Boost
  | otherwise = Buck
    where x = read2 18 st

slaves_found :: Status -> [Int]
slaves_found st = filter (bit b) [1..16] where b = read2 120 st

charge_current :: Status -> Double
charge_current = (/ 1666) . read2f 20

supply_volts_with_current :: Status -> Double
supply_volts_with_current st = (read2f 22 st) * 46.96 / 4095 / 16

supply_volts :: Status -> Double
supply_volts st = (read2f 24 st) * 46.96 / 4095.0

supply_amps :: Status -> Double
supply_amps = (/ 150) . read2f 80

cycle_num :: Status -> Int
cycle_num st = fromEnum $ read1 142 st

slow_avg_amps :: Status -> Double
slow_avg_amps = (/ 600) . read2f 116

packs :: Status -> Int
packs = fromEnum . read1 136

mah_in :: Status -> Int
mah_in = (`div` 2160) . fromEnum . read4 34

mah_out :: Status -> Int
mah_out = (`div` 2160) . fromEnum . read4 84

discharge_amp_set :: Status -> Double
discharge_amp_set = (/ 600) . read2f 92

discharge_pwm :: Status -> Int
discharge_pwm st = fromEnum $ read2 94 st

error_code :: Status -> Int
error_code st = fromEnum $ read1 134 st

fast_amps :: Status -> Double
fast_amps = (/ 600) . read2f 30

high_temp :: Status -> Bool
high_temp st = bit (read2 50 st) 2

max_cell :: Status -> Double
max_cell = (/ 12797) . read2f 74

nicd_fallback_v :: Status -> Double
nicd_fallback_v st = (read2f 70 st) / 12797 - (max_cell st)

out_positive :: Status -> Double
out_positive = (/ 4095) . read2f 32

preset :: Status -> Int
preset st = fromEnum $ read1 137 st

preset_set_amps :: Status -> Double
preset_set_amps = (/ 600) . read2f 118

regen_volt_set :: Status -> Double
regen_volt_set st = (read2f 90 st) * 46.96 / 4095

screen_num :: Status -> Int
screen_num st = fromEnum $ read1 139 st

start_avg :: Status -> Double
start_avg = (/ 10) . read2f 40

start_supply_volts :: Status -> Double
start_supply_volts st = (read2f 104 st) * 46.96 / 4095
