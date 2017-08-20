module Powerlab.Status (
  Status
  , Chemistry
  , PWMType
  , Mode
  ) where

import Data.Word
import Data.Time.Clock

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


data Status = Status { avgAmps :: Double
                     , avgCell :: Double
                     , batteryNeg :: Double
                     , batteryPos :: Double
                     , cPUTemp :: Double
                     , cRC :: Word16
                     , cellVoltages :: [Double]
                     , chargeComplete :: Bool
                     , chargeCurrent :: Double
                     , chargeDuration :: DiffTime
                     , chemistry :: Chemistry
                     , computeCRC :: Word16
                     , cycleNum :: Int
                     , detectedCellCount :: Int
                     , dischAmpSet :: Double
                     , dischargePWM :: Int
                     , errorCode :: Int
                     , fastAmps :: Double
                     , highTemp :: Bool
                     , iRs :: [Double]
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
                     , vRAmps :: Double
                     , version :: String
  }
            deriving(Show)

-- CellVoltage(n Int) Double
-- IR(cell Int) Double
-- balancePWM(cell Int) Int