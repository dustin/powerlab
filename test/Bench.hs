import Powerlab

import qualified Data.ByteString as B

import Criterion
import Criterion.Main

testData = B.pack [108,180,233,208,195,11,228,20,129,16,113,67,174,120,47,137,145,128,240,223,218,199,130,211,220,113,252,181,18]

bench_crc16 = bench "test data" $ whnf crc16 testData

main = defaultMain [bench_crc16]
