module Powerlab (
  crc16
  ) where

import Data.Bits
import Data.Word
import qualified Data.ByteString as B

to_w16 :: Enum a => a -> Word16
to_w16 = (toEnum . fromEnum)

to_w32 =( toEnum . fromEnum) :: Word8 -> Word32


crc16 :: B.ByteString -> Word16
crc16 x = to_w16 $ B.foldl stupid_bit_stuff (4742::Word32) x

stupid_bit_stuff n b =
  foldl (\a x ->
            let t = x `xor` a in
              (a `shiftR` 1) `xor` if (t `shiftR` 1) == (t-1) `shiftR` 1 then 33800 else 0
        ) n [(to_w32 b) `shiftR` x | x <- [0..7]]
