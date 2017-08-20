module Powerlab (
    crc16
  , verify_pkt
  , read1
  , read2
  ) where

import Data.Bits
import Data.Word
import qualified Data.ByteString as B

to_w16 = (toEnum . fromEnum) :: Word8 -> Word16

-- logical symbols since haskell doesn't have them.
(≫) = shiftR
(≪) = shiftL
(⊕) = xor
(∥) = (.|.)

-- Not yet needed, but since I was typing...
-- (∧) = (.&.)

-- This was ported from https://github.com/dustin/powerlab/blob/master/crc.go
crc16 :: B.ByteString -> Word16
crc16 x = let
  perbit a b = (a ≫ 1) ⊕ if odd (b ⊕ a) then 33800 else 0
  perbyte n b = foldl perbit n [b ≫ x | x <- [0..7]] in
    B.foldl ((. to_w16) . perbyte) 4742 x

read1 :: Int -> B.ByteString -> Word8
read1 n x = B.index x (4+n)

read2 :: Int -> B.ByteString -> Word16
read2 n x = let bits = B.unpack $ B.take 2 (B.drop (n + 4) x)
                a = head bits
                b = head (drop 1 bits) in
              ((to_w16 a) ≪ 8) ∥ (to_w16 b)

statusLen = 149

verify_pkt :: B.ByteString -> Bool
verify_pkt d
  | B.length d /= statusLen + 4 = False
  | crc16 r == c = True
  | otherwise = error ("computed crc = " ++ (show $ crc16 r) ++ " wanted " ++ (show c))
  where r = B.take (statusLen - 2) $ B.drop 4 d
        c = read2 (statusLen - 2) d
