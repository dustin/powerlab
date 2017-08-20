module Powerlab (
    crc16
  , verify_pkt
  , read1
  , read2
  , read2s
  , read4
  ) where

import Data.Bits
import Data.Word
import Data.Int
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B

to_w16 = (toEnum . fromEnum) :: Word8 -> Word16

-- logical symbols since haskell doesn't have them.
(≫) = Data.Bits.shiftR
(⊕) = Data.Bits.xor

-- Not yet needed, but since I was typing...
-- (∧) = Data.Bits.(.&.)
-- (≪) = Data.Bits.shiftL
-- (∥) = Data.Bits.(.|.)

-- This was ported from https://github.com/dustin/powerlab/blob/master/crc.go
crc16 :: B.ByteString -> Word16
crc16 x = let
  perbit a b = (a ≫ 1) ⊕ if odd (b ⊕ a) then 33800 else 0
  perbyte n b = foldl perbit n [b ≫ x | x <- [0..7]] in
    B.foldl ((. to_w16) . perbyte) 4742 x

read1 :: Int64 -> B.ByteString -> Word8
read1 n x = B.index x (4+n)

readn :: (Get x) -> Int64 -> B.ByteString -> x
readn f n l = flip runGet (B.drop (n+4) l) $ do w <- f; return w

read2 :: Int64 -> B.ByteString -> Word16
read2 = readn getWord16be

read2s :: Int64 -> B.ByteString -> Int16
read2s = readn getInt16be

read4 :: Int64 -> B.ByteString -> Word32
read4 = readn getWord32be

statusLen = 149 :: Int64

verify_pkt :: B.ByteString -> Bool
verify_pkt d
  | B.length d /= statusLen + 4 = False
  | crc16 r == c = True
  | otherwise = error ("computed crc = " ++ (show $ crc16 r) ++ " wanted " ++ (show c))
  where r = B.take (statusLen - 2) $ B.drop 4 d
        c = read2 (statusLen - 2) d
