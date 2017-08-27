module Powerlab (
    crc16
  , verify_pkt
  , read1
  , read2, read2s, read2f
  , read4
  , PktWrap, unwrap
  ) where

import Data.Bits
import Data.Word
import Data.Int
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC

class PktWrap a where
  unwrap :: a -> B.ByteString

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

read1 :: PktWrap t => Int64 -> t -> Word8
read1 n x = B.index (unwrap x) (4+n)

readn :: PktWrap t => (Get x) -> Int64 -> t -> x
readn f n l = flip runGet (B.drop (n+4) $ unwrap l) $ do w <- f; return w

read2 :: PktWrap t => Int64 -> t -> Word16
read2 = readn getWord16be

read2s :: PktWrap t => Int64 -> t -> Int16
read2s = readn getInt16be

read2f :: PktWrap t => Int64 -> t -> Double
read2f n x = fromIntegral $ read2 n x

read4 :: PktWrap t => Int64 -> t -> Word32
read4 = readn getWord32be

instance PktWrap B.ByteString where
  unwrap x = x

verify_pkt :: (PktWrap t) => t -> Int64 -> Bool
verify_pkt p n
  | B.length d /= n + 4 = False
  | crc16 r == c = True
  | otherwise = error ("computed crc = " ++ (show $ crc16 r) ++ " wanted " ++ (show c))
  where d = unwrap p
        r = B.take (n - 2) $ B.drop 4 d
        c = read2 (n - 2) d
