module Powerlab (
    crc16
  , verifyPkt
  , read1
  , read2, read2f, read2fs
  , read4
  , PktWrap, unwrap
  ) where

import Data.Bits (Bits, shiftR, xor)
import Data.Foldable (foldl')
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int64)
import Data.Binary.Get (Get, runGet, getInt16be, getWord16be, getWord32be)
import qualified Data.ByteString.Lazy as B

class PktWrap a where
  unwrap :: a -> B.ByteString

-- logical symbols since haskell doesn't have them.
(≫) :: Bits a => a -> Int -> a
(≫) = shiftR
(⊕) :: Bits a => a -> a -> a
(⊕) = xor

-- Not yet needed, but since I was typing...
-- (∧) = Data.Bits.(.&.)
-- (≪) = Data.Bits.shiftL
-- (∥) = Data.Bits.(.|.)

-- This was ported from https://github.com/dustin/powerlab/blob/master/crc.go
crc16 :: B.ByteString -> Word16
crc16 = let
  perbit a b = (a ≫ 1) ⊕ if odd (b ⊕ a) then 33800 else 0
  perbyte n b = foldl' perbit n [b ≫ x' | x' <- [0..7]] in
    B.foldl' ((. toEnum . fromEnum) . perbyte) 4742

read1 :: PktWrap t => Int64 -> t -> Word8
read1 n x = B.index (unwrap x) (4+n)

readn :: PktWrap t => Get x -> Int64 -> t -> x
readn f n l = runGet f (B.drop (n+4) $ unwrap l)

read2 :: PktWrap t => Int64 -> t -> Word16
read2 = readn getWord16be

read2fs :: PktWrap t => Int64 -> t -> Double
read2fs n x = fromIntegral $ readn getInt16be n x

read2f :: PktWrap t => Int64 -> t -> Double
read2f n x = fromIntegral $ read2 n x

read4 :: PktWrap t => Int64 -> t -> Word32
read4 = readn getWord32be

instance PktWrap B.ByteString where
  unwrap = id

verifyPkt :: (PktWrap t) => t -> Int64 -> Either String Bool
verifyPkt p n
  | B.length d /= n + 4 = Left ("invalid length: " ++ show (B.length d) ++ " want " ++ show (n + 4))
  | crc16 r == c = Right True
  | otherwise = Left ("computed crc = " ++ show (crc16 r) ++ " wanted " ++ show c)
  where d = unwrap p
        r = B.take (n - 2) $ B.drop 4 d
        c = read2 (n - 2) d
