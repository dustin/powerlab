module Powerlab (
  crc16
  ) where

import Data.Bits
import Data.Word
import qualified Data.ByteString as B

to_w16 = (toEnum . fromEnum) :: Enum a => a -> Word16
to_w32 = (toEnum . fromEnum) :: Enum a => a -> Word32

-- logical symbols since haskell doesn't have them.
(≫) = shiftR
(⊕) = xor

-- Not yet needed, but since I was typing...
-- (≪) = shiftL
-- (∧) = (.&.)
-- (∥) = (.|.)

-- This was ported from https://github.com/dustin/powerlab/blob/master/crc.go
crc16 :: B.ByteString -> Word16
crc16 x = let
  perbit a b = (a ≫ 1) ⊕ if odd (b ⊕ a) then 33800 else 0
  perbyte n b = foldl perbit n [b ≫ x | x <- [0..7]] in
    to_w16 $ B.foldl ((. to_w32) . perbyte) 4742 x
