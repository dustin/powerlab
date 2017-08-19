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

crc16 :: B.ByteString -> Word16
crc16 x = let
  perbyte n b = foldl (\a x -> (a ≫ 1) ⊕ if odd (x ⊕ a) then 33800 else 0) n [(to_w32 b) ≫ x | x <- [0..7]] in
    to_w16 $ B.foldl perbyte 4742 x
