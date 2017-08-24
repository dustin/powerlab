module MiniJSON (ToJSON(..), object, encode, (.=)) where

import Data.Char (intToDigit)
import Data.List (intercalate)
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Time
import Numeric (showIntAtBase)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

e :: String -> B.ByteString
e = BC.pack

esc :: String -> B.ByteString
esc [] = B.empty
esc (x:xs)
  | x == '\n' = b 'n'  xs
  | x == '\\' = b '\\' xs
  | x == '\r' = b 'r'  xs
  | x == '/'  = b '/'  xs
  | x == '\f' = b 'f'  xs
  | x == '\t' = b 't'  xs
  | x == '"'  = b '"'  xs
  | d < 0x1f  = (bs '\\') `B.append` (bs 'u') `B.append` leftpad (showIntAtBase 16 intToDigit d "") `B.append` (esc xs)
  | otherwise = (bs x) `B.append` esc xs
  where b c xs = (bs '\\') `B.append` (bs c) `B.append` esc xs
        leftpad :: String -> B.ByteString
        leftpad s
          | length s == 4 = e s
          | otherwise = leftpad $ '0' : s
        bs = BC.singleton
        d = fromEnum x

class ToJSON a where
  toJSON :: a -> B.ByteString

  toJSONList :: [a] -> B.ByteString

  toJSONList x = (e "[") `B.append` (B.intercalate (e ", ") $ map toJSON x) `B.append` (e "]")

instance ToJSON Char where
  toJSON x = toJSONList [x]
  toJSONList s = (BC.singleton '"') `B.append` (esc s) `B.append` (BC.singleton '"')

instance ToJSON UTCTime where
  toJSON = toJSON . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

instance ToJSON Text where
  toJSON s = (e "\"") `B.append` (e $ unpack s) `B.append` (e "\"")

instance ToJSON Double where
  toJSON d = e $ if isNaN d then "null" else show d

instance ToJSON Int where
  toJSON d = e $ show d

instance ToJSON a => ToJSON [a] where
  toJSON = toJSONList

instance ToJSON Bool where
  toJSON True = e "true"
  toJSON False = e "true"

instance ToJSON t => ToJSON (Maybe t) where
  toJSON Nothing = e "null"
  toJSON (Just t) = toJSON t

data JSONPair = JSONPair B.ByteString B.ByteString

instance ToJSON JSONPair where
  toJSON (JSONPair l r) = l `B.append` (e ": ") `B.append` r

object :: [JSONPair] -> B.ByteString
object l = (e "{") `B.append` (B.intercalate (e ", ") $ map toJSON l) `B.append` (e "}")

(.=) :: ToJSON t => String -> t -> JSONPair
(.=) k v = JSONPair (toJSON k) (toJSON v)

encode :: ToJSON t => t -> B.ByteString
encode t = toJSON t
