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

(+++) = B.append

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
  | d < 0x1f  = (bs '\\') +++ (bs 'u') +++ leftpad (showIntAtBase 16 intToDigit d "") +++ (esc xs)
  | otherwise = (bs x) +++ esc xs
  where b c xs = (bs '\\') +++ (bs c) +++ esc xs
        leftpad :: String -> B.ByteString
        leftpad s
          | length s == 4 = e s
          | otherwise = leftpad $ '0' : s
        bs = BC.singleton
        d = fromEnum x

class ToJSON a where
  toJSON :: a -> B.ByteString

  toJSONList :: [a] -> B.ByteString

  toJSONList x = (e "[") +++ (B.intercalate (e ", ") $ map toJSON x) +++ (e "]")

instance ToJSON Char where
  toJSON x = toJSONList [x]
  toJSONList s = (BC.singleton '"') +++ (esc s) +++ (BC.singleton '"')

instance ToJSON UTCTime where
  toJSON = toJSON . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

instance ToJSON Text where
  toJSON s = (e "\"") +++ (e $ unpack s) +++ (e "\"")

instance ToJSON Double where
  toJSON d = e $ if isNaN d then "null" else show d

instance ToJSON Int where
  toJSON d = e $ show d

instance ToJSON Integer where
  toJSON d = e $ show d

instance ToJSON a => ToJSON [a] where
  toJSON = toJSONList

instance ToJSON Bool where
  toJSON True = e "true"
  toJSON False = e "false"

instance ToJSON t => ToJSON (Maybe t) where
  toJSON Nothing = e "null"
  toJSON (Just t) = toJSON t

data JSONPair = JSONPair B.ByteString B.ByteString

instance ToJSON JSONPair where
  toJSON (JSONPair l r) = l +++ (e ": ") +++ r

object :: [JSONPair] -> B.ByteString
object l = (e "{") +++ (B.intercalate (e ", ") $ map toJSON l) +++ (e "}")

(.=) :: ToJSON t => String -> t -> JSONPair
(.=) k v = JSONPair (toJSON k) (toJSON v)

encode :: ToJSON t => t -> B.ByteString
encode t = toJSON t
