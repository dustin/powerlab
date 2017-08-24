module MiniJSON where

import Data.Maybe
import Data.Time
import Data.List (intercalate)
import Data.Text (Text, unpack)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC

e :: String -> B.ByteString
e = B.fromStrict . BC.pack

class ToJSON a where
  toJSON :: a -> B.ByteString

  toJSONList :: [a] -> B.ByteString

  toJSONList x = (e "[") `B.append` (B.intercalate (e ", ") $ map toJSON x) `B.append` (e "]")

instance ToJSON Char where
  toJSON x = toJSONList [x]
  toJSONList s = e $ "\"" ++ s ++ "\""

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
  toJSON Nothing = B.fromStrict $ BC.pack "null"
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
