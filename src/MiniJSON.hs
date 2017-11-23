{-# LANGUAGE OverloadedStrings #-}

module MiniJSON (ToJSON(..), object, encode, (.=)) where

import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.String (fromString)
import Numeric (showHex)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

esc :: String -> B.ByteString
esc [] = B.empty
esc (x:xs)
  | x == '\n' = b 'n'
  | x == '\\' = b '\\'
  | x == '\r' = b 'r'
  | x == '/'  = b '/'
  | x == '\f' = b 'f'
  | x == '\t' = b 't'
  | x == '"'  = b '"'
  | d < 0x1f  = bs '\\' <> bs 'u' <> leftpad (showHex d "") <> esc xs
  | otherwise = bs x <> esc xs
  where b c = bs '\\' <> bs c <> esc xs
        leftpad :: String -> B.ByteString
        leftpad s
          | length s == 4 = fromString s
          | otherwise = leftpad $ '0' : s
        bs = BC.singleton
        d = fromEnum x

class ToJSON a where
  toJSON :: a -> B.ByteString

  toJSONList :: [a] -> B.ByteString

  toJSONList x = "[" <> B.intercalate ", " (map toJSON x) <> "]"

instance ToJSON Char where
  toJSON x = toJSONList [x]
  toJSONList s = BC.singleton '"' <> esc s <> BC.singleton '"'

instance ToJSON UTCTime where
  toJSON = toJSON . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

instance ToJSON Text where
  toJSON s = "\"" <> esc (unpack s) <> "\""

bcshow :: Show s => s -> B.ByteString
bcshow = fromString.show

instance ToJSON Double where
  toJSON d
    | isNaN d = nil
    | d == 1/0 = nil
    | d == -1/0 = nil
    | otherwise = bcshow d
    where nil = "null"

instance ToJSON Int where
  toJSON = bcshow

instance ToJSON Integer where
  toJSON = bcshow

instance ToJSON a => ToJSON [a] where
  toJSON = toJSONList

instance ToJSON Bool where
  toJSON True = "true"
  toJSON False = "false"

instance ToJSON t => ToJSON (Maybe t) where
  toJSON Nothing = "null"
  toJSON (Just t) = toJSON t

data JSONPair = JSONPair B.ByteString B.ByteString

instance ToJSON JSONPair where
  toJSON (JSONPair l r) = l <> ": " <> r

object :: [JSONPair] -> B.ByteString
object l = "{" <> B.intercalate ", " (map toJSON l) <> "}"

(.=) :: ToJSON t => String -> t -> JSONPair
(.=) k v = JSONPair (toJSON k) (toJSON v)

encode :: ToJSON t => t -> B.ByteString
encode = toJSON
