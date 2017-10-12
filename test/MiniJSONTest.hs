module MiniJSONTest (tests) where

import MiniJSON

import Data.List
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC


import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

newtype JStr = JStr String

instance Show JStr where
  show (JStr a) = show a ++ " " ++ show (map fromEnum a)

instance Arbitrary JStr where
  arbitrary = do
    s <- arbitrary :: Gen String
    return $ JStr $ "\"" ++ s ++ "\""

prop_valid_chars (JStr i) =
  let s = BC.unpack . B.toStrict $ encode i
      h = head s
      t = last s
      m = take (length s - 1) (drop 1 s) in
    (h == '"') && (t == '"') && valid m
  where valid [] = True
        valid (x:xs)
          | fromEnum x < 0x1f = False
          | x == '\\' && valid_escape xs = valid $ skip_escape xs
          | otherwise = valid xs
        valid_escape (x:xs)
          | x `elem` "\"\\/bfnrt" = True
          | x == 'u' = take 4 xs \\ "012345689AaBbCcDdEeFf" == ""
          | otherwise = False
        skip_escape (x:xs)
          | x `elem` "\"\\/bfnrt" = xs
          | x == 'u' = drop 4 xs

tests = [
  testProperty "valid string" (prop_valid_chars :: JStr -> Bool)
  ]


