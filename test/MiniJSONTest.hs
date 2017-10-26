module MiniJSONTest (tests, prop_valid_chars) where

import MiniJSON

import Data.List
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC


import Test.QuickCheck
import Test.QuickCheck.Arbitrary ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (Test)


newtype JStr = JStr String

instance Show JStr where
  show (JStr a) = show a ++ " " ++ show (map fromEnum a)

instance Arbitrary JStr where
  arbitrary = do
    s <- arbitrary :: Gen String
    return $ JStr s

  shrink (JStr s) = map JStr (shrink s)

prop_valid_chars :: JStr -> Property
prop_valid_chars (JStr i) =
  let s = BC.unpack . B.toStrict $ encode i
      h = head s
      t = last s
      m = take (length s - 1) (drop 1 s) in
    collect escapes $
    classify (numescs == 0) "no escapes" $
    classify (numescs > 1) "multiple escapes" $
    (h == '"') && (t == '"') && valid m
  where valid [] = True
        valid (x:xs)
          | fromEnum x < 0x1f = False
          | x == '\\' && valid_escape xs = valid $ skip_escape xs
          | x == '\\' = False
          | otherwise = valid xs
        valid_escape (x:xs)
          | x `elem` "\"\\/bfnrt" = True
          | x == 'u' = nub (take 4 xs) \\ "0123456789AaBbCcDdEeFf" == ""
          | otherwise = False
        valid_escape _ = False
        skip_escape (x:xs)
          | x `elem` "\"\\/bfnrt" = xs
          | x == 'u' = drop 4 xs
        skip_escape _ = error "Invalid escape"
        escapes = case dropWhile (/= '\\') (BC.unpack . B.toStrict $ encode i) of
                    (_:c:_) -> [c]
                    _ -> ""
        numescs :: Int
        numescs = (length.elemIndices '\\') (BC.unpack . B.toStrict $ encode i)

tests :: [Test]
tests = [
  testProperty "valid string" prop_valid_chars
  ]
