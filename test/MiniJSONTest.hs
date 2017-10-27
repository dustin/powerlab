module MiniJSONTest (tests, prop_valid_chars, parseJSONStr) where

import MiniJSON

import Data.List
import Text.Read (readEither)
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

parseJSONStr :: String -> Either String String
parseJSONStr s
  | head s == '"' && last s == '"' = sequence $ chars m
  | otherwise = Left "missing quotes"
  where m = take (length s - 2) (drop 1 s)
        chars :: String -> [Either String Char]
        chars []                    = []
        chars ('\\':'\\':xs)        = Right '\\' : chars xs
        chars ('\\':'n':xs)         = Right '\n' : chars xs
        chars ('\\':'r':xs)         = Right '\r' : chars xs
        chars ('\\':'b':xs)         = Right '\b' : chars xs
        chars ('\\':'f':xs)         = Right '\f' : chars xs
        chars ('\\':'t':xs)         = Right '\t' : chars xs
        chars ('\\':'/':xs)         = Right '/' : chars xs
        chars ('\\':'"':xs)         = Right '"' : chars xs
        chars ('\\':'u':a:b:c:d:xs) = parseHex [a,b,c,d] : chars xs
        chars a@('\\':_)            = [Left ("error near \\" ++ take 3 a)]
        chars (x:xs)                = Right x : chars xs
        parseHex :: String -> Either String Char
        parseHex n = toEnum <$> (readEither ("0x" ++ n) :: Either String Int)

prop_valid_chars :: JStr -> Property
prop_valid_chars (JStr i) =
  let m = take (length s - 1) (drop 1 s) in
    collect escapes $
    classify (numescs == 0) "no escapes" $
    classify (numescs > 1) "multiple escapes" $
    (head s == '"') && (last s == '"') && valid m
  where s = BC.unpack . B.toStrict $ encode i
        valid [] = True
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
        escapes = case dropWhile (/= '\\') s of
                    (_:c:_) -> [c]
                    _ -> ""
        numescs = (length.elemIndices '\\') s

prop_str_roundtrips :: JStr -> Bool
prop_str_roundtrips (JStr i) = case parseJSONStr (BC.unpack . B.toStrict $ encode i) of
                                 Left x -> error x
                                 Right i' -> i == i'

tests :: [Test]
tests = [
  testProperty "valid string" prop_valid_chars,
  testProperty "round t rips" prop_str_roundtrips
  ]
