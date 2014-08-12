module Conversion (hex2bs, bs2base64, bs2hex) where

import Numeric
import Data.Char
import Data.List
import qualified Data.ByteString as B
import Data.Word8

splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy n xs = take n xs : splitBy n (drop n xs)

hex2ints :: String -> [Word8]
hex2ints = map hex2int . splitBy 2
  where hex2int [a,b] = fromIntegral $ 16 * (digitToInt a) + (digitToInt b)

hex2bs :: String -> B.ByteString
hex2bs = B.pack . hex2ints

bs2hex :: B.ByteString -> String
bs2hex = concat . map toHex . map fromIntegral . B.unpack
  where toHex n = showHex n ""

bs2base64 :: B.ByteString -> String
bs2base64 = concat . map toBase64 . splitBy 3 . map fromIntegral . B.unpack

enc64 :: Int -> Char
enc64 = ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" !!)

toBase64 :: [Int] -> String
toBase64 [a]     = (take 2 $ toBase64 [a,0,0]) ++ "=="
toBase64 [a,b]   = (take 3 $ toBase64 [a,b,0]) ++ "="
toBase64 [a,b,c] = map enc64 [w, x, y, z]
  where w = a `div` 4
        x = a `mod` 4 * 16 + b `div` 16
        y = b `mod` 16 * 4 + c `div` 64
        z = c `mod` 64
