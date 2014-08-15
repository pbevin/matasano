module Conversion (hex2bs, bs2base64, base642bs, bs2hex, text2bs) where

import Numeric
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.ByteString.Lazy as B
import Data.Word8
import Text.Printf

splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy n xs = take n xs : splitBy n (drop n xs)

hex2ints :: String -> [Word8]
hex2ints = map hex2int . splitBy 2
  where hex2int [a,b] = fromIntegral $ 16 * (digitToInt a) + (digitToInt b)

hex2bs :: String -> B.ByteString
hex2bs = B.pack . hex2ints

text2bs :: String -> B.ByteString
text2bs = B.pack . map (fromIntegral . ord)

bs2hex :: B.ByteString -> String
bs2hex = concat . map toHex . map fromIntegral . B.unpack

toHex :: Int -> String
toHex = printf "%02x"

bs2base64 :: B.ByteString -> String
bs2base64 = concat . map toBase64 . splitBy 3 . map fromIntegral . B.unpack

base642bs :: String -> B.ByteString
base642bs = B.pack . concat . map fromBase64 . splitBy 4 . map fromIntegral . map fromJust . filter isJust . map (`elemIndex` base64chars)

enc64 :: Int -> Char
enc64 = (base64chars !!)

base64chars :: String
base64chars  = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

toBase64 :: [Int] -> String
toBase64 [a]     = (take 2 $ toBase64 [a,0,0]) ++ "=="
toBase64 [a,b]   = (take 3 $ toBase64 [a,b,0]) ++ "="
toBase64 [a,b,c] = map enc64 [w, x, y, z]
  where w = a `div` 4
        x = a `mod` 4 * 16 + b `div` 16
        y = b `mod` 16 * 4 + c `div` 64
        z = c `mod` 64

fromBase64 :: Integral a => [a] -> [a]
-- fromBase64 [a, b, '=', '='] = fromBase64 [a, b]
fromBase64 [a, b] = take 1 $ fromBase64 [a, b, 0, 0]
fromBase64 [a, b, c] = take 2 $ fromBase64 [a, b, c, 0]
fromBase64 [a, b, c, d] = [x, y, z]
  where x = a * 4 + (b `div` 16)
        y = (b `mod` 16) * 16 + (c `div` 4)
        z = (c `mod` 4) * 64 + d
