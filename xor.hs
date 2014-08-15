module Xor (fixedXor, solveXor, repeatingXor) where

import qualified Data.ByteString as B
import Data.Bits
import Data.Char
import Data.List
import Data.Function

fixedXor :: B.ByteString -> B.ByteString -> B.ByteString
fixedXor as bs = B.pack $ zipWith xor (bytes as) (bytes bs)
  where bytes = map fromIntegral . B.unpack

repeatingXor :: B.ByteString -> B.ByteString -> B.ByteString
repeatingXor key text = fixedXor (B.cycle key) text

type Score = Int
solveXor :: B.ByteString -> [(B.ByteString, Score)]
solveXor message = reverse $ sortBy (compare `on` snd) $ map addScore $ tries
  where addScore try = (try, score try)
        tries = [ decodeWithKey key message | key <- [0..255] ]
        decodeWithKey key = fixedXor (B.pack $ repeat key)
        score bs = sum $ map (freq . fromIntegral) $ filter printable $ B.unpack bs
        printable ch = ch >= 32 && ch < 127
        freq ch = case (toLower $ chr ch) `elemIndex` (reverse "etaonrishdlfcmugypwbvkjxqz ") of
                     Just f -> f + 1
                     Nothing -> 0
