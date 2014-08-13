module Xor (fixedXor, solveXor) where

import qualified Data.ByteString.Lazy as B
import Data.Bits
import Data.Char
import Data.List
import Data.Function

fixedXor :: B.ByteString -> B.ByteString -> B.ByteString
fixedXor as bs = B.pack $ zipWith xor (bytes as) (bytes bs)
  where bytes = map fromIntegral . B.unpack

type Score = Int
solveXor :: B.ByteString -> [(B.ByteString, Score)]
solveXor message = reverse $ sortBy (compare `on` snd) $ map addScore $ tries
  where addScore try = (try, score try)
        tries = [ decodeWithKey key message | key <- [0..255] ]
        decodeWithKey key = fixedXor (B.pack $ repeat key)
        score bs = sum $ map (freq . fromIntegral) $ filter printable $ B.unpack bs
        printable ch = ch >= 32 && ch < 127
        freq ch = case (chr ch) `elemIndex` (reverse "etaoinshrdlu") of
                     Just f -> f
                     Nothing -> -1
