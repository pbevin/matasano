import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Bits
import Data.List
import Data.Function
import Conversion

-- from 02
fixedXor :: B.ByteString -> B.ByteString -> B.ByteString
fixedXor as bs = B.pack $ zipWith xor (bytes as) (bytes bs)
  where bytes = map fromIntegral . B.unpack

solve :: B.ByteString -> [B.ByteString]
solve message = reverse $ map fst $ sortBy (compare `on` snd) $ map addScore $ tries
  where addScore try = (try, score try)
        tries = [ decodeWithKey key message | key <- [0..255] ]
        decodeWithKey key = fixedXor (B.pack $ repeat key)
        score bs = sum $ map (freq . fromIntegral) $ filter printable $ B.unpack bs
        printable ch = ch >= 32 && ch < 127
        freq ch = case (chr ch) `elemIndex` (reverse "etaoinshrdlu") of
                     Just f -> f
                     Nothing -> -1

main :: IO ()
main = putStrLn $ show $ head $ solve $ hex2bs "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
