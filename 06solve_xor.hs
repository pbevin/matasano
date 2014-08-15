import qualified Data.ByteString.Lazy as B
import Data.Word8
import Data.Bits
import Data.Char
import Data.List
import Data.Function
import Conversion

bitsSet :: (Bits a, Num a) => a -> Int
bitsSet 0 = 0
bitsSet n = 1 + (bitsSet $ n .&. (n-1))

editDistance :: B.ByteString -> B.ByteString -> Int
editDistance as bs = sum $ map bitsSet $ B.zipWith xor as bs

ed :: String -> String -> Int
ed = editDistance `on` text2bs

bestKeyLength :: B.ByteString -> Int
bestKeyLength as = fst $ minimumBy (compare `on` snd) $ eds as [2..40]

eds :: B.ByteString -> [Int] -> [(Int, Int)]
eds as lens = map addEditDistance $ [2..40]
  where addEditDistance len = (len, 100 * (editDistance xs ys) `div` len)
          where xs = B.take n as
                ys = B.take n (B.drop n as)
                n = fromIntegral len

main = do
  enc <- readFile "6.txt"
  putStrLn $ show $ eds (base642bs enc) [2..80]
  putStrLn $ show $ bestKeyLength (base642bs enc)
