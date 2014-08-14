import Data.Bits
import Data.Char

bitsSet :: Int -> Int
bitsSet 0 = 0
bitsSet n = 1 + (bitsSet $ n .&. (n-1))

editDistance :: String -> String -> Int
editDistance as bs = sum $ map bitsSet $ zipWith xor (map ord as) (map ord bs)
