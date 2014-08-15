import qualified Data.ByteString as B
import Data.List
import Conversion

inGroups :: Int -> B.ByteString -> [B.ByteString]
inGroups n bs
  | B.null bs   = []
  | otherwise = xs : inGroups n ys where (xs, ys) = B.splitAt (fromIntegral n) bs

isAES ::B.ByteString -> Bool
isAES bs = blocks /= nub blocks
  where blocks = inGroups 16 bs

detectAES :: [(Int, B.ByteString)] -> (Int, B.ByteString)
detectAES bss = head $ filter (isAES . snd) bss

main = do
  file <- readFile "8.txt"
  putStrLn $ show $ detectAES $ zip [1..] $ map hex2bs $ lines file
