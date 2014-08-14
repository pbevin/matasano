import Conversion
import Xor
import Data.List
import Data.Function

main :: IO ()
main = do
  txt <- readFile "4.txt"
  let xs = reverse . sortBy (compare `on` snd) . map (head . solveXor . hex2bs) $ lines txt
  putStrLn $ show $ head xs


