import Conversion
import Xor

main :: IO ()
main = putStrLn $ show $ head $ solveXor $ hex2bs enc
  where enc = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
