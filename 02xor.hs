import Xor
import Conversion

main :: IO ()
main = do
  putStrLn $ bs2hex $ fixedXor (hex2bs "1c0111001f010100061a024b53535009181c") (hex2bs "686974207468652062756c6c277320657965")
