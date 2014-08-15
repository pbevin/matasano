import AES
import Conversion

main = do
  cipher <- readFile "7.txt"
  putStrLn $ show $ aesDecrypt "YELLOW SUBMARINE" $ base642bs cipher
