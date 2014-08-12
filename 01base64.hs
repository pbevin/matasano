import Conversion

hex2base64 :: String -> String
hex2base64 = bs2base64 . hex2bs

main :: IO ()
main = do
  putStrLn $ hex2base64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
