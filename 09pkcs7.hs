import AES

import qualified Data.ByteString.Char8 as B

main :: IO ()
main = putStrLn $ show $ pkcsPad 20 $ B.pack "YELLOW SUBMARINE"
