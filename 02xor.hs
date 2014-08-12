import qualified Data.ByteString.Lazy as B
import Data.Bits
import Conversion

fixedXor :: B.ByteString -> B.ByteString -> B.ByteString
fixedXor as bs = B.pack $ zipWith xor (bytes as) (bytes bs)
  where bytes = map fromIntegral . B.unpack

main :: IO ()
main = do
  putStrLn $ bs2hex $ fixedXor (hex2bs "1c0111001f010100061a024b53535009181c") (hex2bs "686974207468652062756c6c277320657965")
