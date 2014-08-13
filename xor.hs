module Xor (fixedXor) where

import qualified Data.ByteString.Lazy as B
import Data.Bits
import Data.Char

fixedXor :: B.ByteString -> B.ByteString -> B.ByteString
fixedXor as bs = B.pack $ zipWith xor (bytes as) (bytes bs)
  where bytes = map fromIntegral . B.unpack
