module AES (aesDecrypt, pkcsPad) where

import qualified Data.ByteString.Char8 as B
import Crypto.Cipher.AES
import Data.Char

aesDecrypt :: String -> B.ByteString -> B.ByteString
aesDecrypt key ciphertext = decryptECB aes ciphertext
  where aes = initAES $ B.pack key

pkcsPad :: Int -> B.ByteString -> B.ByteString
pkcsPad len inp
  | B.length inp >= len = inp
  | otherwise           = B.append inp $ padding (len - B.length inp)
     where padding n = B.replicate n $ chr n
