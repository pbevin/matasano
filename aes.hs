module AES (aesDecrypt) where

import qualified Data.ByteString.Char8 as B
import Crypto.Cipher.AES

aesDecrypt :: String -> B.ByteString -> B.ByteString
aesDecrypt key ciphertext = decryptECB aes ciphertext
  where aes = initAES $ B.pack key
