import Xor
import Conversion

main :: IO ()
main = do
  putStrLn $ bs2hex $ repeatingXor (text2bs "ICE") (text2bs stanza)

stanza :: String
stanza = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
