module Bin64 (writeBin64, readBin64) where


import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary (encode, decode)
import Data.Word (Word64)


writeBin64 :: String -> [Int] -> IO ()
writeBin64 filepath input =
  B.writeFile filepath $ BL.toStrict $ encode $ intArrayToWord64Array input
  where
    intArrayToWord64Array :: [Int] -> [Word64]
    intArrayToWord64Array = map fromIntegral


readBin64 :: String -> IO [Int]
readBin64 filepath =
  B.readFile filepath >>= \bytes -> return $ word64ArrayToIntArray $ decode $ BL.fromStrict bytes
  where 
    word64ArrayToIntArray :: [Word64] -> [Int]
    word64ArrayToIntArray = map fromIntegral
