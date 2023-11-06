module BinHex (readBinHexa, writeBinHexa) where


import qualified Data.ByteString as B
import Data.Word (Word8)


writeBinHexa :: String -> [Int] -> IO ()
writeBinHexa filepath input =
    case intArrayToWord8Array input of
        Left   _ -> putStrLn "compilation failed, retry with -64"
        Right ii -> B.writeFile filepath $ B.pack ii

intArrayToWord8Array :: [Int] -> Either String [Word8]
intArrayToWord8Array = intArrayToWord8Array' []
  where
    intArrayToWord8Array' :: [Word8] -> [Int] -> Either String [Word8]
    intArrayToWord8Array' acc [] = Right acc
    intArrayToWord8Array' acc (i : n) =
        case intToWord8 i of
            Left  err -> Left err
            Right bi  -> intArrayToWord8Array' (acc ++ [bi]) n

    intToWord8 :: Int -> Either String Word8
    intToWord8 nbr
      | nbr < 0   = Right 0
      | 255 < nbr = Left "error"
      | otherwise = Right $ fromIntegral nbr



readBinHexa :: String -> IO [Int]
readBinHexa filepath = B.readFile filepath >>= \bytes -> return $ word8ArrayToIntArray $ B.unpack bytes

word8ArrayToIntArray :: [Word8] -> [Int]
word8ArrayToIntArray = map word8ToInt
  where
    word8ToInt :: Word8 -> Int
    word8ToInt = fromIntegral
