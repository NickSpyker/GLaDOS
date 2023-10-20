module Lib (readToString, getFilesContent, haveElemOf, rmOcc) where


import Control.Exception (catch, IOException)


readToString :: String -> IO (Either String String)
readToString filepath =
  (Right <$> readFile filepath) `catch` (\e -> return $ Left $ show (e :: IOException))


getFilesContent :: [String] -> IO (Either String [String])
getFilesContent paths =
  sequenceA <$> traverse readToString paths


haveElemOf :: [String] -> [String] -> Bool
haveElemOf [] _ = False
haveElemOf (value : next) ls
  | value `elem` ls = True
  | otherwise = haveElemOf next ls


rmOcc :: [String] -> [String] -> [String]
rmOcc [] _ = []
rmOcc (value : next) occ
  | value `elem` occ = rmOcc next occ
  | otherwise = value : rmOcc next occ
