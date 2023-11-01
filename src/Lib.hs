module Lib (readToString, getFilesContent, haveElemOf, rmOcc, isNumber, isNumberOrDot, extractBtwQuot, trim, finishWith) where


import Control.Exception (catch, IOException)
import Data.List (isPrefixOf)
import Data.Char (isDigit)


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


isNumber :: String -> Bool
isNumber []  = False
isNumber [c] = isDigit c
isNumber (c : cn)
  | isDigit c = isNumber cn
  | otherwise = False


isNumberOrDot :: String -> Bool
isNumberOrDot []  = False
isNumberOrDot inp = isNumberOrDot' False inp
  where
    isNumberOrDot' :: Bool -> String -> Bool
    isNumberOrDot' _ [] = True
    isNumberOrDot' False ('.' : cn) = isNumberOrDot' True cn
    isNumberOrDot' b ( c  : cn)
      | isDigit c = isNumberOrDot' b cn
      | otherwise = False


extractBtwQuot :: String -> Maybe String
extractBtwQuot ('"' : ms) = extractBtwQuot' [] ms
  where
    extractBtwQuot' :: String -> String -> Maybe String
    extractBtwQuot' acc ('\\' : c : next) = extractBtwQuot' (acc ++ ['\\', c]) next
    extractBtwQuot' acc ('"'  :        _) = Just acc
    extractBtwQuot' acc ( c   :     next) = extractBtwQuot' (acc ++ [c]) next
    extractBtwQuot' _ _ = Nothing
extractBtwQuot _ = Nothing


trim :: String -> String
trim = trim' False
 where
  trim' :: Bool -> String -> String
  trim' _ (' '  : next) = trim next
  trim' _ ('\n' : next) = trim next
  trim' _ ('\t' : next) = trim next
  trim' _ ('\r' : next) = trim next
  trim' False s = reverse $ trim' True $ reverse s
  trim' True  s = s


finishWith :: String -> String -> Bool
finishWith str pattern = finishWith' (reverse str) $ reverse pattern
  where
    finishWith' :: String -> String -> Bool
    finishWith' rstr rpattern = rpattern `isPrefixOf` rstr
