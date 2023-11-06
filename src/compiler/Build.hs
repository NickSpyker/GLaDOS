module Build (save, load) where


import Instruction (Prog, Data(..), Instruction(..), Insts)
import Bin64 (readBin64, writeBin64)
import Data.Char (ord, chr)


getMagic :: [Int]
getMagic = [ 0, 33, 71, 76, 97, 68, 79, 83, 0]
----------- \0  !   G   L   a   D   O   S  \0


save :: String -> Prog -> IO ()
save filepath program =
  case getAllBin [] program of
    Left  err    -> putStrLn err
    Right resBin -> writeBin64 filepath resBin >> putStrLn "build success"


getAllBin :: [Int] -> [(String, Insts)] -> Either String [Int]
getAllBin acc [        ] = Right $ getMagic ++ acc
getAllBin acc (m : next) =
  case getModuleBin m of
    Left  err  -> Left err
    Right mbin -> getAllBin (acc ++ mbin) next
  where
    getModuleBin :: (String, Insts) -> Either String [Int]
    getModuleBin (moduleName, insts) =
      case instsToBinary False [] insts of
        Left  err -> Left err
        Right bin -> Right $ [0, 0] ++ stringToIntArray moduleName ++ [0, 0] ++ bin ++ [0] -- end with [0, 0]


load :: String -> IO (Prog)
load filepath = readBin64 filepath >>= checkMagic


checkMagic :: [Int] -> IO (Prog)
checkMagic (0 : 33 : 71 : 76 : 97 : 68 : 79 : 83 : 0 : next) =
  case binParser next of
    Left  err  -> putStrLn ("Binary error:" ++ err) >> return []
    Right prog -> return (prog)
checkMagic _ = return []


-- ### Start of Bin parsers ### --


binParser :: [Int] -> Either String Prog
binParser = binParser' []
  where
    binParser' :: Prog -> [Int] -> Either String Prog
    binParser' acc [ ] = Right acc
    binParser' acc bin =
      case parseModule bin of
        Left err -> Left err
        Right (moduleBytecode, ne) -> binParser' (acc ++ [moduleBytecode]) ne


parseModule :: [Int] -> Either String ((String, Insts), [Int])
parseModule (0 : 0 : maybeModule) =
  case parseModuleName [] maybeModule of
    Left   err            -> Left err
    Right (modName, body) ->
      case parseBody [] body of
        Left   err                 -> Left $ "\nin module \"" ++ modName ++ "\" parsing:\n" ++ err
        Right (insts, otherModule) -> Right ((modName, insts), otherModule)
parseModule _ = Left "invalid module"


parseModuleName :: [Int] -> [Int] -> Either String (String, [Int])
parseModuleName acc (0 : 0 : 0 : next) = Right (intArratToString acc, next)
parseModuleName acc (c     : next) = parseModuleName (acc ++ [c]) next
parseModuleName _ _ = Left "wrong module name format"


parseBody :: Insts -> [Int] -> Either String (Insts, [Int])
parseBody acc (0:2:3:5:7:11:13:17:19:23:0:next) = Right (acc, next)
parseBody acc [0,0] = Right (acc, [])
parseBody acc bin =
    case parseInstruction bin of
        Nothing     -> Left $ "cannot parse section in module,\n\nalredy have:\n" ++ show acc ++ "\n\nbin:\n" ++ show bin
        Just (i, n) -> parseBody (acc ++ i) n


type BinParser = [Int] -> Maybe (Insts, [Int])


parseInstruction :: BinParser
parseInstruction (301 :        next) = Just ([Call],      next)
parseInstruction (302 :        next) = Just ([Ret],       next)
parseInstruction (303 :        next) = Just ([PrintTop],  next)
parseInstruction (305 : code : next) = Just ([Exit code], next)
parseInstruction (306 : next) =
    case getData next of
        Nothing -> Nothing
        Just (d, n) -> Just ([Push d], n)
parseInstruction (307 : nb : next) = Just ([JumpIfFalse nb], next)
parseInstruction (308 : nb : next) = Just ([PushFromArg nb], next)
parseInstruction (309 : next) =
    case fetchString [] next of
        Nothing -> Nothing
        Just (name, nn) ->
            case parseBody [] nn of
                Left _ -> Nothing
                Right (insts, nnn) -> Just ([SaveToEnv name insts], nnn)
parseInstruction (310 : next) =
    case fetchString [] next of
        Nothing -> Nothing
        Just (name, nn) -> Just ([PushFromEnv name], nn)
parseInstruction _ = Nothing


fetchString :: [Int] -> [Int] -> Maybe (String, [Int])
fetchString acc (0 : next) = Just (intArratToString acc, next)
fetchString acc (c : next) = fetchString (acc ++ [c]) next
fetchString _ _ = Nothing


getData :: [Int] -> Maybe (Data, [Int])
getData (101 : i : d : next) = Just (Float (buildFloat i d), next)
getData (102 : 0 : 0 : next) = Just (Bool False, next)
getData (102 : 1 : 0 : next) = Just (Bool True,  next)
getData (103 : c : 0 : next) = Just (Char (chr c), next)
getData (104 : i : 0 : next) = Just (Int i, next)
getData (201 : next) = Just (ArrayStart, next)
getData (202 : next) = Just (ArrayEnd, next)
getData (203 : next) = Just (Add, next)
getData (204 : next) = Just (Sub, next)
getData (205 : next) = Just (Mul, next)
getData (206 : next) = Just (Div, next)
getData (207 : next) = Just (Mod, next)
getData (208 : next) = Just (And, next)
getData (209 : next) = Just (Not, next)
getData (210 : next) = Just (Or, next)
getData (211 : next) = Just (Gt, next)
getData (212 : next) = Just (Lt, next)
getData (213 : next) = Just (Eq, next)
getData _ = Nothing


buildFloat :: Int -> Int -> Float
buildFloat x y = buildFloat' (fromIntegral x) (fromIntegral y)
    where
        buildFloat' :: Float -> Float -> Float
        buildFloat' fx fy
            | fy < 1.0 = fx + fy
            | otherwise = buildFloat' fx (fy / 10)


-- ### End of Bin parsers ### --


splitFloat :: Float -> (Int, Int)
splitFloat f =
  let (intPart, fracPart) = properFraction f
      intStr = show intPart
      fracStr = drop 2 $ show fracPart
  in (read intStr, read fracStr)


stringToIntArray :: String -> [Int]
stringToIntArray [] = [0]
stringToIntArray (c : next) = ord c : stringToIntArray next


intArratToString :: [Int] -> String
intArratToString = map chr


------------
-- Converter
---- type:        10x v v
---- operation:   20x
---- instruction: 30x v v
-------
-- Info
---- end of an string: 0


instsToBinary :: Bool -> [Int] -> Insts -> Either String [Int]
instsToBinary True  acc [        ] = Right $ acc ++ [0,2,3,5,7,11,13,17,19,23,0]
instsToBinary False acc [        ] = Right $ acc ++ [0]
instsToBinary vvvvv acc (c : next) =
  case instructionToBin c of
    Left  err -> Left err
    Right bin -> instsToBinary vvvvv (acc ++ bin) next


instructionToBin :: Instruction -> Either String [Int]
instructionToBin  Call            = Right [301]
instructionToBin  Ret             = Right [302]
instructionToBin  PrintTop        = Right [303]
instructionToBin  PrintStack      = Right [304]
instructionToBin (Exit code)      = Right [305, code]
instructionToBin (Push dt)        =
  case dataToBin dt of
    Left  err -> Left err
    Right val -> Right $ [306] ++ val
instructionToBin (JumpIfFalse nb) = Right [307, nb]
instructionToBin (PushFromArg nb) = Right [308, nb]
instructionToBin (SaveToEnv s ii) =
  case instsToBinary True [] ii of
    Left  err -> Left err
    Right bin -> Right $ [309] ++ stringToIntArray s ++ bin
instructionToBin (PushFromEnv s)  = Right $ [310] ++ stringToIntArray s ++ [0]


dataToBin :: Data -> Either String [Int]
-- types
dataToBin (Float    f) = let (int, dec) = splitFloat f in Right [101, int, dec]
dataToBin (Bool False) = Right [102, 0,        0]
dataToBin (Bool  True) = Right [102, 1,        0]
dataToBin (Char  char) = Right [103, ord char, 0]
dataToBin (Int    nbr) = Right [104, nbr,      0]
-- operations
dataToBin  ArrayStart  = Right [201]
dataToBin  ArrayEnd    = Right [202]
dataToBin  Add         = Right [203]
dataToBin  Sub         = Right [204]
dataToBin  Mul         = Right [205]
dataToBin  Div         = Right [206]
dataToBin  Mod         = Right [207]
dataToBin  And         = Right [208]
dataToBin  Not         = Right [209]
dataToBin  Or          = Right [210]
dataToBin  Gt          = Right [211]
dataToBin  Lt          = Right [212]
dataToBin  Eq          = Right [213]
dataToBin  (Fun _)     = Left  "Unhandled compilation error, see \"./src/compiler/Build.hs\""
