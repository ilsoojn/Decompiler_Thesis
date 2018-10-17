module OtherFunction where

import Data.Bits
import Data.Bool
import Data.Char
import Data.DeriveTH
import Data.List
import Data.List.Split
import Data.List.Utils hiding (split)
import Data.Maybe
import Data.String.Utils hiding (split)
import Data.Strings
import Data.Tuple
import Data.Typeable
import Numeric

import Debug.Trace
import Text.Regex.Posix

import Lists
import RegexFunction
import StatementInstr



{- Architecture Instructions related -}

getInstructionType ::  String -> String
getInstructionType op
  | isNothing found_op = "none"
  | otherwise = fromJust found_op
  where found_op = lookup op (map swap instructions)

toSym ::  String -> String
toSym op
  | isNothing symOP = "call"
  | otherwise = fromJust symOP
  where symOP = lookup op (arithmetic ++ condition)

{-************** Other Regex Functions **************-}

isAequalB :: String -> Bool
isAequalB s = (not.null) (s =~ regex_ab :: [[String]])

getB :: String -> String
getB s = unwords $ map (head.tail) (s =~ regex_ab :: [[String]])

{-************** remove character or substring from String **************-}

rmChar :: String -> String -> String
rmChar ch str = (filter.flip notElem) ch str

rmStr :: String -> String -> String
rmStr st str = unwords $ splitOn st str

{-************** Pop Front & Back **************-}

popFirst :: [String] -> (String, [String])
popFirst x = (strip $ head x, map strip $ tail x)
popLast :: [String] -> (String, [String])
popLast x = (strip $ last x, map strip $ init x)

popFront :: String -> (String, String)
popFront x = (strip $ (head.words) x, strip $ (unwords.tail.words) x)
popBack :: String -> (String, String)
popBack x = (strip $ (last.words) x, strip $ (unwords.init.words) x)

{-************** Split By Bracket (nested) **************-}
splitBrackets [] str n list = list
splitBrackets (v:vs) str n list
  | (hasOpening v && hasEnding v && n > 0) = splitBrackets vs (str ++ " " ++ v) (n) list
  | (hasOpening v && hasEnding v && n == 0) = splitBrackets vs str (n) (list ++ [v])
  | (hasOpening v) = splitBrackets vs (str ++ " " ++ v) (n+1) list
  | (hasEnding v && n > 1) = splitBrackets vs (str ++ " " ++ v) (n-1) list
  | (hasEnding v && n == 1) = splitBrackets vs "" 0 (list ++ [str ++ " " ++ v])
  | otherwise = splitBrackets vs "" 0 (list ++ [v])

splitStartEndOneOf dlm_s dlm_e str = do
  let list = concat $ map (split (startsWithOneOf dlm_s)) $ split (endsWithOneOf dlm_e) str
      list' = filter (/= ",") $ filter (not.null) $ map strip list
  map strip (splitBrackets list' "" 0 [])

{-************** Modifing Functions **************-}
strSplit' dlm str = (strip x, strip xs) where (x, xs) = strSplit dlm str
sBreak' dlm str = (strip x, strip xs) where (x, xs) = sBreak dlm str
splitOn' dlm str = filter (not.null) $ map strip (splitOn dlm str)
splitOneOf' dlm str = filter (not.null) $ map strip (splitOneOf dlm str)

replaceline :: String -> String -> [String] -> [String]
replaceline old new [] = []
replaceline old new (x:xs)
  | (old == x) = new : (replaceline old new xs)
  | (old == v) = (f ++ new ++ b) : (replaceline old new xs)
  | otherwise = x : (replace' old new xs)
    where v = str_var ++ get_regexLine x regex_var
          [f, b] = get_regexLine_all x regex_fbpadding

replace' :: String -> String -> [String] -> [String]
replace' old new [] = []
replace' old new (x:xs) = unwords (replaceline old new (words x)) : replace' old new xs



replaceWord :: String -> String -> [String] -> String -> [String]
replaceWord old new [] regex = []
replaceWord old new (str:ss) regex
  | (old == str) = new : (replaceWord old new ss regex)
  | otherwise = do
    let after_pad = unwords $ map (head.tail) (str =~ regex :: [[String]])
        tmp_word = head $ endBy after_pad str
    (bool str (new ++ after_pad) (old == tmp_word)) : (replaceWord old new ss regex)

replaceLine :: String -> String -> String -> String -> String
replaceLine old new tyStr line
  | (isInfixOf old line) = do
    let s = split (startsWith old) line--no strip!!! ["front", "old...", "old..."]
        r = tyStr ++ get_regexLine line regex_fb
    unwords $ replaceWord old new s r
  | otherwise = line


{-************** USE(variable) **************-}

usePropLine :: String -> String -> [String] -> [String]
usePropLine old new [] = []
usePropLine old new (x:xs)
  | (not.null) rs = do
    let (lpad : v : rpad :etc) = (tail.head) rs
        new_word = bool x (lpad ++ new ++ rpad) ("%" ++ v == old)
    (new_word : usePropLine old new xs)
  | otherwise = (x : usePropLine old new xs)
  where rs = (x =~ regex_padding :: [[String]])

usePropagation :: String -> String -> [String] -> [String]
usePropagation old new [] = []
usePropagation old new (x:xs) = ((unwords $ usePropLine old new $ words x):(usePropagation old new xs))

{-************************************************************************
                            Type conversions
  *************************************************************************-}

strToInt :: String -> Integer
strToInt x = round (read x :: Double)

strToFloat :: String -> Double
strToFloat x = read x :: Double

strHexToStrBin :: String -> String
strHexToStrBin [] = ""
strHexToStrBin (x:xs) = do
  case toUpper(x) of
    '0' -> "0000" ++ (strHexToStrBin xs)
    '1' -> "0001" ++ (strHexToStrBin xs)
    '2' -> "0010" ++ (strHexToStrBin xs)
    '3' -> "0011" ++ (strHexToStrBin xs)
    '4' -> "0100" ++ (strHexToStrBin xs)
    '5' -> "0101" ++ (strHexToStrBin xs)
    '6' -> "0110" ++ (strHexToStrBin xs)
    '7' -> "0111" ++ (strHexToStrBin xs)
    '8' -> "1000" ++ (strHexToStrBin xs)
    '9' -> "1001" ++ (strHexToStrBin xs)
    'A' -> "1010" ++ (strHexToStrBin xs)
    'B' -> "1011" ++ (strHexToStrBin xs)
    'C' -> "1101" ++ (strHexToStrBin xs)
    'D' -> "1110" ++ (strHexToStrBin xs)
    'E' -> "1111" ++ (strHexToStrBin xs)
    _ -> "ERROR"

printList [] = []
printList (x:xs) = show x : (printList xs)
test x = printList (zipWith (\a b-> b * 2^a) [0..(length x)] (map digitToInt x))
test2 x = strBinToDec x

strBinToDec :: String -> Int
strBinToDec x = sum $ zipWith (\a b -> b * 2^a) [0..(length x)] (map digitToInt x)
fractionConv x = 1 + (sum $ zipWith (\i b -> b * 2^(-i)) [0..(length x)] (map digitToInt x))

strBinToDec_exp :: Fractional a => String -> a
strBinToDec_exp (s:x) = do
  let sign = (-1) ^ (bool 0 1 (s == '1'))
      e = 2 ^^ (strBinToDec (reverse x) - 1023)
  (sign * e)

strBinToDec_frac :: String -> Double
strBinToDec_frac x = 1 + 1 / (fromIntegral $ strBinToDec x)

strHexToDouble :: String -> Double
strHexToDouble x = do
  let (e, f) = splitAt 3 (filter (/= ' ') x)
      e' = strBinToDec_exp (strHexToStrBin e)
      f' = strBinToDec_frac (strHexToStrBin f)
  (e' * f')

{-************************************************************************
                            All Ones Byte
  *************************************************************************-}
isHexCap :: Char -> Bool
isHexCap c = isDigit c || ((fromIntegral (ord c - ord 'A')::Word) <= 5)

isNum :: String -> Bool
isNum s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  [(_, "-")] -> True
  _         -> False

isNum' :: String -> Bool
isNum' s = (length $ filter (isHexDigit) s) == (length s)

-- pre: Double
isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

-- pre: Integer
is0s :: Integer -> Bool --, Bits a // (Num a, Integral a, Eq a, Floating a, RealFrac a) => a
is0s x = isInt $ logBase 2 (fromInteger $ abs x)
-- is1s x = ((.&.) x (x+1)) == 0

is0s_sz :: (Num a, Integral a, Eq a, Floating a, RealFrac a) => a -> a -> Bool
is0s_sz sz x = sz == logBase 2 (abs x)

areZeros, hasZeros :: [Integer] -> Bool --(Num a, Integral a, Eq a, Floating a, RealFrac a) => [a]
areZeros x = null $ dropWhile (==True) (map is0s x)
hasZeros x = null $ takeWhile (==True) (map is0s x)

complementSet :: [String] -> [String] -> [String]
complementSet [] _ = []
complementSet (x:xs) y = complementSet xs (filter (/= x) y)

{-************************************************************************
                      LeftVar : Ordinary  VARIABLES
  *************************************************************************-}

addVariable :: String -> String -> String -> String -> [LeftVar] -> [LeftVar]
addVariable v t i s varList = varList ++ [(LeftVar v t i s)]

removeVariable :: LeftVar -> [LeftVar] -> [LeftVar] -> [LeftVar]
removeVariable vInfo [] px = px
removeVariable vInfo (x:xs) px
  | (variable vInfo == variable x) = (px ++ xs) -- found matching info
  | otherwise = removeVariable vInfo xs (px ++ [x])

lookupList :: String -> [LeftVar] -> Maybe LeftVar
lookupList v [] = Nothing
lookupList v (x:xs)
  | (v == variable x) = Just x
  | otherwise = lookupList v xs

updateList :: LeftVar -> [LeftVar] -> [LeftVar] -> [LeftVar]
updateList vInfo [] px = px
updateList vInfo (x:xs) px
  | (variable vInfo == variable x) = (px ++ [vInfo] ++ xs) -- found matching info
  | otherwise = updateList vInfo xs (px ++ [x])

setType, setInstr, setState :: String -> LeftVar -> LeftVar
setType new_type x =  x { vtype=new_type }
setInstr new_instr x = x { instruction=new_instr }
setState new_state x = x { state=new_state }

getType, getInstr, getState :: LeftVar -> String
getType x = vtype x
getInstr x = instruction x
getState x = state x


{-************************************************************************
                      RP : REGISTER FILE VARIABLES
  *************************************************************************-}

removePointer :: RP -> [RP] -> [RP] -> [RP]
removePointer pInfo [] px = px
removePointer pInfo (x:xs) px
  | (rname pInfo == rname x) = (px ++ xs) -- found matching info
  | otherwise = removePointer pInfo xs (px ++ [x])

lookupList_p :: String -> [RP] -> Maybe RP
lookupList_p p [] = Nothing
lookupList_p p (x:xs)
  | (p == rname x) = Just x
  | otherwise = lookupList_p p xs

updateList_p :: RP -> [RP] -> [RP] -> [RP]
updateList_p pInfo [] px = px
updateList_p pInfo (x:xs) px
  | (rname pInfo == rname x) = (px ++ [pInfo] ++ xs) -- found matching info
  | otherwise = updateList_p pInfo xs (px ++ [x])

setName, setBase, setRstate :: String -> RP -> RP
setName name x =  x { rname=name }
setBase base x = x { rbase=base }
setRstate state x = x { rstate = state }

setIndex :: Integer -> RP -> RP
setIndex idx x = x { ridx=idx }

setPermit :: Bool -> RP -> RP
setPermit bool x = x { permit=bool }

getName, getBase, getRstate :: RP -> String
getName x = rname x
getBase x = rbase x
getRstate x = rstate x

getIndex :: RP -> Integer
getIndex x = ridx x

getPermit :: RP -> Bool
getPermit x = permit x

variableType :: VAR -> String
variableType var
  | (isVaArg var) = (argty var)
  | (isCatchPad var) || (isCatchSwitch var) || (isCleanUpPad var) = "token"
  | otherwise = do
    if ((isAlloca var) || (isBinary var) || (isBitwise var) || (isCmpf var) || (isCmpi var) || (isConv var) || (isGetElemPtr var) || (isLoad var) || (isPhi var) || (isSelect var)) --(isInvoke var) ||
      then (ty var)
      else "none"

{-************************************************************************
                              Use (variable)
  *************************************************************************-}
isMatchPointer u [] = False
isMatchPointer u (v:vs)
  | (u == v || u == p) = True
  | otherwise = isMatchPointer u vs
    where p = str_var ++ get_regexLine v regex_fb

isUsePtr p line
  | (isInfixOf p line) = do
    let tmpList = map (str_var ++) $ splitOn "%" line
    isMatchPointer p tmpList
  | otherwise = False

isMatchList u [] = False
isMatchList u (v:vs)
  | (u == v || a == b) = True
  | otherwise = isMatchList u vs
    where a = filter isDigit u
          b = filter isDigit v

isUse v line
  | (isInfixOf v line) = do
    let tmp = concat $ map words (split (startsWith v) line)
        tmpList = filter (isPrefixOf v) tmp
    isMatchList v tmpList
  | otherwise = False

findDef :: String -> [LeftVar] -> String
findDef v vList
  | (not.isNothing) vInfo = getState (fromJust vInfo)
  | otherwise = ""
  where vInfo = lookupList v vList

-- value content fname line_number recursive_list
-- -> (line_number, (used variable info))
findUse :: String -> [String] -> [String] -> [String]
findUse v [] uselist = uselist
findUse v (line:nextCont) uselist
  | (isFunctionEnd line) = uselist
  | (isUse v state) =  findUse v nextCont (uselist ++ [line])
  | otherwise = findUse v nextCont uselist
    where state = last $ splitOn' " = " line
