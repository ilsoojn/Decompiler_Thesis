module OtherFunction where

import Data.Bool
import Data.Char
import Data.DeriveTH
import Data.List
import Data.List.Split
import Data.List.Utils
import Data.Maybe
import Data.String.Utils
import Data.Strings
import Data.Tuple
import Data.Typeable

import Debug.Trace
import Text.Regex.Posix

regexInit_fn, regexInit_bb, regex_fn, regex_bb, regex_block, regexEnd_fn, start_fn, start_bb::String
regexInit_fn = "^define void @fn_(.*)\\(.*\\{"
regexEnd_fn = "^}"
regexInit_bb = "^bb_(.*):.*"
regex_block = "(.*): [^%](.*)"
regex_fn = ".*?@fn_(.*)\\(.*\\{"
regex_bb = ".*?%bb_(.*)\\ .*"

start_fn = "define void @fn_"
start_bb = "bb_"

regex_semi, regex_array, regex_sync, regex_pad, regex_landpad::String
regex_semi = "%(.*) : %(.*)"
regex_array = ".*?[(.*)] [(.*)](.*)"
regex_sync = "\\((\".*\")\\)"
regex_pad = "(.*)[(.*)]"
regex_landpad = ".*?\\{(.*)\\}(.*)"
regex_fbpadding= "(.*)%[0-9a-zA-Z\\_\\-]*(.*)"
regex_fb = ".*%([0-9a-zA-Z\\_\\-]*).*"
regex_ab = ".* = %([0-9a-zA-Z\\_\\-\\+]*)"

{-************** Regex **************-}

get_RegexMatch :: String -> String -> String
get_RegexMatch rgx str = strip $ head $ map (head.tail) (str =~ rgx :: [[String]])

get_allRegexMatch :: String -> String -> [String]
get_allRegexMatch rgx str = map strip ((tail.head) (str =~ rgx :: [[String]]))

is_RegexMatch :: String -> String -> Bool
is_RegexMatch rgx str = bool False True (null (str =~ rgx :: [[String]]))

{-************** Function and BasicBlock **************-}

isFunction, isBasicBlock, isBlock, isStackPtr, isFunctionEnd:: String -> Bool
isFunction line = (not.null) $ map (head.tail) (line =~ regexInit_fn :: [[String]])
isBasicBlock line = (not.null) $ map (head.tail) (line =~ regexInit_bb :: [[String]])
isBlock line = (not.null) $ map (head.tail) (line =~ regex_block :: [[String]])
isStackPtr line  = (not.null) $ map (head.tail) (line =~ ".* = .* %RSP_(.*),\\ (.*)" :: [[String]])
isFunctionEnd line = (not.null) $ map (head.tail) (line =~ regexEnd_fn :: [[String]])

getFunctionName, getBlockName :: String -> String
getFunctionName line = "@fn_" ++ (unwords $ map (head.tail) (line =~ regexInit_fn :: [[String]]))
getBlockName line = "%bb_" ++ (unwords  $ map (head.tail) (line =~ regexInit_bb :: [[String]]))

getFrontBackPadding :: String -> (String, String)
getFrontBackPadding s = do
  let [front,back] = (tail.head)(s =~ regex_fbpadding :: [[String]])
  (front,back)

getFrontBackNonPadding :: String -> String
getFrontBackNonPadding s = unwords $ map (head.tail) (s =~ regex_fb :: [[String]])

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

{-************** SSA **************-}

getSSAssignValues :: [String] -> [String]
getSSAssignValues line = map strip (filter (isInfixOf "%") line)

getConstantValues :: [String] -> [String]
getConstantValues line = map strip (filter (not.isInfixOf "%") line)

{-************** Truple **************-}

stripTuple :: (String, String) -> (String, String)
stripTuple (x,y) = (head v, last v) where v = map strip [x,y]

selectTuple :: (Enum a, Strings a) => Int -> (a, a, a) -> a
selectTuple index (a, b, c) = do
  case index of
    0 -> a
    1 -> b
    2 -> c
    _ -> c

{-************** Pop Front & Back **************-}

popFirst :: [String] -> (String, [String])
popFirst x = (strip $ head x, map strip $ tail x)
popLast :: [String] -> (String, [String])
popLast x = (strip $ last x, map strip $ init x)

popFront :: String -> (String, String)
popFront x = stripTuple ((head.words) x, (unwords.tail.words) x)
popBack :: String -> (String, String)
popBack x = stripTuple ((last.words) x, (unwords.init.words) x)

{-************** Key and Value Pair **************-}

pairOneToMany :: String -> [String] -> [(String, String)]
pairOneToMany k [] = []
pairOneToMany k (v:vs) = (k, v):(pairOneToMany k vs)

pairOneToOne :: (Num a, Integral a) => [a] -> [a] -> [(a, a)]
pairOneToOne [] [] = []
pairOneToOne key [] = []
pairOneToOne [] value = []
pairOneToOne (k:ks) (v:vs) = [(k, v)]++ (pairOneToOne ks vs)

{-************** Modifing Functions **************-}

strSplit' dlm str = stripTuple (strSplit dlm str)
sBreak' dlm str = stripTuple (sBreak dlm str)
splitOn' dlm str = map strip (splitOn dlm str)
splitOneOf' dlm str = map strip (splitOneOf dlm str)

getLabel line = (strip.snd) (strSplit "label" line)

padAround :: String -> String -> String
padAround "" line = line
padAround c line = do
  let ch = [head c]
      str =  strPadBoth ' ' 3 ch
      new_str = strip $ join str $ map strip (splitOn ch line)
  padAround (tail c) new_str

replaceWord :: String -> String -> String -> String
replaceWord old new "" = ""
replaceWord old new string = do
  let (tmp: otherwords) = words (padAround "()[]{}" string)
      word = rmChar "," tmp
      new_word = bool tmp new (word == old)
  (new_word ++ " " ++ (replaceWord old new $ unwords otherwords))

rpWord :: String -> String -> String -> String
rpWord old new "" = ""
rpWord old new line = do
  let (x:xs) = words line
      tmp = "%" ++ (getFrontBackNonPadding x)
      --tmp = "%" ++ (filter isAlphaNum x)
      word = bool x (replace old new x) (tmp == old)
  strip (word ++ " " ++ (rpWord old new $ unwords xs))

rp :: String -> String -> [String] -> [String]
rp old new [] = []
rp old new (line: content) = (rpWord old new line):(rp old new content)

{-************** Variables **************-}

addVariable :: String -> String -> [(String, String)] -> [(String, String)]
addVariable v t varList = varList ++ [(v, t)]

removeVariable :: String -> [(String, String)] -> [(String, String)]
removeVariable v varList = filter (/= (v, getType v varList)) varList

getType :: String -> [(String, String)] -> String
getType v varList = bool (fromJust t) "none" (isNothing t)
  where t = (lookup v varList)

variableType :: VAR -> String
variableType var
  | (isVaArg var) = (argty var)
  | (isCatchPad var) || (isCatchSwitch var) || (isCleanUpPad var) = "token"
  | otherwise = do
    if ((isAlloca var) || (isBinary var) || (isBitwise var) || (isCmpf var) || (isCmpi var) || (isConv var) || (isGetElemPtr var) || (isLoad var) || (isPhi var) || (isSelect var)) --(isInvoke var) ||
      then (ty var)
      else "none"

detectVariable :: [String] -> String -> [(String, String)]
detectVariable [] fname varSet = varList
detectVariable (line:nextCont) fname varSet
  | (isVarDeclare line fname) = do
    let (v, (var, reg)) = statement line fname
        newList = addVariable (fromJust v) (variableType var) varSet
    detectVariable nextCont fname newList
  | otherwise = detectVariable nextCont fname varSet

{-************** USE(variable) **************-}

usePropLine :: String -> String -> [String] -> [String]
usePropLine old new [] = []
usePropLine old new (x:xs)
  | (not.null) rs = do
    let (lpad : v : rpad :etc) = (tail.head) rs
        new_word = bool x (lpad ++ new ++ rpad) ("%" ++ v == old)
    (new_word : usePropLine old new xs)
  | otherwise = (x : usePropLine old new xs)
  where rs = (x =~ "(.*)%([0-9]*)(.*)" :: [[String]])

usePropagation :: String -> String -> [String] -> [String]
usePropagation old new [] = []
usePropagation old new (x:xs) = ((unwords $ usePropLine old new $ words x):(usePropagation old new xs))
