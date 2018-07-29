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
regexInit_bb = "^bb_(.*):.*"
regex_block = "(.*):(.*)"
regex_fn = ".*?@fn_(.*)\\(.*\\{"
regex_bb = ".*?%bb_(.*)\\ .*"
regexEnd_fn = "^}"

start_fn = "define void @fn_"
start_bb = "bb_"


regex_semi, regex_array, regex_sync, regex_pad, regex_landpad::String
regex_semi = " = (.*):(.*)"
regex_array = ".*?[(.*)] [(.*)](.*)"
regex_sync = "\\((\".*\")\\)"
regex_pad = "(.*)[(.*)]"
regex_landpad = ".*?\\{(.*)\\}(.*)"

{-************** Regex **************-}

get_RegexMatch :: String -> String -> String
get_RegexMatch rgx str = strip $ head $ map (head.tail) (str =~ rgx :: [[String]])

get_allRegexMatch :: String -> String -> [String]
get_allRegexMatch rgx str = map strip ((tail.head) (str =~ rgx :: [[String]]))

is_RegexMatch :: String -> String -> Bool
is_RegexMatch rgx str = bool False True (null (str =~ rgx :: [[String]]))

{-************** Function and BasicBlock **************-}

isFunction, isBasicBlock, isBlock :: String -> Bool
isFunction line = (not.null) $ map (head.tail) (line =~ regexInit_fn :: [[String]])
isBasicBlock line = (not.null) $ map (head.tail) (line =~ regexInit_bb :: [[String]])
isBlock line = (not.null) $ map (head.tail) (line =~ regex_block :: [[String]])

getFunctionName, getBlockName :: String -> String
getFunctionName line = "@fn_" ++ (unwords $ map (head.tail) (line =~ regexInit_fn :: [[String]]))
getBlockName line = "%bb_" ++ (unwords  $ map (head.tail) (line =~ regexInit_bb :: [[String]]))

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
popBack x = stripTuple ((last.words) x, (unwords.tail.words) x)

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
