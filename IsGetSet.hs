module IsGetSet where

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

import Lists

is_regexLine :: String -> String -> Bool
is_regexLine line regex = (not.null) $ map (head.tail) (line =~ regex :: [[String]])

isFunction line = is_regexLine line regexLine_fn
isBasicBlock line = is_regexLine line regexLine_bb
isBlock line = is_regexLine line regex_semi1
isStackPtr line  = is_regexLine line regex_rsp
isFunctionEnd line = is_regexLine line regexEnd_fn

isLHSRHS line = is_regexLine line regex_ab

get_regexLine :: String -> String -> String
get_regexLine line regex = strip $ unwords $ map (head.tail) (line =~ regex :: [[String]])

get_regexLine_all :: String -> String -> [String]
get_regexLine_all line regex = map strip ((tail.head) (line =~ regex :: [[String]]))

getFunctionName line = str_fn ++ (get_regexLine line regexLine_fn)
getBlockName line = str_bb ++ (get_regexLine line regexLine_bb)

getRHS line = get_regexLine line regex_ab

getPadding :: String -> String -> [String]
getPadding line regex = (tail.head)(line =~ regex :: [[String]])

getFrontBackPadding :: String -> (String, String)
getFrontBackPadding line = (front,back)
  where [front,back] = (tail.head)(line =~ regex_fbpadding :: [[String]])

getFrontBackNonPadding :: String -> String
getFrontBackNonPadding line = get_regexLine line regex_fb

{-************ Bracket ************-}
hasOpening x = (elem '[' x) || (elem '{' x) || (elem '<' x)
hasEnding x = (elem ']' x) || (elem '}' x) || (elem '>' x)

{-************** SSA **************-}

getSSAssignValues :: [String] -> [String]
getSSAssignValues line = map strip (filter (isInfixOf "%") line)

getConstantValues :: [String] -> [String]
getConstantValues line = map strip (filter (not.isInfixOf "%") line)

getLabel :: String -> String
getLabel line = (strip.snd) (strSplit "label" line)

getSyncscope :: String -> Maybe String
getSyncscope line = bool Nothing (Just $ get_regexLine line regex_sync) (isInfixOf "syncscope" line)
