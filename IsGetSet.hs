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

isLHS line fname = (not.isFunction) line && (not.isBasicBlock) line && (not.isBlockLabel) line && (not.null) fname && (isInfixOf " = " line)

is_regexLine :: String -> String -> Bool
is_regexLine line regex = (not.null) $ map (head.tail) (line =~ regex :: [[String]])

isFunction line = is_regexLine line regexLine_fn
isBasicBlock line = is_regexLine line regexLine_bb    -- bb_#: ...
isBlockLabel line = is_regexLine line regexLine_label -- ; <label>:bb_#...
isSemiColon line = is_regexLine line regex_semicolon  -- v = %a  : %b
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

getFrontBackNonPadding :: String -> String
getFrontBackNonPadding line = get_regexLine line regex_fb

-- %v = %a : %b -> [a, b]
getHighLowVariable :: String -> [String]
getHighLowVariable line = get_regexLine_all line regex_semicolon

-- paddings

getPadding :: String -> String -> [String]
getPadding line regex = (tail.head)(line =~ regex :: [[String]])

getFrontBackPadding :: String -> (String, String)
getFrontBackPadding line = (front,back)
  where [front,back] = (tail.head)(line =~ regex_fbpadding :: [[String]])

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
