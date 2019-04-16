module RegexFunction where

import Control.Monad
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
isFunctionEnd line = is_regexLine line regexEnd_fn
isEntryExit line = (isPrefixOf "entry_fn" line) || (isPrefixOf "exit_fn" line)
isRegPointer str = (isPrefixOf "%R" str || isPrefixOf "%E" str || isPrefixOf "%ZMM" str || isPrefixOf "%YMM" str || isPrefixOf "%XMM" str)
hasRegPointer line = (isInfixOf "%R" line || isInfixOf "%E" line)
isInitialPointer str = (isInfixOf "_ptr" str || isInfixOf "_init" str)
hasInitialPointer line = (isInfixOf "_ptr" line || isInfixOf "_init" line)

isColonLine line = is_regexLine line regex_colon  -- v = %a  : %b
isEqualLine line = (is_regexLine line regex_equal)  || (is_regexLine line regex_equalLoad) || (is_regexLine line regex_equalNumber)        -- L = R
isEqualState line = (is_regexLine line regex_rhs) || (is_regexLine line regex_rhsLoad) || (is_regexLine line regex_rhsNumber)
isDoubleRHS line = do
  let eqState = is_regexLine line regex_rhsLoad
      tmp = getDoubleRHS line
  case (reads tmp :: [(Double, String)]) of
    [(_, "")] -> eqState && True
    _         -> False

get_regexLine :: String -> String -> String
get_regexLine line regex = strip $ unwords $ map (head.tail) (line =~ regex :: [[String]])

get_regexLine_all :: String -> String -> Maybe [String]
get_regexLine_all line regex = do
  let tmp = (line =~~ regex :: Maybe [[String]])
  if (isNothing tmp)
    then Nothing
    else Just $ map strip $ (tail.head) (fromJust tmp)

-- Assembly Code
getAddressName line = get_regexLine_all line regex_asm

-- Handle Function / Block Statement
getFunctionAddr line = (get_regexLine line regexLine_fn)
getFunctionName line = str_fn ++ (get_regexLine line regexLine_fn)
getFunctionInfo line = get_regexLine_all line regexLine_startFn
getBlockName line = str_bb ++ (get_regexLine line regexLine_bb)
getBlockLabel line = (get_regexLine line regexLine_label)
getBlockPreds line = map strip $ splitOn "," (get_regexLine line regex_bb_pred)

getRHS_state line
  | (is_regexLine line regex_rhsLoad) = "["++ (get_regexLine line regex_rhsLoad) ++ "]"
  | (is_regexLine line regex_rhs) = '%':(get_regexLine line regex_rhs)
  | otherwise = (get_regexLine line regex_rhsNumber)

getRHS line = getRHS_state state where state = bool line (strip $ snd $ strSplit " = " line) (isInfixOf " = " line)

getDoubleRHS line = get_regexLine line regex_rhsLoad

getFrontBackNonPadding :: String -> String
getFrontBackNonPadding line = get_regexLine line regex_fb

-- %v = %a : %b -> [a, b]
getHighLowVariable :: String -> [String]
getHighLowVariable line = map strip (splitOn ":" line)

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
