module Elimination where

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

import StatementParse
import StatementInstr
import OtherFunction
import IsGetSet
import Lists

hasDeadVar [] vList = False
hasDeadVar (v:vs) vList
  | (isNothing vInfo) = True
  | otherwise = False || (hasDeadVar vs vList)
  where vInfo = lookupList v vList

elimination :: Bool -> [String] -> String -> [LeftVar] -> [RP] -> [String] -> ([String], [LeftVar])
elimination False [] fname vList pList preCont = (preCont, vList)
elimination True [] fname vList pList preCont = trace("\n\n---------------------\n\n") elimination False preCont "" vList pList []
elimination change (line : nextCont) fname vList pList preCont
  | trace("e: " ++ line)(isFunction line) = elimination (change || False) nextCont (getFunctionName line) vList pList (preCont ++ [line])
  | (isBlockLabel line || isBasicBlock line) = elimination (change || False) nextCont fname vList pList (preCont ++ [line])
  | (isEntryExit line) = elimination (change || False) nextCont fname vList pList (preCont ++ [line])
  | (isLHS line fname) = do
    let (x, (var, ops)) = statement (strip line)
        v = fromJust x
        use = filter (not.null) (findUse v nextCont [])
        op_variable = filter (not.isInfixOf "fn") (filter (not.isInfixOf "bb") $ filter (isPrefixOf str_var) ops)

    if (null use)
      then do
        let vInfo = lookupList v vList
            info = fromJust vInfo
            newList = removeVariable info vList []
        trace("\tE NO")elimination (change || True) nextCont fname newList pList preCont
      else if (hasDeadVar op_variable vList)
        then trace("\tE YES Dead")elimination (change || True) nextCont fname vList pList preCont
        else trace("\tE YES Live")elimination (change || False) nextCont fname vList  pList(preCont ++ [line])

  | otherwise = do
    let (x, (var, ops)) = statement (strip line)
        op_var = filter (not.isInfixOf "fn") (filter (not.isInfixOf "bb") $ filter (isPrefixOf str_var) ops)
    if (hasDeadVar op_var vList)
      then trace("\tE Dead")elimination (change || True) nextCont fname vList pList preCont
      else trace("\tE Live")elimination (change || False) nextCont fname vList  pList(preCont ++ [line])
