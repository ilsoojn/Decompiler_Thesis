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
import RegexFunction
import Lists

-- Def(ops) > 0 := True / False
hasNoDef [] vList = False
hasNoDef (v:vs) vList
  | (isNothing vInfo) = True
  | otherwise = False || (hasNoDef vs vList)
  where vInfo = lookupList v vList

variableElim :: Bool -> [String] -> [LeftVar] -> [RP] -> [String] -> ([String], [LeftVar])
variableElim False [] vList pList preCont = (preCont, vList)
variableElim True [] vList pList preCont = variableElim False preCont vList pList []
variableElim change (line : nextCont) vList pList preCont
  -- | (isFunction line) = variableElim change nextCont (getFunctionName line) vList pList (preCont ++ [line])
  | (isFunction line || isBlockLabel line || isBasicBlock line) = variableElim change nextCont vList pList (preCont ++ [line])
  | (isEntryExit line) = variableElim change nextCont vList pList (preCont ++ [line])
  | (isInfixOf " = " line) = do
    -- LHS
    let (x, (var, ops)) = statement (strip line)
        v = fromJust x
        use = filter (not.null) (findUse v nextCont [])
        op_variable = filter (not.isInfixOf "fn") (filter (not.isInfixOf "bb") $ filter (isPrefixOf str_var) ops)

    if (null use)
      then do
        -- DEAD Variable
        if (isNothing $ lookupList v vList)
          then do
             variableElim True nextCont vList pList preCont --trace("use(" ++ v ++ ") : NO\n not in List")
          else do
            let v' = fromJust $ lookupList v vList
                newList = removeVariable v' vList []
            variableElim True nextCont newList pList preCont -- trace("use(" ++ v ++ ") : NO\n yes in List")

      else if (hasNoDef op_variable vList)
        then
          -- LIVE Variable but NoDef(ops)
          variableElim True nextCont vList pList preCont -- trace("use(" ++ v ++ ") : YES\n Dead variables")
        else
          -- LIVE Variable and Def(ops)
          variableElim change nextCont vList  pList (preCont ++ [line]) -- trace("use(" ++ v ++ ") : YES\n No dead variables")

  | otherwise = do
    -- No LHS
    let (x, (var, ops)) = statement (strip line)
        op_var = filter (not.isInfixOf "fn") (filter (not.isInfixOf "bb") $ filter (isPrefixOf str_var) ops)
    if (hasNoDef op_var vList)
      then
        -- NoDef(ops)
        variableElim True nextCont vList pList preCont -- trace("def-use( x )\n Dead variables"++ line)
      else
        -- Def(ops)
        variableElim change nextCont vList  pList (preCont ++ [line]) -- trace("def-use( x )\n No dead variables")

registerElim :: [String] -> [(String, String)] -> [String] ->[String]
registerElim [] nameList preContent = preContent
registerElim (line:nextContent) nameList preContent
  | (isFunction line || isFunctionEnd line || isBlockLabel line || isBasicBlock line || isEntryExit line) = registerElim nextContent nameList (preContent ++ [line])
  | (isPrefixOf "store" line) = do
    -- STORE statement
    let s = storeStatement line
        value = str_v s
        location = str_at s
    if (isInfixOf "_ptr" line || elem location reg_base)
      then
        registerElim nextContent nameList preContent
      else do
        -- Possible Variable State
        let str = lookup location nameList
            vname = bool (fromJust str) (str_var ++ (names !! length nameList)) (isNothing str)
            pair = trace("(" ++ location ++ ", " ++ vname ++ ")")(location, vname)

            newLine = replace location vname line
            new_nextCont = replace' location vname nextContent
            new_preCont = replace' location vname preContent
            newList = map head $ (group.sort) (pair : nameList)
        registerElim new_nextCont newList (new_preCont ++ [newLine])

    | otherwise = registerElim nextContent nameList (preContent ++ [line])

elimination :: [String] -> [LeftVar] -> [RP] -> ([String], [LeftVar])
elimination content vList pList = do

  let (newContent, newVarList) = variableElim False content vList pList []
  (newContent, newVarList)
  --(registerElim newContent [] [], newVarList)
