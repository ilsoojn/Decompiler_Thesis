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

precisionConversion :: [String] -> Int -> String -> [String] -> [String]
precisionConversion [] _ _ content = content
precisionConversion (line : next) addr val preCont
  | (isInfixOf " = " line) = do
    let [v, state] = splitOn " = " line
    if (isEqualState state && isDoubleRHS state)
      then do
        let address = fromInteger $ strToInt $ getDoubleRHS state
            value = getData 64 addr val address
            double = show $ strHexToDouble 64 value
            newLine = trace(v ++ " -> " ++ double) concat[v, " = ", double]
            new_next = replace' v double next
        precisionConversion new_next addr val (preCont ++ [newLine])

      else precisionConversion next addr val (preCont ++ [line])
  | otherwise = precisionConversion next addr val (preCont ++ [line])

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
             variableElim True nextCont vList pList preCont -- trace("use(" ++ v ++ ") = 0 : " ++ line)
          else do
            let v' = fromJust $ lookupList v vList
                newList = removeVariable v' vList []
            variableElim True nextCont newList pList preCont -- trace("use(" ++ v ++ ") = 0 : " ++ line)

      else if (hasNoDef op_variable vList)
        then
          -- LIVE Variable but NoDef(ops)
          variableElim True nextCont vList pList preCont -- trace("def(" ++ v ++ ") = 0 : " ++ line)
        else
          -- LIVE Variable and Def(ops)
          variableElim change nextCont vList  pList (preCont ++ [line]) -- trace("good\t" ++ line)

  | otherwise= do
    -- No LHS
    let (x, (var, ops)) = statement (strip line)
        op_var = filter (not.isInfixOf "fn") (filter (not.isInfixOf "bb") $ filter (isPrefixOf str_var) ops)

    if (isStore var)
      then do
        let (x, y) = (value var, at var)
        if (isInfixOf "_init" line || isInfixOf "_ptr" line || elem y reg_base)
          then variableElim True nextCont vList pList preCont
          else variableElim change nextCont vList pList (preCont ++ [line])
      else
        if (hasNoDef op_var vList)
          then
            -- NoDef(ops)
            variableElim True nextCont vList pList preCont -- trace("def( - ) = 0 : " ++ line)
          else
            -- Def(ops)
            variableElim change nextCont vList pList (preCont ++ [line]) -- trace("good\t" ++ line)

elimination :: [String] -> [LeftVar] -> [RP] -> ([String], [LeftVar])
elimination content vList pList = do

  let (newContent, newVarList) = variableElim False content vList pList []
  (newContent, newVarList)
