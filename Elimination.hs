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
  | (isNothing vInfo) = True --trace("hasNoDef("++v++") x" )
  | otherwise = False || (hasNoDef vs vList)
  where vInfo = lookupList v vList

variableElim :: Bool -> [String] -> [LeftVar] -> [RP] -> [String] -> ([String], [LeftVar])
variableElim False [] vList pList preCont = (preCont, vList)
variableElim True [] vList pList preCont = variableElim False preCont vList pList [] --trace("\n-----------------\n")
variableElim change (line : nextCont) vList pList preCont
  -- | (isFunction line) = variableElim change nextCont (getFunctionName line) vList pList (preCont ++ [line])
  | (isFunction line || isBlockLabel line || isBasicBlock line) = variableElim change nextCont vList pList (preCont ++ [line])
  | (isEntryExit line) = variableElim change nextCont vList pList (preCont ++ [line])
  | (isInfixOf " = " line) = do
    -- LHS
    let (x, (var, ops)) = statement (strip line)
        v = fromJust x
        use = filter (not.null) (findUse v nextCont [])
        operands = filter (not.isInfixOf "fn") (filter (not.isInfixOf "bb") $ filter (isPrefixOf str_var) ops)

    case (isInfixOf "_init" line || isInfixOf "_ptr" line) of
      True -> do
        let v' = fromJust $ lookupList v vList
            newList = removeVariable v' vList []
        variableElim True nextCont newList pList preCont

      _ -> do
        if (null use)
          then do
            -- DEAD Variable
            if (isNothing $ lookupList v vList)
              then do
                 variableElim True nextCont vList pList preCont --trace("use(" ++ v ++ ") = 0 : " ++ line)
              else do
                let v' = fromJust $ lookupList v vList
                    newList = removeVariable v' vList []
                variableElim True nextCont newList pList preCont --trace("use(" ++ v ++ ") = 0 : " ++ line)

          else if (hasNoDef operands vList)
            then do-- LIVE Variable but NoDef(ops)
              let v' = fromJust $ lookupList v vList
                  newList = removeVariable v' vList []
              variableElim True nextCont newList pList preCont --trace("def(" ++ v ++ ") = 0 : " ++ line)
            else -- LIVE Variable and Def(ops)
              variableElim change nextCont vList  pList (preCont ++ [line]) --trace("good\t" ++ line)

  | otherwise= do
    -- No LHS
    let (x, (var, ops)) = statement (strip line)
        operands = filter (not.isInfixOf "fn") (filter (not.isInfixOf "bb") $ filter (isPrefixOf str_var) ops)

    if (op var == "store")
      then do
        let (x, y) = (value var, at var)
        if (isInfixOf "_init" line || isInfixOf "_ptr" line)-- || elem y reg_base)
          then variableElim True nextCont vList pList preCont

          else if (hasNoDef [x, y] vList)
            then -- NoDef(ops)
              variableElim True nextCont vList pList preCont --trace("def( - ) = 0 : " ++ line)
            else -- Def(ops)
              variableElim change nextCont vList pList (preCont ++ [line])

      else if (hasNoDef operands vList)
        then -- NoDef(ops)
          variableElim True nextCont vList pList preCont --trace("def( - ) = 0 : " ++ line)
        else -- Def(ops)
          variableElim change nextCont vList pList (preCont ++ [line]) --trace("good\t" ++ line)

elimination :: [String] -> [LeftVar] -> [RP] -> ([String], [LeftVar])
elimination content vList pList = do

  let (newContent, newVarList) = variableElim False content vList pList []
  (newContent, newVarList)
