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

{-******************************************************
            FLAG Statements Elimination
******************************************************-}

-- filter' _ [] [] = []
-- filter' eqCmp (c:condLst) (x:dataLst)
--   | (eqCmp == c) = x : filter' eqCmp condLst dataLst
--   | otherwise filter' eqCmp condLst dataLst

flagElimination :: [String] -> [String] -> [LeftVar] -> ([String], [LeftVar])
flagElimination settings precontent list = do
  let shl_statements = map head $ chunksOf 2 settings
      or_statements = map last $ chunksOf 2 settings

      s = map statement shl_statements
      o = map statement or_statements

      -- shls = trace("here")map head $ chunksOf 2 s  --[(sx, (svar, [sreg]))...]
      sx = map (fromJust.fst) (s ++ o)
      sreg = map (snd.snd) s

      idxf = [ strToInt x | x <- (concat sreg), isNum x ]
      valf = [ x | x <- (concat sreg), not (isNum x) ]

  if (idxf /= [0, 2, 4, 6, 7, 11])
    then (settings, list)
    else do
      let line = (last sx) ++ " = " ++ (join "." valf)
          tmpVar = map fromJust $ map (`lookupList` list) (init sx) --trace(" >>" ++ (last settings) ++ "\n >>" ++ line)
          newList = removeVariables tmpVar list
      ([line], newList)

cleanCode :: [String] -> [String] -> [LeftVar] -> ([String], [LeftVar])
cleanCode [] preContent vList = (preContent, vList)
cleanCode (line : nextContent) preContent vList
  | (isInfixOf "=" line) = do
    let (x, (s, r)) = statement line
    if (op s == "load" && ptr s == "%CtlSysEFLAGS")
      then do
        let nextLines = take 12 nextContent               -- get next 12 statements
            nextStatement = map (getAfter "=") nextLines
            nextInstrList = map (head.words) nextStatement  -- collect instructions of next 12 statement
            tmpInstrList = concat $ replicate 6 ["shl", "or"] -- bit setting

        if (nextInstrList /= tmpInstrList)
          then cleanCode nextContent (preContent ++ [line]) vList --trace("  x:\n\t" ++ (join " " nextInstrList) ++ "\n\t" ++ (join " " tmpInstrList))
          else do
            let (newLine, newList) = flagElimination nextLines [] vList
            cleanCode (newLine ++ (drop 12 nextContent)) (preContent ++ [line]) newList --trace("  v:\n\t" ++ (join " " nextInstrList) ++ "\n\t" ++ (join " " tmpInstrList))

      else cleanCode nextContent (preContent ++ [line]) vList
  | otherwise = cleanCode nextContent (preContent ++ [line]) vList

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
    let (na, (var, reg)) = statement (strip line)
        operands = filter (not.isInfixOf "fn") (filter (not.isInfixOf "bb") $ filter (isPrefixOf str_var) reg)

    if (op var == "store")
      then do
        if (isInfixOf "_init" line || isInfixOf "_ptr" line)-- || elem y reg_base)
          then variableElim True nextCont vList pList preCont

          else if (hasNoDef operands vList)
            then -- NoDef(reg)
              variableElim True nextCont vList pList preCont --trace("def( - ) = 0 : " ++ line)
            else -- Def(reg)
              variableElim change nextCont vList pList (preCont ++ [line])

      else if (hasNoDef operands vList)
        then -- NoDef(reg)
          variableElim True nextCont vList pList preCont --trace("def( - ) = 0 : " ++ line)
        else -- Def(reg)
          variableElim change nextCont vList pList (preCont ++ [line]) --trace("good\t" ++ line)

elimination :: [String] -> [LeftVar] -> [RP] -> ([String], [LeftVar])
elimination content vList pList = do

  let (eContent, eVarList) = variableElim False content vList pList []
      (cContent, cVarList) = cleanCode eContent [] eVarList
      (newContent, newVarList) = variableElim False cContent cVarList pList []
  (newContent, newVarList)
