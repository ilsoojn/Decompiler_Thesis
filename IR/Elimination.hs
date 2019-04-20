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

-- Handle CtlSysEFLAGS Idioms ()
cleanCode :: Function -> [String] -> [String] -> Function
cleanCode f [] newCode = f{ code = newCode }
cleanCode f (line : nextContent) preContent
  | (isInfixOf "=" line) = do
    let (x, (s, r)) = statement line
    if (op s == "load" && ptr s == "%CtlSysEFLAGS")
      then do
        let nextLines = take 12 nextContent               -- get next 12 statements
            nextStatement = map (getAfter "=") nextLines
            nextInstrList = map (head.words) nextStatement  -- collect instructions of next 12 statement
            tmpInstrList = concat $ replicate 6 ["shl", "or"] -- bit setting
            vList = variables f

        if (nextInstrList /= tmpInstrList)
          then cleanCode f nextContent (preContent ++ [line]) --trace("  x:\n\t" ++ (join " " nextInstrList) ++ "\n\t" ++ (join " " tmpInstrList))
          else do
            let (newLine, newList) = flagElimination nextLines [] vList
            cleanCode (f{ variables = newList }) (newLine ++ (drop 12 nextContent)) (preContent ++ [line]) --trace("  v:\n\t" ++ (join " " nextInstrList) ++ "\n\t" ++ (join " " tmpInstrList))

      else cleanCode f nextContent (preContent ++ [line])
  | otherwise = cleanCode f nextContent (preContent ++ [line])

-- Def(ops) > 0 := True / False
hasNoDef [] vList = False
hasNoDef (v:vs) vList
  | (isNothing vInfo) = True --trace("hasNoDef("++v++") x" )
  | otherwise = False || (hasNoDef vs vList)
  where vInfo = lookupList v vList

variableElim :: Bool -> Function -> [String] -> [String] -> Function-- [LeftVar] -> [RP] -> [String] -> ([String], [LeftVar])
variableElim False f [] newCode = f {code = newCode }
variableElim True f [] preCont = variableElim False f preCont [] --trace("\n-----------------\n")
variableElim change f (line : nextCont) preCont
  -- | (isFunction line) = variableElim change nextCont (getFunctionName line) vList pList (preCont ++ [line])
  | (isFunction line || isBlockLabel line || isBasicBlock line || isEntryExit line) = variableElim change f nextCont (preCont ++ [line])
  | (isLHS line (fname f)) = do
    -- LHS
    let (x, (var, ops)) = statement (strip line)
        v = fromJust x
        use = filter (not.null) (findUse v nextCont)
        operands = filter (not.isInfixOf "fn") (filter (not.isInfixOf "bb") $ filter (isPrefixOf str_var) (words $ unwords ops))
        vList = variables f

    case (isInfixOf "_init" line || isInfixOf "_ptr" line || elem v reg_base) of
      True -> do
        let v' =  fromJust $ lookupList v vList
            newList = removeVariable v' vList []
        variableElim True (f{ variables = newList }) nextCont preCont

      _ -> do
        if (null use)
          then do
            -- DEAD Variable
            if (isNothing $ lookupList v vList)
              then do
                 trace("use(" ++ v ++ ") = 0 : " ++ line) variableElim True f nextCont preCont --trace("use(" ++ v ++ ") = 0 : " ++ line)
              else do
                let v' = fromJust $ lookupList v vList
                    newList = removeVariable v' vList []
                trace("use(" ++ v ++ ") = 0 : " ++ line) variableElim True (f{ variables = newList }) nextCont preCont --trace("use(" ++ v ++ ") = 0 : " ++ line)

          else if (hasNoDef operands vList)
            then do-- LIVE Variable but NoDef(ops)
              let v' = fromJust $ lookupList v vList
                  newList = removeVariable v' vList []
              trace("def(" ++ v ++ ") = 0 : " ++ line) variableElim True (f{ variables = newList }) nextCont preCont --trace("def(" ++ v ++ ") = 0 : " ++ line)
            else -- LIVE Variable and Def(ops)
              trace("good\t" ++ line)variableElim change f nextCont (preCont ++ [line]) --trace("good\t" ++ line)

  | otherwise= do
    -- No LHS
    let (na, (var, reg)) = statement (strip line)
        operands = filter (not.isInfixOf "fn") (filter (not.isInfixOf "bb") $ filter (isPrefixOf str_var) (words $ unwords reg))
        vList = variables f

    if (op var == "store")
      then do

        if (isInfixOf "_init" line || isInfixOf "_ptr" line)
          then variableElim True f nextCont preCont

          else if (hasNoDef operands vList)
            then -- NoDef(reg)
              trace("def( - ) = 0 : " ++ line) variableElim True f nextCont preCont --trace("def( - ) = 0 : " ++ line)
            else -- Def(reg)
              variableElim change f nextCont (preCont ++ [line])

      else if (hasNoDef operands vList)
        then -- NoDef(reg)
          trace("def( - ) = 0 : " ++ line) variableElim True f nextCont preCont --trace("def( - ) = 0 : " ++ line)
        else -- Def(reg)
        trace("good\t" ++ line)  variableElim change f nextCont (preCont ++ [line]) --trace("good\t" ++ line)

elimination :: Function -> Function
elimination f = do

  let tmpFn = variableElim False f (code f) []
      tmpFn2 = cleanCode tmpFn (code tmpFn) []
  variableElim False tmpFn2 (code tmpFn2) []
