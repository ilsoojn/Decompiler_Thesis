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

propagateTypeVar :: [String] -> String -> String -> String -> String -> [LeftVar] -> [String] -> ([String], [LeftVar])
propagateTypeVar [] oT oV nT nV vList preCont = (preCont, vList)
propagateTypeVar (line : nextCont) ot ov nt nv vList preCont
  | (isFunction line || isBasicBlock line || isBlockLabel line) = propagateTypeVar nextCont ot ov nt nv vList (preCont ++ [line])
  | (isUse ov (bool line (last $ splitOn' " = " line) (isInfixOf " = " line))) = do
    let (front, back) = strSplit ot (replace ov nv line)
        newline = bool (front ++ nt ++ back) (front) (null back)
        vtmp = lookupList ov vList

    if ((not.isNothing) vtmp) -- line: def(someVariable) = .... oldVar
      then do
        let vi = fromJust vtmp
            new_vtype  = bool ot nt ((vtype vi) == ot)
            newInfo = vi{ vtype=new_vtype, state=newline }
            newList = updateList newInfo vList []
        propagateTypeVar nextCont ot ov nt nv newList (preCont ++ [newline])
      else propagateTypeVar nextCont ot ov nt nv vList (preCont ++ [newline])

  | otherwise = propagateTypeVar nextCont ot ov nt nv vList (preCont ++ [line])

propagation :: [String] -> String -> [LeftVar] -> [RP] -> [String] -> ([String], ([LeftVar]))
propagation [] fname vList pList preCont = (preCont, vList)
propagation (line: nextCont) fname vList pList preCont
  | (isFunction line) = propagation nextCont (getFunctionName line) vList pList (preCont ++ [line])
  | (isFunctionEnd line) = (preCont, vList)
  | (isBasicBlock line || isBlockLabel line) = propagation nextCont fname vList pList (preCont ++ [line])
  | (isLHS line fname) = do
    let (x, (xvar, xop)) = statement (strip line)
        v = fromJust x

    case (isRegPointer line) of
      True -> do
        let rp = fromJust $ lookupList_p v pList
            newLine = concat[v, " = ", getRstate rp]
        propagation nextCont fname vList pList (preCont ++ [newLine])
      _ -> do
        if (isConv xvar)
          then do
            let (new_nextCont, newList) = propagateTypeVar nextCont (ty xvar) (v) (ty1 xvar) (value xvar) vList []
            propagation new_nextCont fname newList pList (preCont ++ [line])

          else if (isSemicolon xvar)
            then do
              let from_v = fromJust $ lookupList v vList
                  fromT = vtype from_v
                  to_v = lookupList (low xvar) vList
                  toT = bool (getType $ fromJust to_v) "none" (isNothing to_v)

                  (new_nextCont, newList) = propagateTypeVar nextCont fromT (v) toT (low xvar) vList []
              propagation new_nextCont fname newList pList (preCont ++ [line])

            else propagation nextCont fname vList pList (preCont ++ [line])

  | otherwise = propagation nextCont fname vList pList (preCont ++ [line])

hasDeadVar [] vList = False
hasDeadVar (v:vs) vList
  | (isNothing vInfo) = True
  | otherwise = False || (hasDeadVar vs vList)
  where vInfo = lookupList v vList

elimination :: Bool -> [String] -> String -> [LeftVar] -> [RP] -> [String] -> ([String], [LeftVar])
elimination False [] fname vList pList preCont = (preCont, vList)
elimination True [] fname vList pList preCont = trace("\n\n---------------------\n\n") elimination False preCont "" vList pList []
elimination change (line : nextCont) fname vList pList preCont
  | (isFunction line) = elimination (change || False) nextCont (getFunctionName line) vList pList (preCont ++ [line])
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
        trace("E NO: " ++ line)elimination (change || True) nextCont fname newList pList preCont
      else if (hasDeadVar op_variable vList)
        then trace("E YES Dead: " ++ line)elimination (change || True) nextCont fname vList pList preCont
        else trace("E YES Live: " ++ line)elimination (change || False) nextCont fname vList  pList(preCont ++ [line])

  | otherwise = do
    let (x, (var, ops)) = statement (strip line)
        op_var = filter (not.isInfixOf "fn") (filter (not.isInfixOf "bb") $ filter (isPrefixOf str_var) ops)
    if (hasDeadVar op_var vList)
      then trace("E Dead: " ++ line)elimination (change || True) nextCont fname vList pList preCont
      else trace("E Live: " ++ line)elimination (change || False) nextCont fname vList  pList(preCont ++ [line])
