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

propagation :: [String] -> String -> [LeftVar] -> [String] -> ([String], [LeftVar])
propagation [] fname vList preCont = (preCont, vList)
propagation (line: nextCont) fname vList preCont
  | (isFunction line) = propagation nextCont (getFunctionName line) vList (preCont ++ [line])
  | (isBasicBlock line || isBlockLabel line) = propagation nextCont fname vList (preCont ++ [line])
  | ((not.null) fname && (isInfixOf " = " line)) = do
    let (x, (xvar, xop)) = statement (strip line)
        v = fromJust x
    if (isConv xvar)
      then do
        let (new_nextCont, newList) = propagateTypeVar nextCont (ty xvar) (v) (ty1 xvar) (value xvar) vList []
        propagation new_nextCont fname newList (preCont ++ [line])

      else if (isSemicolon xvar)
        then do
          let from_v = fromJust $ lookupList v vList
              fromT = vtype from_v
              to_v = lookupList (low xvar) vList
              toT = bool (vtype $ fromJust to_v) "none" (isNothing to_v)

              (new_nextCont, newList) = propagateTypeVar nextCont fromT (v) toT (low xvar) vList []
          propagation new_nextCont fname newList (preCont ++ [line])

        else propagation nextCont fname vList (preCont ++ [line])

  | otherwise = propagation nextCont fname vList (preCont ++ [line])

hasDeadVar [] vList = False
hasDeadVar (v:vs) vList
  | (isNothing vInfo) = True
  | otherwise = False || (hasDeadVar vs vList)
  where vInfo = lookupList v vList

elimination :: Bool -> [String] -> String -> [LeftVar] -> [String] -> ([String], [LeftVar])
elimination False [] fname vList preCont = (preCont, vList)
elimination True [] fname vList preCont = elimination False preCont "" vList []
elimination change (line : nextCont) fname vList preCont
  | (isFunction line) = elimination (change || False) nextCont (getFunctionName line) vList (preCont ++ [line])
  | ((not.null) fname && (isInfixOf " = " line)) = do
    let x = statement (strip line)
        v = fromJust (fst x)
        use = filter (not.null) (findUse v nextCont)

    if (null use)
      then do
        let vInfo = lookupList v vList
            info = fromJust vInfo
            newList = removeVariable info vList []
        elimination (change || True) nextCont fname vList preCont
      else elimination (change || False) nextCont fname vList (preCont ++ [line])

  | ((not.isBasicBlock) line && (not.isBlockLabel) line) = do
    let (x, (var, ops)) = statement (strip line)
        op_var = filter (not.isInfixOf "fn") (filter (not.isInfixOf "bb") $ filter (isPrefixOf str_var) ops)
    if (hasDeadVar op_var vList)
      then elimination (change || True) nextCont fname vList preCont
      else elimination (change || False) nextCont fname vList (preCont ++ [line])
  | otherwise = elimination (change || False) nextCont fname vList (preCont ++ [line])
