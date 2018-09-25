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

propagatePointer :: [String] -> String -> RP -> [String]
propagatePointer [] ptr p = []
propagatePointer (line : nextCont) ptr p
  | (isFunction line || isBasicBlock line || isBlockLabel line) = line : (propagatePointer nextCont ptr p)
  | (isInfixOf " = " line) = do
    let [v, state] = splitOn' " = " line

    if (isUsePtr ptr state)
      then do
        let (x, (var, reg)) = statement line

        if (isBinary var)
          then do
            let n = read (bool (head reg) (last reg) (ptr == head reg)) :: Integer

            case (sym var) of
              "+" -> concat[v, " = ", (getBase p), " +", show (getIndex p + n)] : (propagatePointer nextCont ptr p)
              "-" -> concat[v, " = ", (getBase p), " -", show (getIndex p - n)] : (propagatePointer nextCont ptr p)
              _ -> concat[v, " = ", (replace ptr (getRstate p) state)] : (propagatePointer nextCont ptr p)

          else if (isConv var)
            then concat[v, " = ", (getRstate p)] : (propagatePointer nextCont ptr p)
            else concat[v, " = ", (replace ptr (getRstate p) state)] : (propagatePointer nextCont ptr p)

    else line : (propagatePointer nextCont ptr p)

  | otherwise = do
    if (isUsePtr ptr line)
      then (replace ptr (getRstate p) line) : (propagatePointer nextCont ptr p)
      else line : (propagatePointer nextCont ptr p)

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
  | trace(line)(isFunction line) = propagation nextCont (getFunctionName line) vList pList (preCont ++ [line])
  | (isFunctionEnd line) = (preCont ++ [line], vList)
  | (isBasicBlock line || isBlockLabel line) = propagation nextCont fname vList pList (preCont ++ [line])
  | (isLHS line fname) = do
    let (x, (xvar, xop)) = statement (strip line)
        v = fromJust x

    if (isRegPointer line && not (isInfixOf "_ptr" v) && not (elem v $ reg_32 ++ reg_64 ++ reg_ip))
      then do
        let rp = fromJust $ lookupList_p v pList
            newLine = concat[v, " = ", getRstate rp]
            new_nextCont = bool nextCont (propagatePointer nextCont v rp) (getPermit rp)
          -- propagation nextCont fname vList pList (preCont ++ [newLine])
        propagation new_nextCont fname vList pList (preCont ++ [newLine])
      else do
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
