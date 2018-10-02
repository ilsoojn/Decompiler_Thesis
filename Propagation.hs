module Propagation where

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

propagatePointer :: [String] -> String -> RP -> [LeftVar] -> [String]
propagatePointer [] ptr p vList = []
propagatePointer (line : nextCont) ptr p vList
  | (isFunction line || isBasicBlock line || isBlockLabel line) = line : (propagatePointer nextCont ptr p vList)
  | (isInfixOf " = " line) = do
    let [v, state] = splitOn' " = " line

    if (isUsePtr ptr state)
      then do
        let (x, (var, reg)) = statement line

        if (isBinary var)
          then do
            let n = read (bool (head reg) (last reg) (ptr == head reg)) :: Integer

            case (sym var) of
              "+" -> trace(" " ++ show n ++ " :: " ++ line)concat[v, " = ", (getBase p), " +", show (getIndex p + n)] : (propagatePointer nextCont ptr p vList)
              "-" -> trace(" " ++ show n ++ " :: " ++ line)concat[v, " = ", (getBase p), " -", show (getIndex p - n)] : (propagatePointer nextCont ptr p vList)
              _ -> concat[v, " = ", (replace ptr (getRstate p) state)] : (propagatePointer nextCont ptr p vList)

          else if (isConv var)
            then concat[v, " = ", (getRstate p)] : (propagatePointer nextCont ptr p vList)
            else concat[v, " = ", (replace ptr (getRstate p) state)] : (propagatePointer nextCont ptr p vList)

    else line : (propagatePointer nextCont ptr p vList)

  | otherwise = do
    if (isUsePtr ptr line)
      then (replace ptr (getRstate p) line) : (propagatePointer nextCont ptr p vList)
      else line : (propagatePointer nextCont ptr p vList)

propagateVariable :: [String] -> String -> String -> [LeftVar] -> [String] -> ([String], [LeftVar])
propagateVariable [] orgV new vList preCont = (preCont, vList)
propagateVariable (line : nextCont) orgV new vList preCont
  | (isFunction line || isBasicBlock line || isBlockLabel line || isFunctionEnd line) = propagateVariable nextCont orgV new vList (preCont ++ [line])
  | (isInfixOf " = " line) = do
    let [v, state] = splitOn' " = " line
    if (isUse orgV state)
      then do
        let (x, (var, reg)) = statement line

        if (isLoad var)
          then propagateVariable nextCont orgV new vList (preCont ++ [v ++ " = [ " ++ new ++ " ]"])

          else propagateVariable nextCont orgV new vList (preCont ++ [ replace orgV new line ])
          -- else if (isBinary var)
          --   then expression
          --   else if (isBitwise)
          --     then expression
          --     else propagateVariable nextCont v new vList (preCont ++ [line])
      else propagateVariable nextCont orgV new vList (preCont ++ [line])

  | otherwise = do

    if (isUse orgV line)
      then do
        let newLine = replace orgV new line
            v' = fromJust $ lookupList orgV vList
            newList = updateList (setState newLine v') vList []
        propagateVariable nextCont orgV new newList (preCont ++ [newLine])

      else propagateVariable nextCont orgV new vList (preCont ++ [line])
--
-- propagateTypeVar :: [String] -> String -> String -> String -> String -> [LeftVar] -> [String] -> ([String], [LeftVar])
-- propagateTypeVar [] oT oV nT nV vList preCont = (preCont, vList)
-- propagateTypeVar (line : nextCont) ot ov nt nv vList preCont
--   | (isFunction line || isBasicBlock line || isBlockLabel line) = propagateTypeVar nextCont ot ov nt nv vList (preCont ++ [line])
--   | (isUse ov (bool line (last $ splitOn' " = " line) (isInfixOf " = " line))) = do
--     let (front, back) = strSplit ot (replace ov nv line)
--         newline = bool (front ++ nt ++ back) (front) (null back)
--         vtmp = lookupList ov vList
--
--     if ((not.isNothing) vtmp) -- line: def(someVariable) = .... oldVar
--       then do
--         let vi = fromJust vtmp
--             new_vtype  = bool ot nt ((vtype vi) == ot)
--             newInfo = vi{ vtype=new_vtype, state=newline }
--             newList = updateList newInfo vList []
--         propagateTypeVar nextCont ot ov nt nv newList (preCont ++ [newline])
--       else propagateTypeVar nextCont ot ov nt nv vList (preCont ++ [newline])
--
--   | otherwise = propagateTypeVar nextCont ot ov nt nv vList (preCont ++ [line])

propagation :: [String] -> String -> [LeftVar] -> [RP] -> [String] -> ([String], ([LeftVar], [RP]))
propagation [] fname vList pList preCont = (preCont, (vList, pList))
propagation (line: nextCont) fname vList pList preCont
  | (isFunction line) = propagation nextCont (getFunctionName line) vList pList (preCont ++ [line])
  | (isFunctionEnd line) = (preCont ++ [line], (vList, pList))
  | (isBasicBlock line || isBlockLabel line) = propagation nextCont fname vList pList (preCont ++ [line])
  | (isLHS line fname) = do
    let (x, (xvar, xreg)) = statement (strip line)
        v = fromJust x
        base = reg_32 ++ reg_64 ++ reg_ip
    if (isRegPointer line && not (isInfixOf "_ptr" v) && not (elem v $ base))
      then do
        let rp = fromJust $ lookupList_p v pList
            lv = fromJust $ lookupList v vList

        if (isSemicolon xvar)
          then do
            let rp' = rp{ rstate=(getState lv), permit=True }
                newLine = concat[v, " = ", getRstate rp']
                newList = updateList_p rp' pList []
                new_nextCont = bool nextCont (propagatePointer nextCont v rp' vList) (getPermit rp')
            propagation new_nextCont fname vList newList (preCont ++ [newLine])

          else do
            let newLine = concat[v, " = ", getRstate rp]
                new_nextCont = bool nextCont (propagatePointer nextCont v rp vList) (getPermit rp)
              -- propagation nextCont fname vList pList (preCont ++ [newLine])
            propagation new_nextCont fname vList pList (preCont ++ [newLine])

      else do
        if (isConv xvar)
          then do
            let (oldType, oldVar) = (ty xvar, v)
                (newType, newVar) = (ty1 xvar, head xreg)
                (new_nextCont, newList) = propagateVariable nextCont oldVar newVar vList []
            propagation new_nextCont fname newList pList (preCont ++ [v ++ " = " ++ head xreg])

          else if (isSemicolon xvar)
            then do
              let oldVar = v
                  newVar = low xvar
                  (new_nextCont, newList) = propagateVariable nextCont oldVar newVar vList []

              propagation new_nextCont fname newList pList (preCont ++ [v ++ " = " ++ head xreg])
            else propagation nextCont fname vList pList (preCont ++ [line])

  | otherwise = propagation nextCont fname vList pList (preCont ++ [line])
