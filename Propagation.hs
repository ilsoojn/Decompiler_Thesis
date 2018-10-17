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
import RegexFunction
import Lists

propagateRegister :: [String] -> String -> RP -> [LeftVar] -> [String] -> ([String], [LeftVar])
propagateRegister [] ptr p vList content = (content, vList)
propagateRegister (line : nextCont) ptr p vList content
  | (isFunction line || isBasicBlock line || isBlockLabel line) = propagateRegister nextCont ptr p vList (content ++ [line])
  | (isInfixOf " = " line) = do
    let [v, state] = splitOn' " = " line

    if trace(ptr ++ ": (" ++ (bool "x" "v" (isUsePtr ptr state) )++ ") " ++ line)(isUsePtr ptr state) -- trace(ptr ++ ": (" ++ (bool "x" "v" (isUsePtr ptr state) )++ ") " ++ line)
      then do
        let (x, (var, reg)) = statement line
            v' = fromJust $ lookupList v vList

        case (instrType var) of
        -- if (isBinary var)
          -- then do
          "binary" -> do
            let n = read (bool (head reg) (last reg) (ptr == head reg)) :: Integer

            case (sym var) of
              "+" -> do
                let newState = concat[(getBase p), " +", show (getIndex p + n)]
                    newList = updateList (setState newState v') vList []
                propagateRegister nextCont ptr p newList (content ++ [v ++ " = " ++ newState])
              "-" -> do
                let newState = concat[(getBase p), " -", show (getIndex p - n)]
                    newList = updateList (setState newState v') vList []
                propagateRegister nextCont ptr p newList (content ++ [v ++ " = " ++ newState])
              _ -> propagateRegister nextCont ptr p vList (content ++ [v ++ " = " ++ (replace ptr (getRstate p) state)])

          -- else if (isConv var)
          "conversion" -> propagateRegister nextCont ptr p (updateList (setState (getRstate p) v') vList []) (content ++ [v ++ " = " ++ (getRstate p)])

          "none" -> do

            if (isInfixOf "+" state)
              then do
                let (a, b) = strSplit' "+" state
                    n = read (bool a b (isMatchPointer ptr [a])) :: Integer
                    idxSum = (getIndex p) + n
                    sym = bool "+" "" (idxSum < 0)
                    baseStr = bool (getBase p ++ sym) "" (getBase p == "")
                    newState = baseStr ++ show idxSum
                    newList = updateList (setState newState v') vList []
                propagateRegister nextCont ptr p newList (content ++ [v ++ " = " ++ newState])

              else if (isInfixOf "-" state)
                then do
                  let (a, b) = strSplit' "-" state
                      n = read (bool a b (isMatchPointer ptr [a])) :: Integer
                      idxSum = (getIndex p) - n
                      sym = bool "+" "" (idxSum < 0)
                      baseStr = bool (getBase p ++ sym) "" (getBase p == "")
                      newState = baseStr ++ show idxSum
                      newList = updateList (setState newState v') vList []
                  propagateRegister nextCont ptr p newList (content ++ [v ++ " = " ++ newState])

                else propagateRegister nextCont ptr p vList (content ++ [v ++ " = " ++ (replace ptr (getRstate p) state)])

          _ -> propagateRegister nextCont ptr p vList (content ++ [v ++ " = " ++ (replace ptr (getRstate p) state)])

    else propagateRegister nextCont ptr p vList (content ++ [line])

  | otherwise = do
    if trace(ptr ++ ": (" ++ (bool "x" "v" (isUsePtr ptr line) )++ ") " ++ line)(isUsePtr ptr line)
      then propagateRegister nextCont ptr p vList (content ++ [replace ptr (getRstate p) line])
      else propagateRegister nextCont ptr p vList (content ++ [line])

propagateVariable :: [String] -> String -> String -> [LeftVar] -> [String] -> ([String], [LeftVar])
propagateVariable [] old new vList preCont = (preCont, vList)
propagateVariable (line : nextCont) old new vList preCont
  | (isFunction line || isBasicBlock line || isBlockLabel line || isFunctionEnd line) = propagateVariable nextCont old new vList (preCont ++ [line])
  | (isInfixOf " = " line) = do --trace("\t" ++ old ++ " ("++ (bool "x" "v" (isUse old line)) ++ ") " ++ line ++ "\n\t\t" ++ (replace old new line))
    let [v, state] = splitOn' " = " line
    if (isUse old state)
      then do
        let (x, (var, reg)) = statement line

        if (isLoad var)
          then do
            let newState = "[" ++ new ++ "]"
                v' = fromJust $ lookupList v vList
                newList = updateList (setState newState v') vList []
            propagateVariable nextCont old new newList (preCont ++ [v ++ " = [ " ++ new ++ " ]"])
          else do
            let newState = replace old new state
                v' = fromJust $ lookupList v vList
                newList = updateList (setState newState v') vList []
            propagateVariable nextCont old new newList (preCont ++ [ v ++ " = " ++ newState ])
          -- else if (isBinary var)
          --   then expression
          --   else if (isBitwise)
          --     then expression
          --     else propagateVariable nextCont v new vList (preCont ++ [line])
      else propagateVariable nextCont old new vList (preCont ++ [line])

  | otherwise = do

    if (isUse old line)
      then do
        let newLine = replace old new line
            -- v' = fromJust $ lookupList old vList
            -- newList = updateList (setState newLine v') vList []
        propagateVariable nextCont old new vList (preCont ++ [newLine])

      else propagateVariable nextCont old new vList (preCont ++ [line])
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
        -- REGISTER --
        let rp = fromJust $ lookupList_p v pList
            lv = fromJust $ lookupList v vList

        if (isSemicolon xvar)
          then do
            let rp' = rp{ rstate=(getState lv), permit=True }
                newLine = concat[v, " = ", getRstate rp']
                newList = updateList_p rp' pList []
                (new_nextCont, new_vList) = propagateRegister nextCont v rp' vList []
            propagation new_nextCont fname vList newList (preCont ++ [newLine])

          else do
            let newLine = trace(v ++ ": " ++ getRstate rp) concat[v, " = ", getRstate rp]
                (new_nextCont, new_vList) = bool (nextCont, vList) (propagateRegister nextCont v rp vList []) (getPermit rp)
              -- propagation nextCont fname vList pList (preCont ++ [newLine])
            propagation new_nextCont fname new_vList pList (preCont ++ [newLine])

      else do
        -- VARIABLE --
        let lv = fromJust $ lookupList v vList

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
            else if (getInstr lv == "binaryOP")
              then do
                let new_nextCont = replace' v (getState lv) nextCont
                propagation new_nextCont fname vList pList (preCont ++ [line])
              else propagation nextCont fname vList pList (preCont ++ [line])

  | otherwise = propagation nextCont fname vList pList (preCont ++ [line])
