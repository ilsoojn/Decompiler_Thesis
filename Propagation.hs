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

    if (isUsePtr ptr state) --trace(ptr ++ ": (" ++ (bool "x" "v" (isUsePtr ptr state) )++ ") " ++ line)
      then do
        let (x, (var, reg)) = statement line
            v' = fromJust $ lookupList v vList

        case (instrType var) of
          "binary" -> do
            let n = read (bool (head reg) (last reg) (ptr == head reg)) :: Integer

            case (sym var) of
              "+" -> do
                let newState = concat[(getBase p), " +", show (getIndex p + n)]
                    newList = updateList (setState newState v') vList []
                propagateRegister nextCont ptr p newList (content ++ [v ++ " = " ++ newState])--trace(" (" ++ ptr ++ " -> " ++ newState ++") " ++ line)
              "-" -> do
                let newState = concat[(getBase p), " -", show (getIndex p - n)]
                    newList = updateList (setState newState v') vList []
                propagateRegister nextCont ptr p newList (content ++ [v ++ " = " ++ newState]) --trace(" (" ++ ptr ++ " -> " ++ newState ++") " ++ line)
              _ -> propagateRegister nextCont ptr p vList (content ++ [v ++ " = " ++ (replace ptr (getRstate p) state)])

          "conversion" ->
            propagateRegister nextCont ptr p (updateList (setState (getRstate p) v') vList []) (content ++ [v ++ " = " ++ (getRstate p)]) --trace(" (" ++ ptr ++ " -> " ++ (getRstate p) ++") " ++ line)

          "none" -> do

            if (isInfixOf "+" state || isInfixOf "-" state)
              then do
                let sym = bool "+" "-" (isInfixOf "-" state)
                    (pos, neg) = (sym == "+", sym == "-")

                    (a, b) = bool (strSplit' "+" state) (strSplit' "-" state) neg
                if (isNum a || isNum b)
                  then do
                    let n = read (bool a b (isMatchPointer ptr [a])) :: Integer
                        idx = getIndex p
                        idxSum = bool (idx + n) (idx - n) neg
                        sym = bool "+" "" (idxSum < 0)
                        baseStr = bool (getBase p ++ sym) "" (getBase p == "")
                        newState = baseStr ++ show idxSum
                        newList = updateList (setState newState v') vList []
                    propagateRegister nextCont ptr p newList (content ++ [v ++ " = " ++ newState])
                  else
                    propagateRegister nextCont ptr p vList (content ++ [v ++ " = " ++ (replace ptr (getRstate p) state)])

                else propagateRegister nextCont ptr p vList (content ++ [v ++ " = " ++ (replace ptr (getRstate p) state)])

          _ -> propagateRegister nextCont ptr p vList (content ++ [v ++ " = " ++ (replace ptr (getRstate p) state)]) --

    else propagateRegister nextCont ptr p vList (content ++ [line])

  | otherwise = do
    if (isUsePtr ptr line) --trace(ptr ++ ": (" ++ (bool "x" "v" (isUsePtr ptr line) )++ ") " ++ line)
      then propagateRegister nextCont ptr p vList (content ++ [replace ptr (getRstate p) line]) --trace(" (" ++ ptr ++ " -> " ++ (getRstate p) ++") " ++ line)
      else propagateRegister nextCont ptr p vList (content ++ [line])

propagateVariable :: [String] -> String -> String -> [LeftVar] -> [String] -> ([String], [LeftVar])
propagateVariable [] old new vList preCont = (preCont, vList)
propagateVariable (line : nextCont) old new vList preCont
  | (isFunction line || isBasicBlock line || isBlockLabel line || isFunctionEnd line) = propagateVariable nextCont old new vList (preCont ++ [line])
  | (isInfixOf " = " line) = do --trace("\t" ++ old ++ " ("++ (bool "x" "v" (isUse old line)) ++ ") " ++ line ++ "\n\t\t" ++ (replace old new line))

    let [v, state] = splitOn' " = " line
    if (isUse old state) --trace(" (" ++ old ++ ", " ++ new ++") : " ++ (bool "x" "v" (isUse old state)) ++ " :" ++ line)
      then do
        let (x, (var, reg)) = statement line

        if (op var == "load")
          then do
            let newState = "[" ++ new ++ "]"
                v' = fromJust $ lookupList v vList
                newList = updateList (setState newState v') vList []
            propagateVariable nextCont old new newList (preCont ++ [replace old new line])--(preCont ++ [v ++ " = [" ++ new ++ "]"]) --trace(" (" ++ old ++ " -> " ++ new ++") " ++ line)

          else do
            let newState = replace old new state
                v' = fromJust $ lookupList v vList --trace(" " ++ state ++ " -> " ++ newState)
                newList = updateList (setState newState v') vList []
            propagateVariable nextCont old new newList (preCont ++ [ v ++ " = " ++ newState ]) --trace(" (" ++ old ++ " -> " ++ new ++") " ++ line)

      else propagateVariable nextCont old new vList (preCont ++ [line])

  | otherwise = do

    if (isUse old line)
      then do
        let newLine = replace old new line
            -- v' = fromJust $ lookupList old vList
            -- newList = updateList (setState newLine v') vList []
        propagateVariable nextCont old new vList (preCont ++ [newLine]) --trace(" (" ++ old ++ " -> " ++ new ++") " ++ line)

      else propagateVariable nextCont old new vList (preCont ++ [line])

propagation :: String -> [String] -> [LeftVar] -> [RP] -> [String] -> ([String], ([LeftVar], [RP]))
propagation _ [] vList pList preCont = (preCont, (vList, pList))
propagation fname (line: nextCont) vList pList preCont
  | (isFunction line || isFunctionEnd line || isBasicBlock line || isBlockLabel line || isEntryExit line) = propagation fname nextCont vList pList (preCont ++ [line])
  | (isLHS line fname) = do
    let (x, (xvar, xreg)) = statement (strip line)
        v = fromJust x

    if (isRegPointer line)
      then do
        -- REGISTER --
        if (isInfixOf "_init" line || isInfixOf "_ptr" line || elem v reg_base)
          then -- REGISTER: initial or ptr --
            propagation fname nextCont vList pList (preCont ++ [line])
          else do
            let rp = fromJust $ lookupList_p v pList
                lv = fromJust $ lookupList v vList

            if (op xvar == "colon")
              then do
                -- %REG = a : b
                let rp' = rp{ rstate=(low xvar), permit=True } --rp' = rp{ rstate=(getState lv), permit=True }
                    newLine = concat[v, " = ", getRstate rp'] --trace(" " ++v ++ " (" ++ low xvar ++ ")")
                    newList = updateList_p rp' pList []
                    (new_nextCont, new_vList) = propagateRegister nextCont v rp' vList []
                propagation fname new_nextCont vList newList (preCont ++ [newLine])

              else do
                let newLine = concat[v, " = ", getRstate rp] --trace(v ++ ": " ++ getRstate rp) c
                    (new_nextCont, new_vList) = bool (nextCont, vList) (propagateRegister nextCont v rp vList []) (getPermit rp)
                propagation fname new_nextCont new_vList pList (preCont ++ [newLine])

      else do
        -- VARIABLE --
        let lv = fromJust $ lookupList v vList

        case (instrType xvar) of
          "conversion" -> do
            let (oldType, oldVar) = (ty xvar, v)
                (newType, newVar) = (ty1 xvar, head xreg)
                (new_nextCont, newList) = propagateVariable nextCont oldVar newVar vList []
            propagation fname new_nextCont newList pList (preCont ++ [v ++ " = " ++ head xreg])

          "colon" -> do
            let oldVar = v
                newVar = low xvar
                (new_nextCont, newList) = propagateVariable nextCont oldVar newVar vList []
            propagation fname new_nextCont newList pList (preCont ++ [v ++ " = " ++ newVar])

          "binaryOP" -> do
            let new_nextCont = replace' v (getState lv) nextCont
            propagation fname new_nextCont vList pList (preCont ++ [line])

          _->
            if (op xvar == "load")
              then do
                let newState = replace v (head xreg) (snd $  strSplit' "=" line)--"[" ++ head xreg ++ "]"
                    v' = fromJust $ lookupList v vList
                    newList = updateList (setState newState v') vList []
                propagation fname nextCont newList pList (preCont ++ [v ++ " = " ++ newState])
              -- else propagation fname nextCont vList pList (preCont ++ [line])
              else
                if (op xvar == "equal")
                  then propagation fname (replace' v (getRHS line) nextCont) vList pList (preCont ++ [line])
                  else propagation fname nextCont vList pList (preCont ++ [line])

  | otherwise = propagation fname nextCont vList pList (preCont ++ [line])
