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

    if (isUsePtr ptr state) -- trace(ptr ++ ": (" ++ (bool "x" "v" (isUsePtr ptr state) )++ ") " ++ line)
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
    if (isUsePtr ptr line) --trace(ptr ++ ": (" ++ (bool "x" "v" (isUsePtr ptr line) )++ ") " ++ line)
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
                newList = trace("> " ++ line) updateList (setState newState v') vList []
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

propagation :: String -> [String] -> [(String, String)] -> [LeftVar] -> [RP] -> [String] -> ([String], ([LeftVar], [RP]))
propagation _ [] nameList vList pList preCont = (preCont, (vList, pList))
propagation fname (line: nextCont) nameList vList pList preCont
  | (isFunctionEnd line) = (preCont ++ [line], (vList, pList))
  | (isFunction line || isBasicBlock line || isBlockLabel line) = propagation fname nextCont nameList vList pList (preCont ++ [line])
  | (isLHS line fname) = do
    let (x, (xvar, xreg)) = statement (strip line)
        v = fromJust x
        -- base = reg_32 ++ reg_64 ++ reg_ip

    if (isRegPointer line && not (isInfixOf "_ptr" v) && not (elem v reg_base))
      then do
        -- REGISTER --
        let rp = fromJust $ lookupList_p v pList
            lv = fromJust $ lookupList v vList

        if (isColon xvar)
          then do
            let rp' = rp{ rstate=(getState lv), permit=True }
                newLine = concat[v, " = ", getRstate rp']
                newList = updateList_p rp' pList []
                (new_nextCont, new_vList) = propagateRegister nextCont v rp' vList []
            propagation fname new_nextCont nameList vList newList (preCont ++ [newLine])

          else do
            let newLine = concat[v, " = ", getRstate rp] -- trace(v ++ ": " ++ getRstate rp) c
                (new_nextCont, new_vList) = bool (nextCont, vList) (propagateRegister nextCont v rp vList []) (getPermit rp)
              -- propagation nextCont  vList pList (preCont ++ [newLine])
            propagation fname new_nextCont nameList new_vList pList (preCont ++ [newLine])

      else do
        -- VARIABLE --
        let lv = fromJust $ lookupList v vList

        case (getInstr lv) of
          "conversion" -> do
            let (oldType, oldVar) = (ty xvar, v)
                (newType, newVar) = (ty1 xvar, head xreg)
                (new_nextCont, newList) = propagateVariable nextCont oldVar newVar vList []
            propagation fname new_nextCont nameList newList pList (preCont ++ [v ++ " = " ++ head xreg])

          "colon" -> do
            let oldVar = v
                newVar = low xvar
                (new_nextCont, newList) = trace("> " ++ line)propagateVariable nextCont oldVar newVar vList []
            propagation fname new_nextCont nameList newList pList (preCont ++ [v ++ " = " ++ head xreg])

          "binaryOP" -> do
            let new_nextCont = replace' v (getState lv) nextCont
            propagation fname new_nextCont nameList vList pList (preCont ++ [line])

          _->
            if (isLoad xvar)
              then do
                let newState = "[" ++ head xreg ++ "]"
                    v' = trace("> " ++ line) fromJust $ lookupList v vList
                    newList = updateList (setState newState v') vList []
                propagation fname nextCont nameList newList pList (preCont ++ [v ++ " = " ++ newState])
              else
                propagation fname nextCont nameList vList pList (preCont ++ [line])

  | (isPrefixOf "store" line) = do
    -- STORE statement with REGISTER pointer
    let s = storeStatement line
        value = str_v s
        location = str_at s
    if (isInfixOf "_ptr" line || elem location reg_base)
        then
          propagation fname nextCont nameList vList pList preCont
        else do
          let str = lookup location nameList
              vname = bool (fromJust str) (str_var ++ (names !! length nameList)) (isNothing str)
              pair = trace("(" ++ location ++ ", " ++ vname ++ ")")(location, vname)

              newLine = replace location vname line
              new_nextCont = replace' location vname nextCont
              new_preCont = replace' location vname preCont
              newList = map head $ (group.sort) (pair : nameList)

          propagation fname new_nextCont newList vList pList (new_preCont ++ [newLine])

  | otherwise = propagation fname nextCont nameList vList pList (preCont ++ [line])
