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

propRegister :: Function -> String -> RP -> String -> String -> String -> String -> (Function, String)
propRegister f line infoP fromP toP lhs rhs
  | (isInfixOf " + " rhs || isInfixOf " - " rhs ) = do
    if (isRegPointer lhs)
      then do
        let rv = trace(lhs)fromJust $ lookupList_p lhs (registers f) -- v in registerList
            sym = bool " + " " - " (isInfixOf " - " rhs)
            (pos, neg) = (sym == " + ", sym == " - ")
            (a, b) = bool (strSplit' " + " rhs) (strSplit' " - " rhs) neg

            convertStr = bool (unwords $ map strip $ replace' fromP toP [b]) (unwords $ map strip $ replace' fromP toP [a]) (hasPtr fromP a)
            newRHS = bool (a ++ sym ++ convertStr) (convertStr ++ sym ++ b) (hasPtr fromP a)
            newList_p = updateList_p (setRstate newRHS rv) (registers f) []
            newList_v = updateList (setState newRHS v) vList []

        trace(" (" ++ fromP ++ " -> "++ toP ++ ") " ++ line ++" -> " ++ lhs++ " = " ++ newRHS)(f{registers = newList_p, variables = newList_v} , unwords[lhs, "=", newRHS])

      else do
        let sym = bool " + " " - " (isInfixOf " - " rhs)
            (pos, neg) = (sym == " + ", sym == " - ")
            (a, b) = bool (strSplit' " + " rhs) (strSplit' " - " rhs) neg

            convertStr = bool (unwords $ map strip $ replace' fromP toP [b]) (unwords $ map strip $ replace' fromP toP [a]) (hasPtr fromP a)
            newRHS = bool (a ++ sym ++ convertStr) (convertStr ++ sym ++ b) (hasPtr fromP a)
            newList_v = updateList (setState newRHS v) vList []
        trace(" (" ++ fromP ++ " -> "++ toP ++ ") " ++ line ++" -> " ++ lhs++ " = " ++ newRHS)(f{variables = newList_v} , unwords[lhs, "=", newRHS])

  | (isInfixOf "+" rhs || isInfixOf "-" rhs) = do
    let sym = bool "+" "-" (isInfixOf "-" rhs)
        (pos, neg) = (sym == "+", sym == "-")
        (a, b) = bool (strSplit' "+" rhs) (strSplit' "-" rhs) neg

    if (isNum a || isNum b)
      then do
        let n = read (bool a b (isMatchPointer fromP [a])) :: Integer
            idx = getIndex infoP
            idxSum = bool (idx + n) (idx - n) neg
            sym = bool "+" "" (idxSum < 0)
            baseStr = bool (getBase infoP++ sym) "" (getBase infoP == "")
            newRHS = baseStr ++ show idxSum
            newList = updateList (setState newRHS v) vList []

        trace(" (" ++ fromP ++ " -> "++ toP ++ ") " ++ line ++" -> " ++ lhs++ " = " ++ newRHS)(f{variables = newList}, unwords[lhs, "=", newRHS])
      else
        trace(" (" ++ fromP ++ " -> "++ toP ++ ") " ++ line ++" -> " ++ lhs++ " = " ++ (replace fromP toP rhs))(f, unwords [lhs, "=", replace fromP toP rhs])
  | otherwise = trace(" (" ++ fromP ++ " -> "++ toP ++ ") " ++ line ++" -> " ++ lhs++ " = " ++ (replace fromP toP rhs))(f, unwords [lhs, "=", replace fromP toP rhs])
    where (x, (var, reg)) = statement line
          (vList, pList) = (variables f, registers f)
          v = fromJust $ lookupList lhs vList

propagateRegister :: Function -> [String] -> String -> RP -> [String] -> ([String], Function)
propagateRegister f [] ptr rptr content = (content, f)
propagateRegister f (line : nextCont) ptr rptr content
  | (isFunction line || isBasicBlock line || isBlockLabel line) = propagateRegister f nextCont ptr rptr (content ++ [line])
  | (isInfixOf " = " line) = do
    let [lhs, rhs] = splitOn' " = " line

    if (hasPtr ptr rhs) --trace(ptr ++ ": (" ++ (bool "x" "v" (hasPtr ptr state) )++ ") " ++ line)
      then do
        let (x, (var, reg)) = statement line
            vList = variables f
            v = fromJust $ lookupList lhs vList
            rnewRHS = (getRstate rptr)

        case (instrType var) of
          "binary" -> do
            let n = read (bool (head reg) (last reg) (ptr == head reg)) :: Integer

            case (sym var) of
              "+" -> do
                let newRHS = concat[(getBase rptr), "+", show (getIndex rptr + n)]
                    newList = updateList (setState newRHS v) vList []
                trace(" (" ++ ptr ++ " -> "++ rnewRHS ++ ") " ++ line ++" -> " ++ lhs++ " = " ++ newRHS)propagateRegister (f{variables = newList}) nextCont ptr rptr (content ++ [lhs++ " = " ++ newRHS])--trace(" (" ++ ptr ++ " -> " ++ newState ++") " ++ line)
              "-" -> do
                let newRHS = concat[(getBase rptr), "-", show (getIndex rptr - n)]
                    newList = updateList (setState newRHS v) vList []
                trace(" (" ++ ptr ++ " -> "++ rnewRHS ++ ") " ++ line ++" -> " ++ lhs++ " = " ++ newRHS)propagateRegister (f{variables = newList}) nextCont ptr rptr (content ++ [lhs ++ " = " ++ newRHS]) --trace(" (" ++ ptr ++ " -> " ++ newState ++") " ++ line)
              _ -> trace(" (" ++ ptr ++ " -> "++ rnewRHS ++ ") " ++ line ++" -> " ++ lhs++ " = " ++ (replace ptr rnewRHS rhs))propagateRegister f nextCont ptr rptr(content ++ [lhs ++ " = " ++ (replace ptr rnewRHS rhs)])

          "conversion" -> do
            let newList = updateList (setState rnewRHS v) vList []
            trace(" (" ++ ptr ++ " -> "++ rnewRHS ++ ") " ++ line ++" -> " ++ lhs++ " = " ++ rnewRHS)propagateRegister (f{variables = newList}) nextCont ptr rptr(content ++ [lhs ++ " = " ++ rnewRHS]) --trace(" (" ++ ptr ++ " -> " ++ (getRstate rptr) ++") " ++ line)

          "none" -> do
            let (f_new, new_line) = propRegister f line rptr ptr rnewRHS lhs rhs
            propagateRegister f_new nextCont ptr rptr (content ++ [new_line])

          _ -> propagateRegister f nextCont ptr rptr (content ++ [line])

    else propagateRegister f nextCont ptr rptr (content ++ [line])

  | otherwise = do
    if (hasPtr ptr line) --trace(ptr ++ ": (" ++ (bool "x" "v" (hasPtr ptr line) )++ ") " ++ line)
      then trace(" (" ++ ptr ++ " -> "++ (getRstate rptr) ++ ") " ++ line ++" -> " ++ (replace ptr (getRstate rptr) line))propagateRegister f nextCont ptr rptr(content ++ [replace ptr (getRstate rptr) line]) --trace(" (" ++ ptr ++ " -> " ++ (getRstate rptr) ++") " ++ line)
      else propagateRegister f nextCont ptr rptr(content ++ [line])

propagateVariable :: [String] -> String -> String -> [LeftVar] -> [String] -> ([String], [LeftVar])
propagateVariable [] old new vList preCont = (preCont, vList)
propagateVariable (line : nextCont) old new vList preCont
  | (isFunction line || isBasicBlock line || isBlockLabel line || isFunctionEnd line) = propagateVariable nextCont old new vList (preCont ++ [line])
  | (isInfixOf " = " line) = do --trace("\t" ++ old ++ " ("++ (bool "x" "v" (isUse old line)) ++ ") " ++ line ++ "\n\t\t" ++ (replace old new line))

    let [lhs, rhs] = splitOn' " = " line
    if (isUse old rhs) --trace(" (" ++ old ++ ", " ++ new ++") : " ++ (bool "x" "v" (isUse old state)) ++ " :" ++ line)
      then do
        let (x, (var, reg)) = statement line

        if (op var == "load")
          then do
            let newRHS = "[" ++ new ++ "]"
                v = fromJust $ lookupList lhs vList
                newList = updateList (setState newRHS v) vList []
            trace(" (" ++ old ++ " -> "++ new ++ ") " ++ line ++" -> " ++ (replace old new line))propagateVariable nextCont old new newList (preCont ++ [replace old new line])--(preCont ++ [lhs ++ " = [" ++ new ++ "]"]) --trace(" (" ++ old ++ " -> " ++ new ++") " ++ line)

          else do
            let newRHS = replace old new rhs
                v = fromJust $ lookupList lhs vList --trace(" " ++ state ++ " -> " ++ newState)
                newList = updateList (setState newRHS v) vList []
            trace(" (" ++ old ++ " -> "++ new ++ ") " ++ line ++" -> " ++ lhs ++ " = " ++ newRHS)propagateVariable nextCont old new newList (preCont ++ [ lhs ++ " = " ++ newRHS ]) --trace(" (" ++ old ++ " -> " ++ new ++") " ++ line)

      else propagateVariable nextCont old new vList (preCont ++ [line])

  | otherwise = do

    if (isUse old line)
      then do
        let newLine = replace old new line
            -- v' = fromJust $ lookupList old vList
            -- newList = updateList (setState newLine v') vList []
        propagateVariable nextCont old new vList (preCont ++ [newLine]) --trace(" (" ++ old ++ " -> " ++ new ++") " ++ line)

      else propagateVariable nextCont old new vList (preCont ++ [line])

propagation :: Function -> [String] -> [String] -> Function
propagation f [] newCode = f{code = newCode}
propagation f (line: nextCont) preCont
  | (isFunction line || isFunctionEnd line || isBasicBlock line || isBlockLabel line || isEntryExit line) = propagation f nextCont (preCont ++ [line])
  | (isLHS line (fname f)) = do
    let (x, (xvar, xreg)) = statement (strip line)
        lhs = fromJust x

        vList = variables f
        v = fromJust $ lookupList lhs vList

    if (isRegPointer line)
      then do
        -- REGISTER --
        if (isInfixOf "_init" line || isInfixOf "_ptr" line || elem lhs reg_base)
          then -- REGISTER: initial or ptr --
            propagation f nextCont (preCont ++ [line])
          else do
            let pList = registers f
                p = fromJust $ lookupList_p lhs pList

            if (instrType xvar == "colon")
              then do
                -- %REG = a : b
                let p' = p{ rstate=(low xvar), permit=True } --rp' = rp{ rstate=(getState lv), permit=True }
                    newLine = concat[lhs, " = ", getRstate p'] --trace(" " ++v ++ " (" ++ low xvar ++ ")")
                    newList = updateList_p p' pList []
                    (new_nextCont, fnew) = propagateRegister (f {registers = newList}) nextCont lhs p' []
                propagation fnew new_nextCont (preCont ++ [newLine])

              else do
                let newLine = concat[lhs, " = ", getRstate p] --trace(v ++ ": " ++ getRstate rp) c
                    (new_nextCont, fnew) = bool (nextCont, f) (propagateRegister f nextCont lhs p []) (getPermit p)
                propagation fnew new_nextCont (preCont ++ [newLine])

      else do
        -- VARIABLE --
        case (instrType xvar) of
          "conversion" -> do
            let (oldType, oldVar) = (ty xvar, lhs)
                (newType, newVar) = (ty1 xvar, head xreg)
                (new_nextCont, newList) = propagateVariable nextCont oldVar newVar vList []
            propagation (f { variables = newList}) new_nextCont (preCont ++ [lhs ++ " = " ++ head xreg])

          "colon" -> do
            let oldVar = lhs
                newVar = low xvar
                (new_nextCont, newList) = propagateVariable nextCont oldVar newVar vList []
            propagation (f { variables = newList}) new_nextCont (preCont ++ [lhs ++ " = " ++ newVar])

          "binaryOP" -> do
            let new_nextCont = replace' lhs (getState v) nextCont
            propagation f new_nextCont (preCont ++ [line])

          _->
            if (op xvar == "load")
              then do
                let newState = replace lhs (head xreg) (snd $  strSplit' "=" line)--"[" ++ head xreg ++ "]"
                    v = fromJust $ lookupList lhs vList
                    newList = updateList (setState newState v) vList []
                propagation (f { variables = newList}) nextCont (preCont ++ [lhs ++ " = " ++ newState])
              -- else propagation fname nextCont vList pList (preCont ++ [line])
              else
                if (op xvar == "equal")
                  then propagation f (replace' lhs (getRHS line) nextCont) (preCont ++ [line])
                  else propagation f nextCont (preCont ++ [line])

  | otherwise = propagation f nextCont (preCont ++ [line])
