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

propagateRegister :: Function -> [String] -> String -> RP -> [String] -> ([String], Function)
propagateRegister f [] ptr rptr content = (content, f)
propagateRegister f (line : nextCont) ptr rptr content
  | (isFunction line || isBasicBlock line || isBlockLabel line) = propagateRegister f nextCont ptr rptr (content ++ [line])
  | (isInfixOf " = " line) = do
    let [v, state] = splitOn' " = " line

    if (hasPtr ptr state) --trace(ptr ++ ": (" ++ (bool "x" "v" (hasPtr ptr state) )++ ") " ++ line)
      then do
        let (x, (var, reg)) = statement line
            vList = variables f
            v' = fromJust $ lookupList v vList

        case (instrType var) of
          "binary" -> do
            let n = read (bool (head reg) (last reg) (ptr == head reg)) :: Integer

            case (sym var) of
              "+" -> do
                let newState = concat[(getBase rptr), "+", show (getIndex rptr + n)]
                    newList = updateList (setState newState v') vList []
                propagateRegister (f{variables = newList}) nextCont ptr rptr (content ++ [v ++ " = " ++ newState])--trace(" (" ++ ptr ++ " -> " ++ newState ++") " ++ line)
              "-" -> do
                let newState = concat[(getBase rptr), "-", show (getIndex rptr - n)]
                    newList = updateList (setState newState v') vList []
                propagateRegister (f{variables = newList}) nextCont ptr rptr (content ++ [v ++ " = " ++ newState]) --trace(" (" ++ ptr ++ " -> " ++ newState ++") " ++ line)
              _ -> propagateRegister f nextCont ptr rptr(content ++ [v ++ " = " ++ (replace ptr (getRstate rptr) state)])

          "conversion" -> do
            let newList = updateList (setState (getRstate rptr) v') vList []
            propagateRegister (f{variables = newList}) nextCont ptr rptr(content ++ [v ++ " = " ++ (getRstate rptr)]) --trace(" (" ++ ptr ++ " -> " ++ (getRstate rptr) ++") " ++ line)

          "none" -> do
            if (isInfixOf " + " state || isInfixOf " - " state)
              then do
                let rv = fromJust $ lookupList_p v (registers f) -- v in registerList
                    sym = bool " + " " - " (isInfixOf " - " state)
                    (pos, neg) = (sym == " + ", sym == " - ")
                    (a, b) = bool (strSplit' " + " state) (strSplit' " - " state) neg
                --
                -- let front = bool
                if trace("(" ++ a ++ ", " ++ b ++")")(hasPtr ptr a)
                  -- a = ptr [+/- n]
                  then do
                    let tmpStr = (unwords $ map strip $ replace' ptr (getRstate rptr) [a]) ++ sym ++ b
                        newList_p = updateList_p (setRstate tmpStr rv) (registers f) []
                        newList_v = updateList (setState tmpStr v') vList []
                    trace("A: " ++ ptr ++ " -> "++ (getRstate rptr) ++ " > " ++ line ++ " -> "++ tmpStr) propagateRegister (f{registers = newList_p, variables = newList_v}) nextCont ptr rptr(content ++ [v ++ " = " ++ tmpStr])
                  -- b = ptr [+/- n]
                  else do
                    let tmpStr = a ++ sym ++ (unwords $ map strip $ replace' ptr (getRstate rptr) [b])
                        newList_p = updateList_p (setRstate tmpStr rv) (registers f) []
                        newList_v = updateList (setState tmpStr v') vList []
                    trace("B: " ++ ptr ++ " -> "++ (getRstate rptr) ++ " > " ++ line ++ " -> "++ tmpStr) propagateRegister (f{registers = newList_p, variables = newList_v}) nextCont ptr rptr(content ++ [v ++ " = " ++ tmpStr])

              else if (isInfixOf "+" state || isInfixOf "-" state)
                then do
                  let sym = bool "+" "-" (isInfixOf "-" state)
                      (pos, neg) = (sym == "+", sym == "-")

                      (a, b) = bool (strSplit' "+" state) (strSplit' "-" state) neg
                  if (isNum a || isNum b)
                    then do
                      let n = read (bool a b (isMatchPointer ptr [a])) :: Integer
                          idx = getIndex rptr
                          idxSum = bool (idx + n) (idx - n) neg
                          sym = bool "+" "" (idxSum < 0)
                          baseStr = bool (getBase rptr++ sym) "" (getBase rptr== "")
                          newState = baseStr ++ show idxSum
                          newList = updateList (setState newState v') vList []
                      propagateRegister (f{variables = newList}) nextCont ptr rptr(content ++ [v ++ " = " ++ newState])
                    else
                      propagateRegister f nextCont ptr rptr(content ++ [v ++ " = " ++ (replace ptr (getRstate rptr) state)])

              else propagateRegister f nextCont ptr rptr(content ++ [v ++ " = " ++ (replace ptr (getRstate rptr) state)])

          _ -> propagateRegister f nextCont ptr rptr(content ++ [v ++ " = " ++ (replace ptr (getRstate rptr) state)]) --

    else propagateRegister f nextCont ptr rptr(content ++ [line])

  | otherwise = do
    if (hasPtr ptr line) --trace(ptr ++ ": (" ++ (bool "x" "v" (hasPtr ptr line) )++ ") " ++ line)
      then propagateRegister f nextCont ptr rptr(content ++ [replace ptr (getRstate rptr) line]) --trace(" (" ++ ptr ++ " -> " ++ (getRstate rptr) ++") " ++ line)
      else propagateRegister f nextCont ptr rptr(content ++ [line])

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

propagation :: Function -> [String] -> [String] -> Function
propagation f [] newCode = f{code = newCode}
propagation f (line: nextCont) preCont
  | (isFunction line || isFunctionEnd line || isBasicBlock line || isBlockLabel line || isEntryExit line) = propagation f nextCont (preCont ++ [line])
  | (isLHS line (fname f)) = do
    let (x, (xvar, xreg)) = statement (strip line)
        v = fromJust x

        vList = variables f
        lv = fromJust $ lookupList v vList

    if (isRegPointer line)
      then do
        -- REGISTER --
        if (isInfixOf "_init" line || isInfixOf "_ptr" line || elem v reg_base)
          then -- REGISTER: initial or ptr --
            propagation f nextCont (preCont ++ [line])
          else do
            let pList = registers f
                rp = fromJust $ lookupList_p v pList

            if (instrType xvar == "colon")
              then do
                -- %REG = a : b
                let rp' = rp{ rstate=(low xvar), permit=True } --rp' = rp{ rstate=(getState lv), permit=True }
                    newLine = concat[v, " = ", getRstate rp'] --trace(" " ++v ++ " (" ++ low xvar ++ ")")
                    newList = updateList_p rp' pList []
                    (new_nextCont, fnew) = propagateRegister (f {registers = newList}) nextCont v rp' []
                propagation fnew new_nextCont (preCont ++ [newLine])

              else do
                let newLine = concat[v, " = ", getRstate rp] --trace(v ++ ": " ++ getRstate rp) c
                    (new_nextCont, fnew) = bool (nextCont, f) (propagateRegister f nextCont v rp []) (getPermit rp)
                propagation fnew new_nextCont (preCont ++ [newLine])

      else do
        -- VARIABLE --
        let lv = fromJust $ lookupList v vList

        case (instrType xvar) of
          "conversion" -> do
            let (oldType, oldVar) = (ty xvar, v)
                (newType, newVar) = (ty1 xvar, head xreg)
                (new_nextCont, newList) = propagateVariable nextCont oldVar newVar vList []
            propagation (f { variables = newList}) new_nextCont (preCont ++ [v ++ " = " ++ head xreg])

          "colon" -> do
            let oldVar = v
                newVar = low xvar
                (new_nextCont, newList) = propagateVariable nextCont oldVar newVar vList []
            propagation (f { variables = newList}) new_nextCont (preCont ++ [v ++ " = " ++ newVar])

          "binaryOP" -> do
            let new_nextCont = replace' v (getState lv) nextCont
            propagation f new_nextCont (preCont ++ [line])

          _->
            if (op xvar == "load")
              then do
                let newState = replace v (head xreg) (snd $  strSplit' "=" line)--"[" ++ head xreg ++ "]"
                    v' = fromJust $ lookupList v vList
                    newList = updateList (setState newState v') vList []
                propagation (f { variables = newList}) nextCont (preCont ++ [v ++ " = " ++ newState])
              -- else propagation fname nextCont vList pList (preCont ++ [line])
              else
                if (op xvar == "equal")
                  then propagation f (replace' v (getRHS line) nextCont) (preCont ++ [line])
                  else propagation f nextCont (preCont ++ [line])

  | otherwise = propagation f nextCont (preCont ++ [line])
