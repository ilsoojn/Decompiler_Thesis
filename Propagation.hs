module Propagation where

import Data.Bool
import Data.Char
import Data.DeriveTH
import Data.List
import Data.List.Split
import Data.List.Utils hiding(split)
import Data.Maybe
import Data.String.Utils hiding(split)
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

stringCalculator idx [] = idx
stringCalculator idx (n: ns) = stringCalculator (idx + n) ns

indexCalculator [] = []
indexCalculator (x:xs)
  | (isNothing rest || null (fromJust rest)) = x : (indexCalculator xs)
  | otherwise = do
    let str = fromJust rest
        noNumError = (not.null) (filter ('+'/=) $ filter ('-' /=) $ filter (not.isDigit) str)
        hasNumError = (not.or)  (map (isDigit) str)
    if ((head str /= '-' && head str /= '+') || noNumError || hasNumError)
      then x : (indexCalculator xs)
      else do
        let nList = map strToInt $ map (filter ('+' /=)) $ split (startsWithOneOf "+-") str
            idx = stringCalculator (head nList) (tail nList)

        if(isNum reg)
          then do
            let i = strToInt reg
                newX = show (i + idx)
            newX : (indexCalculator xs)

          else do
            let sym = bool "+" "" (idx < 0)
                newX = bool (reg ++ sym ++ (show idx)) reg (idx == 0)
            newX : (indexCalculator xs)

    where reg = takeWhile ('-' /=) $  takeWhile ('+' /=) x
          rest = stripPrefix reg x
  --
  -- if (isNothing rest || null (fromJust rest))
  --   then x : (indexCalculator xs)
  --   else do
  --     let str = fromJust rest
  --         noNumError = (not.null) (filter ('+'/=) $ filter ('-' /=) $ filter (not.isDigit) str)
  --         hasNumError = (not.or)  (map (isDigit) str)
  --     if ((head str /= '-' && head str /= '+') || noNumError || hasNumError)
  --       then x : (indexCalculator xs)
  --       else do
  --         let nList = map strToInt $ map (filter ('+' /=)) $ split (startsWithOneOf "+-") str
  --             idx = stringCalculator (head nList) (tail nList)
  --             sym = bool "+" "" (idx < 0)
  --             newX = bool (reg ++ sym ++ (show idx)) reg (idx == 0)
  --         newX : (indexCalculator xs)

propRegister :: Function -> String -> RP -> String -> String -> String -> String -> (Function, String)
propRegister f line infoP fromP toP lhs rhs = do
  let (x, (var, reg)) = statement line
      (vList, pList) = (variables f, registers f)
      v = fromJust $ lookupList lhs vList

      wordsOne = split (startsWithOneOf "-+ %") rhs
      tmpRHS = concat $ replaceline fromP toP wordsOne
      wordsTwo = split (startsWithOneOf "% ,") tmpRHS
      newRHS = concat $ indexCalculator wordsTwo --trace("55 \t" ++ fromP ++ " => " ++ toP ++ " :: " ++ tmpRHS)
      -- sum $ map (\a -> 1) $ filter (== x) list
  if (isRegPointer lhs)
    then do
      let p = fromJust $ lookupList_p lhs pList
          newList_p = updateList_p (setRstate newRHS p) pList []
          newList_v = updateList (setState newRHS v) vList []
      (f{registers = newList_p, variables = newList_v} , unwords[lhs, "=", newRHS]) --trace("1 (" ++ fromP ++ " -> "++ toP ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ newRHS)
    else do
      let newList_v = updateList (setState newRHS v) vList []
      (f{variables = newList_v} , unwords[lhs, "=", newRHS]) --trace("2 (" ++ fromP ++ " -> "++ toP ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ newRHS)


  -- | (isInfixOf " + " rhs || isInfixOf " - " rhs ) = do
  --   if (isRegPointer lhs)
  --     then do
  --       let rv = fromJust $ lookupList_p lhs (registers f) -- v in registerList
  --           sym = bool " + " " - " (isInfixOf " - " rhs)
  --           (pos, neg) = (sym == " + ", sym == " - ")
  --           (a, b) = bool (strSplit' " + " rhs) (strSplit' " - " rhs) neg
  --
  --           convertStr = bool (unwords $ map strip $ replace' fromP toP [b]) (unwords $ map strip $ replace' fromP toP [a]) (hasPtr fromP a)
  --           newRHS = bool (a ++ sym ++ convertStr) (convertStr ++ sym ++ b) (hasPtr fromP a)
  --           newList_p = updateList_p (setRstate newRHS rv) (registers f) []
  --           newList_v = updateList (setState newRHS v) vList []
  --
  --       trace("1 (" ++ fromP ++ " -> "++ toP ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ newRHS)(f{registers = newList_p, variables = newList_v} , unwords[lhs, "=", newRHS])--trace(" (" ++ fromP ++ " -> "++ toP ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ newRHS)
  --
  --     else do
  --       let sym = bool " + " " - " (isInfixOf " - " rhs)
  --           (pos, neg) = (sym == " + ", sym == " - ")
  --           (a, b) = bool (strSplit' " + " rhs) (strSplit' " - " rhs) neg
  --
  --           convertStr = bool (unwords $ map strip $ replace' fromP toP [b]) (unwords $ map strip $ replace' fromP toP [a]) (hasPtr fromP a)
  --           newRHS = bool (a ++ sym ++ convertStr) (convertStr ++ sym ++ b) (hasPtr fromP a)
  --           newList_v = updateList (setState newRHS v) vList []
  --       trace("2 (" ++ fromP ++ " -> "++ toP ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ newRHS)(f{variables = newList_v} , unwords[lhs, "=", newRHS]) --trace(" (" ++ fromP ++ " -> "++ toP ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ newRHS)
  --
  -- | (isInfixOf "+" rhs || isInfixOf "-" rhs) = do
  --   let sym = bool "+" "-" (isInfixOf "-" rhs)
  --       (pos, neg) = (sym == "+", sym == "-")
  --       (a, b) = bool (strSplit' "+" rhs) (strSplit' "-" rhs) neg
  --
  --   if (isNum a || isNum b)
  --     then do
  --       let n = read (bool a b (isMatchPointer fromP [a])) :: Integer
  --           idx = getIndex infoP
  --           idxSum = bool (idx + n) (idx - n) neg
  --           sym = bool "+" "" (idxSum < 0)
  --           baseStr = bool (getBase infoP++ sym) "" (getBase infoP == "")
  --           newRHS = baseStr ++ show idxSum
  --           newList = updateList (setState newRHS v) vList []
  --
  --       trace("3 (" ++ fromP ++ " -> "++ toP ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ newRHS)(f{variables = newList}, unwords[lhs, "=", newRHS]) --trace(" (" ++ fromP ++ " -> "++ toP ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ newRHS)
  --     else
  --       trace("4 (" ++ fromP ++ " -> "++ toP ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ (replace fromP toP rhs))(f, unwords [lhs, "=", replace fromP toP rhs]) --trace(" (" ++ fromP ++ " -> "++ toP ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ (replace fromP toP rhs))
  -- | otherwise = trace("5 (" ++ fromP ++ " -> "++ toP ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ (replace fromP toP rhs))(f, unwords [lhs, "=", replace fromP toP rhs]) --trace(" (" ++ fromP ++ " -> "++ toP ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ (replace fromP toP rhs))
  --   where (x, (var, reg)) = statement line
  --         (vList, pList) = (variables f, registers f)
  --         v = fromJust $ lookupList lhs vList

propagateRegister :: Function -> [String] -> String -> RP -> [String] -> ([String], Function)
propagateRegister f [] ptr rptr content = (content, f)
propagateRegister f (line : nextCont) ptr rptr content
  | (isFunction line || isBasicBlock line || isBlockLabel line) = propagateRegister f nextCont ptr rptr (content ++ [line])
  | (isInfixOf " = " line) = do
    let [lhs, rhs] = splitOn' " = " line

    if (hasPtr ptr rhs) --trace(ptr ++ ": (" ++ (bool "x" "v" (hasPtr ptr state) )++ ")\t" ++ line)
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
                propagateRegister (f{variables = newList}) nextCont ptr rptr (content ++ [lhs++ " = " ++ newRHS]) --trace("1 (" ++ ptr ++ " -> "++ rnewRHS ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ newRHS)
              "-" -> do
                let newRHS = concat[(getBase rptr), "-", show (getIndex rptr - n)]
                    newList = updateList (setState newRHS v) vList []
                propagateRegister (f{variables = newList}) nextCont ptr rptr (content ++ [lhs ++ " = " ++ newRHS]) --trace("2 (" ++ ptr ++ " -> "++ rnewRHS ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ newRHS)

              _ -> propagateRegister f nextCont ptr rptr(content ++ [lhs ++ " = " ++ (replace ptr rnewRHS rhs)]) --trace("3 (" ++ ptr ++ " -> "++ rnewRHS ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ (replace ptr rnewRHS rhs))
                --
          "conversion" -> do
            let newList = updateList (setState rnewRHS v) vList []
            propagateRegister (f{variables = newList}) nextCont ptr rptr(content ++ [lhs ++ " = " ++ rnewRHS]) --trace("4 (" ++ ptr ++ " -> "++ rnewRHS ++ ")\t" ++ line ++" -> " ++ lhs++ " = " ++ rnewRHS)
          "none" -> do
            let (f_new, new_line) = propRegister f line rptr ptr rnewRHS lhs rhs
            propagateRegister f_new nextCont ptr rptr (content ++ [new_line]) --trace("5 (" ++ ptr ++ " -> "++ rnewRHS ++ ")\t" ++ line ++" -> " ++ new_line)

          _ -> do
            if (op var == "icmp" || op var == "fcmp")
              then do
                let [a, b] = replaceline ptr rnewRHS reg
                    cmp = unwords [a, sym var, b]
                    newLine = unwords [lhs, "=", cmp]
                    newList = updateList (setState cmp v) vList []
                    -- (f_new, new_line) = propRegister f line rptr ptr rnewRHS lhs rhs
                propagateRegister f nextCont ptr rptr (content ++ [newLine])  --trace("184 (" ++ ptr ++ " -> "++ rnewRHS ++ " BUT " ++ cmp ++ ")\t" ++ line ++" -> " ++ newLine)
              else propagateRegister f nextCont ptr rptr (content ++ [line])

    else propagateRegister f nextCont ptr rptr (content ++ [line])

  | otherwise = do
    if (hasPtr ptr line) --trace(ptr ++ ": (" ++ (bool "x" "v" (hasPtr ptr line) )++ ")\t" ++ line)
      then propagateRegister f nextCont ptr rptr(content ++ [replace ptr (getRstate rptr) line]) --trace("6 (" ++ ptr ++ " -> "++ (getRstate rptr) ++ ")\t" ++ line ++" -> " ++ (replace ptr (getRstate rptr) line))
      else propagateRegister f nextCont ptr rptr(content ++ [line])

propagateVariable :: [String] -> String -> String -> [LeftVar] -> [String] -> ([String], [LeftVar])
propagateVariable [] old new vList preCont = (preCont, vList)
propagateVariable (line : nextCont) old new vList preCont
  | (isFunction line || isBasicBlock line || isBlockLabel line || isFunctionEnd line) = propagateVariable nextCont old new vList (preCont ++ [line])
  | (isInfixOf " = " line) = do --trace("\t" ++ old ++ " ("++ (bool "x" "v" (isUse old line)) ++ ")\t" ++ line ++ "\n\t\t" ++ (replace old new line))

    let [lhs, rhs] = splitOn' " = " line
    if (isUse old rhs) --trace(" (" ++ old ++ ", " ++ new ++") : " ++ (bool "x" "v" (isUse old state)) ++ " :" ++ line)
      then do
        let (x, (var, reg)) = statement line

        if (op var == "load")
          then do
            let newRHS = "[" ++ new ++ "]"
                v = fromJust $ lookupList lhs vList
                newList = updateList (setState newRHS v) vList []
            propagateVariable nextCont old new newList (preCont ++ [replace old new line]) --trace("7 (" ++ old ++ " -> "++ new ++ ")\t" ++ line ++" -> " ++ (replace old new line))

          else do
            let newRHS = replace old new rhs
                v = fromJust $ lookupList lhs vList --trace(" " ++ state ++ " -> " ++ newState)
                newList = updateList (setState newRHS v) vList []
            propagateVariable nextCont old new newList (preCont ++ [ lhs ++ " = " ++ newRHS ]) --trace("8 (" ++ old ++ " -> "++ new ++ ")\t" ++ line ++" -> " ++ lhs ++ " = " ++ newRHS)

      else propagateVariable nextCont old new vList (preCont ++ [line])

  | otherwise = do

    if (isUse old line)
      then do
        let newLine = replace old new line
            -- v' = fromJust $ lookupList old vList
            -- newList = updateList (setState newLine v') vList []
        propagateVariable nextCont old new vList (preCont ++ [newLine]) --trace("9 (" ++ old ++ " -> "++ new ++ ")\t" ++ line ++" -> " ++ newLine)

      else propagateVariable nextCont old new vList (preCont ++ [line])

propagation :: Function -> [String] -> [String] -> Function
propagation f [] newCode = f{code = newCode}
propagation f (line: nextCont) preCont
  | (isFunction line || isFunctionEnd line || isBasicBlock line || isBlockLabel line || isEntryExit line) = propagation f nextCont (preCont ++ [line])
  | (isLHS line (fname f)) = do
    let (x, (xvar, xreg)) = statement (strip line)
        (lhs, rhs) = strSplit " = " line

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
                propagation fnew new_nextCont (preCont ++ [newLine]) --trace("10 (" ++ lhs ++ " -> "++ low xvar ++ ")\t" ++ line ++" -> " ++ newLine)

              else do
                let newLine = concat[lhs, " = ", getRstate p] --trace(v ++ ": " ++ getRstate rp) c
                    (new_nextCont, fnew) = bool (nextCont, f) (propagateRegister f nextCont lhs p []) (getPermit p)
                propagation fnew new_nextCont (preCont ++ [newLine]) --trace("11 (" ++ lhs ++ " -> "++ getRstate p ++ ")\t" ++ line ++" -> " ++ newLine)

      else do
        -- VARIABLE --
        case (instrType xvar) of
          "conversion" -> do
            let (oldType, oldVar) = (ty xvar, lhs)
                (newType, newVar) = (ty1 xvar, head xreg)
                (new_nextCont, newList) = propagateVariable nextCont oldVar newVar vList []
            propagation (f { variables = newList}) new_nextCont (preCont ++ [lhs ++ " = " ++ head xreg]) --trace("12 (" ++ oldVar ++ " -> "++ newVar ++ ")\t" ++ line ++" -> " ++ lhs ++ " = " ++ head xreg)

          "colon" -> do
            let oldVar = lhs
                newVar = low xvar
                (new_nextCont, newList) = propagateVariable nextCont oldVar newVar vList []
            propagation (f { variables = newList}) new_nextCont (preCont ++ [lhs ++ " = " ++ newVar]) --trace("13 (" ++ oldVar ++ " -> "++ newVar ++ ")\t" ++ line ++" -> " ++ lhs ++ " = " ++ newVar)

          "binaryOP" -> do
            let new_nextCont = replace' lhs (getState v) nextCont
            propagation f new_nextCont (preCont ++ [line])

          _-> case (op xvar) of

            "load" -> do
                let newState = replace lhs (head xreg) (snd $  strSplit' "=" line)--"[" ++ head xreg ++ "]"
                    v = fromJust $ lookupList lhs vList
                    newList = updateList (setState newState v) vList []
                    newLine = unwords [lhs, "=", newState]
                propagation (f { variables = newList}) nextCont (preCont ++ [newLine]) --trace("14 (" ++ lhs ++ " -> "++ newState ++ ")\t" ++ line ++" -> " ++ newLine)
              -- else propagation fname nextCont vList pList (preCont ++ [line])

            "equal" -> propagation f (replace' lhs rhs nextCont) (preCont ++ [line]) --trace("15 (" ++ lhs ++ " -> "++ rhs ++ ")\t" ++ line)

            _ -> do
              if(op xvar == "icmp" || op xvar == "fcmp" )
                then do
                  let [a, b] = xreg
                      cmp = unwords [a, sym xvar, b]
                      newLine = unwords [lhs, "=", cmp]
                      newList = updateList (setState cmp v) vList []
                      -- (newContent, newList) = propagateVariable nextCont lhs cmp vList []
                  propagation (f{variables = newList}) nextCont (preCont ++ [newLine]) --trace("16 (" ++ lhs ++ " -> "++ cmp ++ ")\t" ++ line ++ " -> " ++ newLine)
                  -- trace("16 (" ++ lhs ++ " -> "++ cmp ++ ")\t" ++ line ++ " -> " ++ newLine)propagation (f{variables = newList}) newContent (preCont ++ [newLine])
                else propagation f nextCont (preCont ++ [line])

  | otherwise = propagation f nextCont (preCont ++ [line])
