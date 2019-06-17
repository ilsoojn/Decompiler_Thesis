module Idioms where

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

import RegexFunction
import Lists
import OtherFunction
import StatementInstr
import StatementParse

{-
    INPUT
      %v1 = And/Sub int_type %register_ptr, n

    OUTPUT
      if nextline is %v2 = inttoptr int_type %register_ptr to ptr_type
        then %v2 = %ptr+-n (no nextline)
        else %v1 = %ptr+-n          (keep nextline)
-}
binaryOP :: String -> String -> String -> [LeftVar] -> ([String], String, [LeftVar])
binaryOP v new_state nextline vList
  | ((not.isNothing) nv && (instrType nvar == "conversion") && (op nvar == "inttoptr")) = do
    let newLine = concat[(fromJust nv), " = ", new_state]
        n = fromJust $ lookupList (fromJust nv) vList
        n' = n{ instruction="binaryOP", state=new_state }
        newList = updateList n' (removeVariable c vList []) []
    ([newLine], "", newList)

  | otherwise = do
    let newLine = concat[v, " = ", new_state]
        c' = c{ instruction="binaryOP", state=new_state }
        newList = updateList c' vList []
    ([newLine], nextline, newList)

    where (nv, (nvar, nreg)) = statement nextline
          c = fromJust $ lookupList v vList

{-
  INPUT
    %v1 = zext i128 %a to i256      ( v1 <- 00000.....0 : a_128 )
    %v2 = and i256 %b, -340282366920938463463374607431768211456 ( v2 <- b_128 : 00000...0 )
    %v = or i256 %v1, %v2           (v <- v2 : v1 <- b_128 : a_128)
  OUTPUT
    %v = %b_bitsize : %a_bitsize
-}
bitwiseOP :: String -> String -> String -> [String] -> [LeftVar] -> (String, [String], String, [LeftVar])
bitwiseOP pre curr next content vList
  | ((not.isNothing) pv && (not.isNothing) nv) = do

    let p = fromJust $ lookupList (fromJust pv) vList
        c = fromJust $ lookupList (fromJust cv) vList
        n = fromJust $ lookupList (fromJust nv) vList

        useP = length (filter (not.null) $ findUse (fromJust pv) content)
        useC = length (filter (not.null) $ findUse (fromJust cv) content)
        useN = length (filter (not.null) $ findUse (fromJust nv) content)

        isUse_one = (useP == 1) && (useC == 1)
        isIdiomInstr = (getInstr p == "zext" || getInstr p == "trunc") && (getInstr n == "or") -- <can't get getInstr p becuase it is a
        isIdiom = (elem (fromJust pv) nreg) && (elem (fromJust cv) nreg)

    if (isUse_one && isIdiomInstr && isIdiom)
      then do
        let newState = (head $ values cvar) ++ " : " ++ (value pvar)
            newVariable = n{ instruction="bitwiseOP", state=newState }
            newLine = concat [variable n, " = " , newState]
            new_vList = updateList newVariable (removeVariable c (removeVariable p vList []) []) []
        ("", [newLine], "", new_vList)

      else (pre, [curr], next, vList)

  | otherwise = (pre, [curr], next, vList)
    where (pv, (pvar, preg)) = statement pre
          (cv, (cvar, creg)) = statement curr
          (nv, (nvar, nreg)) = statement next

{-
  INPUT: content preContent Function count_1 count_2
-}
detectIdiom :: [String] -> [String] -> Function -> Integer -> Integer -> Function
detectIdiom [] txt f bin bit = trace("\nDetected Idioms\n - Idiom 1 (binaryOP): " ++ show bin ++ "\n - Idiom 2 (bitwiseOP): "++ show bit) (f {code = txt})
detectIdiom (line: content) oldTxt f binaryN bitwiseN
  | (isLHS line (fname f)) = do

    let (v, (xState, reg)) = statement (strip line)
        iType = instrType xState

    case iType of
      "binary" -> do
        let instr = op xState
            vList = variables f

        if (or $ map isNum reg)
          then do
            let ptr = bool (last reg) (head reg) (isNum $ last reg)
                idx = bool (read (head reg) :: Integer) (read (last reg) :: Integer) (isNum $ last reg)

            if ((instr == "add" || instr == "sub") && hasRegPointer (last $ splitOn " = " line))
              then do
                let sym = bool (bool "+" "-" (idx < 0)) (bool "-" "+" (idx < 0)) (instr /= "add")
                    new_state = concat [ptr, sym, show (abs idx)]

                    (cline, nline, newList) = binaryOP (fromJust v) new_state (head content) vList
                    newContent = filter (not.null) $ nline:(tail content)

                trace(fromJust v ++ " 119")detectIdiom newContent (oldTxt ++ cline) (f {variables = newList}) (binaryN + 1) bitwiseN --trace("binary: " ++ (fromJust v))

              else detectIdiom content (oldTxt ++ [line]) f binaryN bitwiseN

          else if ((instr == "add" || instr == "sub") && hasRegPointer (last $ splitOn " = " line))
            then do
                let sym = bool " + " " - " (instr /= "add")
                    new_state = concat [head reg, sym, last reg]
                    (cline, nline, newList) = binaryOP (fromJust v) new_state (head content) vList
                    newContent = filter (not.null) $ nline:(tail content)
                detectIdiom newContent (oldTxt ++ cline) (f {variables = newList}) (binaryN + 1) bitwiseN --trace("binary: " ++ (fromJust v))

              else detectIdiom content (oldTxt ++ [line]) f binaryN bitwiseN
        {-
        Idiom 2:
          %v1 = zext i128 %a to i256      ( v1 <- 00000.....0 : a_128 )
          %v2 = and i256 %b, -340282366920938463463374607431768211456 ( v2 <- b_128 : 00000...0 )
          %v = or i256 %v1, %v2           (v <- v2 : v1 <- b_128 : a_128)
      -}
      "bitwise" -> do
        let instr = op xState
            [a, b] = reg
            vList = variables f

        if ((instr == "and") && (isNum b || isNum' b) && (isInt $ strToFloat b) && (is0s $ strToInt b))
          then do
            let (pline, cline, nline, newList) = bitwiseOP (last oldTxt) (line) (head content) (line : content) vList
                newContent = filter (not.null) $ nline:(tail content)
                newPre = filter (not.null) $ init oldTxt ++ [pline] ++ cline

            detectIdiom newContent newPre (f {variables = newList}) binaryN (bitwiseN + 1) --trace("bitwise: " ++ (fromJust v))

          else detectIdiom content (oldTxt ++ [line]) f binaryN bitwiseN

      _ -> detectIdiom content (oldTxt ++ [line]) f binaryN bitwiseN

  | otherwise = detectIdiom content (oldTxt ++ [line]) f binaryN bitwiseN
