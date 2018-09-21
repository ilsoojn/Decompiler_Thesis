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

import IsGetSet
import Lists
import OtherFunction
import StatementInstr
import StatementParse

-- isLHS line fname = (not.isFunction) line && (not.isBasicBlock) line && (not.isBlockLabel) line && (not.null) fname && (isInfixOf " = " line)

{-
    INPUT

      %v1 = And/Sub int_type %register_ptr, n

    OUTPUT

      if nextline is %v2 = inttoptr int_type %register_ptr to ptr_type
        then %v2 = %ptr+-n (no nextline)
        else %v1 = %ptr+-n          (keep nextline)
-}
idiom1 :: String -> String -> String -> [LeftVar] -> ([String], [LeftVar])
idiom1 v new_state nextline vList
  | ((not.isNothing) nv && (op nvar) == "inttoptr") = do
    let newLine = concat[(fromJust nv), " = ", new_state]
        n = fromJust $ lookupList (fromJust nv) vList
        n' = n{ instruction="", state=new_state }
        newList = updateList n (removeVariable c vList []) []
    ([newLine, ""], newList)

  | otherwise = do
    let newLine = concat[v, " = ", new_state]
        c' = c{ instruction="", state=new_state }
        newList = updateList c vList []
    ([newLine, nextline], newList)

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
idiom2 :: String -> String -> String -> [String] -> [LeftVar] -> ([String], [LeftVar])
idiom2 pre curr next content vList
  | ((not.isNothing) pv && (not.isNothing) nv) = do

    let p = fromJust $ lookupList (fromJust pv) vList
        c = fromJust $ lookupList (fromJust cv) vList
        n = fromJust $ lookupList (fromJust nv) vList

        useP = length (filter (not.null) $ findUse (fromJust pv) content [])
        useC = length (filter (not.null) $ findUse (fromJust cv) content [])
        useN = length (filter (not.null) $ findUse (fromJust nv) content [])

        isUse_one = (useP == 1) && (useC == 1)
        isIdiomInstr = (getInstr p == "zext" || getInstr p == "trunc") && (getInstr n == "or") -- <can't get getInstr p becuase it is a
        isIdiom = (elem (fromJust pv) nreg) && (elem (fromJust cv) nreg)

    if (isUse_one && isIdiomInstr && isIdiom)
      then do
        let newState = (head $ values cvar) ++ " : " ++ (value pvar)
            newVariable = n{ instruction="", state=newState }
            newLine = concat [variable n, " = " , newState]
            new_vList = updateList newVariable (removeVariable c (removeVariable p vList []) []) []
        (["", newLine, ""], new_vList)

      else ([pre, curr, next], vList)

  | otherwise = ([pre, curr, next], vList)
    where (pv, (pvar, preg)) = statement pre
          (cv, (cvar, creg)) = statement curr
          (nv, (nvar, nreg)) = statement next

detectIdiom :: [String] -> String -> [String] -> [LeftVar] -> ([String], [LeftVar])
detectIdiom [] fn pre vList = (pre, vList)
detectIdiom (line: next) fn pre vList
  | (next == [""]) = (pre, vList)
  | (isFunction line) = detectIdiom next (getFunctionName line) (pre ++ [line]) vList
  | (isLHS line fn) = do

    let (v, (rhs, reg)) = statement (strip line)
        v_type = variableType rhs
    {-
      Idiom 1:
        case 1: %v1 = And/Sub int_type %register_ptr, n
        case 2: %v1 = And/Sub int_type %register_ptr, n
                %v2 = inttoptr int_type %register_ptr to ptr_type
    -}
    if (isBinary rhs)
      then do
        let instr = op rhs
            ptr = head reg
            idx = read (last reg) :: Integer

        if (instr == "add" || instr == "sub" && isUsePointer line)
          then do
            let sym = bool (bool " +" " -" (idx < 0)) (bool " -" " +" (idx < 0)) (instr /= "add")
                new_state = concat [ptr, sym, show (abs idx)]

                ([cline, nline], newList) = idiom1 (fromJust v) new_state (head next) vList
                newNext = filter (not.null) $ nline:(tail next)
            detectIdiom newNext fn (pre ++ [cline]) newList
          else detectIdiom next fn (pre ++ [line]) vList
        {-
        Idiom 2:
          %v1 = zext i128 %a to i256      ( v1 <- 00000.....0 : a_128 )
          %v2 = and i256 %b, -340282366920938463463374607431768211456 ( v2 <- b_128 : 00000...0 )
          %v = or i256 %v1, %v2           (v <- v2 : v1 <- b_128 : a_128)
      -}
      else if (isBitwise rhs)
        then do
          let instr = op rhs
              [a, b] = reg

          if ((instr == "and") && (isNum b || isNum' b) && (isInt $ strToFloat b) && (is0s $ strToInt b))
            then do
              let ([pline, cline, nline], newList) = idiom2 (last pre) (line) (head next) (line : next) vList
                  newNext = filter (not.null) $ nline:(tail next)
                  newPre = filter (not.null) $ init pre ++ [pline, cline]
              detectIdiom newNext fn newPre newList

            else detectIdiom next fn (pre ++ [line]) vList
        else detectIdiom next fn (pre ++ [line]) vList

  | otherwise = detectIdiom next fn (pre ++ [line]) vList
