module NamingPrecision where

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

variableName :: [String] -> [(String, String)] -> [LeftVar] -> [String] -> Integer -> ( [String], ([LeftVar], [(String, String)]) )
variableName [] nameList vList content num = trace("Detected Variables: " ++ show num) (content, (vList, nameList))
variableName (line : next) nameList vList content num
  | (isFunction line || isFunctionEnd line || isBlockLabel line || isBasicBlock line || isEntryExit line) = variableName next nameList vList (content ++ [line]) num
  | (isPrefixOf "store" line && (not $ or $ map (`elem` reg_base) (words line)) && (not $ hasInitialPointer line)) = do
    let s = storeStatement line
        (vtype, v, addr) = (ty s, value s, at s)
        str = lookup addr nameList
        vname = bool (fromJust str) (str_var ++ (names !! length nameList)) (isNothing str)
        pair = (addr, vname) --trace("(" ++ addr ++ ", " ++ vname ++ ")")

        tmpSz = filter (isDigit) vtype
        tmp1 = bool "10" "16" (vtype == "fp128" || vtype == "ppc_fp128") -- x86_fp80
        tmp2 = bool tmp1 "8" (vtype == "double")
        tmp3 = bool tmp2 "4" (vtype == "float")
        tmp4 = bool tmp3 "2" (vtype == "half")
        align_sz = bool (show $ round $ fromInteger(strToInt tmpSz)/8) tmp4 (null tmpSz)

        -- Create an allocating statement for the variable
        -- align_sz = bool (show $ round $ fromInteger(strToInt tmpSz)/8) ("8") (vtype == "double")
        state = "alloca " ++ vtype ++ ", align " ++ align_sz
        v' = LeftVar vname vtype "alloca" state
        newState = vname ++ " = " ++ state

    if (isNothing str)
      then variableName next (pair : nameList) (v' : vList)  ((head content : newState : tail content) ++ [line]) (num + 1)
      else variableName next nameList vList (content ++ [line]) num

  | otherwise = variableName next nameList vList (content ++ [line]) num

propagateName :: [String] -> [(String, String)] -> [String]
propagateName content [] = content
propagateName content ((location, str) : nameList) = propagateName (replace' location str content) nameList --trace(location ++ " :: " ++ str)

functionName :: [String] -> [(String, String)] -> [String]
functionName content [] = content
functionName content ((addr,fn) : asmTable) = do
  let address = str_fn ++ addr
      name = '@' : fn
  functionName (map (replace address name) content) asmTable

precisionConversion :: [String] -> Int -> String -> [String] -> [String]
precisionConversion [] _ _ content = content
precisionConversion (line : next) addr val preCont
  | (isInfixOf " = " line) = do
    let [v, state] = splitOn " = " line
        (lhs, (x, r)) = statement line
    --if (isEqualState state && isDoubleRHS state)
    if ((op x == "load") && isNum(ptr x)) -- && "double" == (ty x)
      then do
        let b1 = bool 80 128 (ty x == "ppc_fp128" || ty x == "fp128")
            b2 = bool b1 64 (ty x == "double")
            b3 = bool b2 32 (ty x == "float")
            bit = bool b3 16 (ty x == "half")
            address = fromInteger $ strToInt (ptr x)
            value = getData bit addr val address
            double = show $ strHexToDouble bit value
            newLine = concat[v, " = ", double]
            new_next = replace' v double next
        precisionConversion new_next addr val (preCont ++ [newLine])

      else precisionConversion next addr val (preCont ++ [line])
  | otherwise = precisionConversion next addr val (preCont ++ [line])
