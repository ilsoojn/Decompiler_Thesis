module ControlFlow where

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

nameConversion content [] = content
nameConversion content ((location, str) : nameList) = trace(location ++ " :: " ++ str) nameConversion (replace' location str content) nameList

variableName [] nameList vList content = (content, (vList, nameList))
variableName (line : next) nameList vList content
  | (isFunction line || isFunctionEnd line || isBlockLabel line || isBasicBlock line || isEntryExit line) = variableName next nameList vList (content ++ [line])
  | (isPrefixOf "store" line) = do
    let s = storeStatement line
        (vtype, v, addr) = (ty s, value s, at s)
        str = lookup addr nameList
        vname = bool (fromJust str) (str_var ++ (names !! length nameList)) (isNothing str)
        pair = (addr, vname) --trace("(" ++ addr ++ ", " ++ vname ++ ")")

        tmpSz = dropWhile (isLetter) vtype
        align_sz = bool (show $ round $ fromInteger(strToInt tmpSz)/8) ("8") (vtype == "double")
        state = "alloca " ++ vtype ++ ", align " ++ align_sz
        v' = LeftVar vname vtype "alloca" state
        newState = vname ++ " = " ++ state

    if (isNothing str)
      then variableName next (pair : nameList) (v' : vList)  ((head content : newState : tail content) ++ [line])
      else variableName next nameList vList (content ++ [line])

  | otherwise = variableName next nameList vList (content ++ [line])

splitBasicBlock :: [String] -> String -> [String] -> [String]  -> [BasicBlock] -> [BasicBlock]
splitBasicBlock [] _ _ _ blockList = blockList
splitBasicblock (line : next) label preds content list
  | (isFunctionEnd line) = (list ++ [BasicBlock label preds content])
  | (isFunction line) = splitBasicBlock next label preds [] list
  | (isBasicBlock line || isBlockLabel line || isEntryExit line) = do

      let eLabel = (bool "entry" "exit" (isPrefixOf "exit" line))
          blockLabel = bool (getBlockName line) eLabel (isEntryExit line)
          predsList = getBlockPreds line
      if (null label || null content)
        then splitBasicblock next blockLabel predsList [] list
        else splitBasicblock next blockLabel predsList [] (list ++ [BasicBlock label preds content])

  | otherwise = splitBasicblock next label preds (content ++ [line]) list
  --do
    -- if (isInfixOf "icmp" line || isInfixOf "fcmp" line)
    --   then do
    --     let (c, (x, r)) = statement line
    --         symbol = sym x
    --         [a, b] = values x
    --         newLine = unwords["(", a, symbol, b, ")"]

-- memoryType op lhs var line =
--   | (op == "alloca") = unwords[ty var, fromJust lhs]
--     -- case (ty var) of
--     --   "i16" -> "short " ++ fromJust lhs
--     --   "i32" -> "int " ++ fromJust lhs -- long
--     --   "i64" -> "long long " ++ fromJust lhs
--     --   "i16" -> "short " ++ fromJust lhs
--     --
--   -- | (op == "load")
--   | (op == "store") = (at var) ++ " = " ++ (value var)
--   | (op == "fence")
--   | (op == "cmpxchg") = do
--       let x =
--   | (op == "atomicrmw")
--   | otherwise = --getelementptr"
--
--
-- statementConversion [] _ content = content
-- statementConversion (line : next) vList content =
--   -- | (isBlockLabel line || isBasicBlock line) =
--   | otherwise = do
--     let (lhs, (x, r)) = statement line
--
--     case (instrType x) of
--
--       "binary" -> do--["add", "fadd", "sub", "fsub", "mul", "fmul", "udiv", "sdiv", "fdiv", "urem", "srem", "frem"]
--         let [a: b: etc] = values x
--             sndSign = isInfixOf "-" b
--             b' = bool b ("("++ b ++")") (sndSign)
--             v = fromJust lhs
--             new = unwords[v, "=", a, sym x, b']
--         statementConversion next vList (content ++ [new])
--
--       "bitwise" -> -- ["shl", "lshr", "ashr", "and", "or", "xor"]
--         let [a : b : etc] = values x
--             new = unwords[fromJust lhs, "=", a, sym x, b]
--         statementConversion next vList (content ++ [new])
--       -- "conversion" ->
--       "memroy" -> -- ["alloca", "load", "store", "fence", "cmpxchg", "atomicrmw", "getelementptr"]
--       "terminator" -> -- ["ret", "br", "switch", "indirectbr", "invoke", "resume", "catchswitch", "catchret", "cleanupret", "unreachable"]
--       "vector" -> --["extractelement", "insertelement", "shufflevector"]
--       "aggregate"-> --["extractvalue", "insertvalue"]
--       "other" -> -- ["icmp", "fcmp", "phi", "select", "call", "va_arg", "landingpad", "catchpad", "cleanuppad"]
--
--       _ -> -- COLON / NONE
