module RegisterPointer where

import Data.Bool
import Data.Char
import Data.DeriveTH
import Data.List
import Data.List.Split
import Data.List.Utils hiding (split)
import Data.Maybe
import Data.String.Utils hiding (split)
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

baseIndex :: String -> Integer -> [RP] -> [LeftVar] -> [String] -> (String, Integer)
baseIndex base idx p v [] = (base, idx)
baseIndex base idx pList vList (line : content)
  | (isBasicBlock line || isBlockLabel line || isEntryExit line) = baseIndex base idx pList vList content
  | (isInfixOf base line && isPrefixOf "store" line) = do
    -- STORE type v, type* BASE
    let s = storeStatement line
        val = strip (value s)
        loc = strip (at s)
    case (base == loc, isInitialPointer val) of
      (True, True) -> (base, idx)
      (True, False) ->
        if (isNum val)
          then ("", strToInt val + idx) --trace("- " ++ val ++ " + " ++ show idx)
          else baseIndex val idx pList vList content
      (False, _) -> baseIndex base idx pList vList content

  | (isInfixOf base line && isInfixOf " = " line) = do
    let (lhs, (x, r)) = statement line
        v = fromJust lhs
        operator = op x

    if (base == v)
      then if ((instrType x == "binary") && (operator == "add" || operator == "sub"))
        then do
          let rp = filter (isPrefixOf str_var) r        -- Variable
              rn = filter (not . isPrefixOf str_var) r  -- Constant
              isSub = (operator == "sub")
          case (length rp) of
            0 -> do -- |rp| = 0 & |rn| = 2
              let [off1, off2] = map strToInt rn
                  off = bool (off1 + off2) (off1 - off2) isSub
              ("", idx + off) --trace("- " ++ show idx ++ " " ++ show off)
            1 -> do -- |rp| = 1 & |rn| = 1
              let n = bool 0 (strToInt $ unwords rn) (isNum (unwords rn))
                  off = bool (idx + n) (idx - n) isSub
              baseIndex (unwords rp) off pList vList content
            _ -> do -- |rp| = 2 & |rn| = 0
              let (rBase1, rIdx1) = baseIndex (head rp) 0 pList vList content
                  (rBase2, rIdx2) = baseIndex (last rp) 0 pList vList content
              if (rBase1 /= rBase2)
                then do
                  let rBase = bool rBase1 rBase2 (isRegPointer rBase2)
                      rIdx = bool (rIdx1 + rIdx2) (rIdx1 - rIdx2) isSub --trace(bool (show rIdx1 ++ "+" ++ show rIdx2) (show rIdx1 ++ "-" ++ show rIdx2) isSub)
                  (rBase, idx + rIdx) --trace("return " ++ rBase ++ " " ++ show idx ++ " " ++ show rIdx)
                else
                  (head rp ++ " " ++ (sym x) ++ " " ++ last rp, 0)

              -- bool (rBase1, idx + rIdx1 + rIdx2) (rBase2, idx + rIdx1 + rIdx2) (isRegPointer rBase2)

        else
          case (instrType x, operator) of
            ("bitwise", "xor") -> (base, idx)--do
            --   let [a, b] = values x
            --   if (a == b)
            --     then ("0", idx)
            --     else (base, idx)
            ("conversion", _) -> baseIndex (unwords r) idx pList vList content
            ("memory", "load") -> baseIndex (unwords r) idx pList vList content
            _ -> (base, idx)
      else baseIndex base idx pList vList content

  | otherwise = baseIndex base idx pList vList content

pointerInfo :: String -> String -> [RP] -> [LeftVar] -> [String] -> RP
pointerInfo lhs rhs pList vList content
  | (isInfixOf "_ptr" lhs) = (RP lhs lhs 0 rhs False) --trace("LHS: " ++ lhs)
  | (instrType var == "binary") = do
    let [a, b] = reg
    case (map isNum reg) of

      [True, True] -> do
        let [x, y] = map fromIntegral [strToInt a, strToInt b]
        case (sym var) of
          "+" -> (RP lhs "" (x + y) (show (x + y)) True)
          "-" -> (RP lhs "" (x - y) (show (x - y)) True)
          "*" -> (RP lhs "" (x * y) (show (x * y)) True)
          "/" -> (RP lhs "" (div x  y) (show (div x y)) True)
          _ -> (RP lhs "" (rem x y) (show (rem x y)) True)


      [False, False] -> do
        let (rbase1, rindex1) = baseIndex a 0 pList vList content
            (rbase2, rindex2) = baseIndex b 0 pList vList content

        case (isRegPointer rbase1 && isRegPointer rbase2) of
          True -> (RP lhs lhs 0 (unwords [a, (sym var), b]) True) -- both operands are variables
          _ -> do
            let rbase = bool rbase1 rbase2 (isRegPointer rbase2)
                rindex = bool rindex1 rindex2 (isRegPointer rbase2)
                v = bool b a (isRegPointer rbase2)
                rindexStr = bool (show rindex) "" (rindex == 0)
                rsym = bool "+" "" (rindex < 0)
            (RP lhs rbase rindex (rbase ++ rsym ++ rindexStr ++ " " ++ sym var ++ " " ++ v) True)
            -- case (sym var) of
            --   "+" -> (RP lhs rbase rindex (rbase ++ rsym ++ rindexStr ++ " + " ++ v) True)
            --   "-" -> (RP lhs rbase rindex (rbase ++ rsym ++ rindexStr ++ " - " ++ v) True)
            --   "*" -> (RP lhs rbase rindex (rbase ++ rsym ++ rindexStr ++ " * " ++ v) True)
            --   "/" -> (RP lhs rbase rindex (rbase ++ rsym ++ rindexStr ++ " / " ++ v) True)
            --   _ -> (RP lhs rbase rindex (rbase ++ rsym ++ rindexStr ++ " % " ++ v) True)

      _ -> do
        let idx = strToInt (bool a b (isNum b))
            (rbase, rindex) = baseIndex (bool b a (isNum b)) 0 pList vList content

        case (sym var) of
          "+" -> do
            let s = rindex + idx
                tmp = bool ("+" ++ show s) (show s) (s < 0)
                sumStr = bool tmp (show s) (null rbase)
            (RP lhs rbase (rindex + idx) (rbase ++ sumStr) True)
          "-" -> do
            let s = rindex - idx
                tmp = bool ("+" ++ show s) (show s) (s < 0)
                subStr = bool tmp (show s) (null rbase)
            (RP lhs rbase (rindex - idx) (rbase ++ subStr) True)
          "*" -> (RP lhs rbase rindex (rbase ++ " * " ++ show idx) True)
          "/" -> (RP lhs rbase rindex (rbase ++ " / " ++ show idx) True)
          _ -> (RP lhs rbase rindex (rbase ++ " % " ++ show idx) True)

  --
  | (instrType var == "conversion") = do
    let (rbase, rindex) = baseIndex (head reg) 0 pList vList content
        sym = bool "+" "" (rindex < 0)
        newState = bool (bool (rbase ++ sym ++  show rindex) (show rindex) (null rbase)) rbase (rindex == 0)
        -- newState = newPtr --replaceLine (head reg) newPtr str_var state
    (RP lhs rbase rindex newState True)

  | (op var == "load") = do
    let (rbase, rindex) = baseIndex (head reg) 0 pList vList content
        sym = bool  "+" "" (rindex < 0)
        tmp = bool (rbase ++ sym ++ show rindex) (show rindex) (null rbase) -- bool ("[" ++ rbase ++ sym ++  show rindex ++ "]") (show rindex) (null rbase)
        newState = bool tmp rbase (rindex == 0)
    (RP lhs rbase rindex newState True)
  --
  | (instrType var == "bitwise") = do
    let newState = join " " [op var, ty var, head reg ++ ",", last reg]
    if (head reg == last reg)
      then (RP lhs "" 0 newState False)
      else (RP lhs lhs 0 newState False)

  | (op var == "alloca") = (RP lhs lhs 0 rhs False)

  | otherwise = (RP lhs lhs 0 rhs False)
    where (px, (var, reg)) = statement rhs
