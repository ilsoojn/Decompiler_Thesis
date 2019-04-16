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
        val = value s
        loc = at s
    case (base == loc, isInitialPointer val) of
      (True, True) -> (base, idx)
      (True, False) ->
        if (isNum val)
          then ("", strToInt val + idx)
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
              ("", idx + off)
            1 -> do -- |rp| = 1 & |rn| = 1
              let n = bool 0 (strToInt $ unwords rn) (isNum (unwords rn))
                  off = bool (idx + n) (idx - n) isSub
              baseIndex (unwords rp) off pList vList content
            _ -> do -- |rp| = 2 & |rn| = 0
              let (rBase1, rIdx1) = baseIndex (head rp) 0 pList vList content
                  (rBase2, rIdx2) = baseIndex (last rp) 0 pList vList content
                  rBase = bool rBase1 rBase2 (isRegPointer rBase2)
                  rIdx = bool (rIdx1 + rIdx2) (rIdx1 - rIdx2) isSub
              (rBase, idx + rIdx)
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

pointerInfo ptr state pList vList content
  | (isInfixOf "_ptr" ptr) = (RP ptr ptr 0 state False)
  | (instrType var == "binary") = do
    let isSub = (op var == "sub")
        [a, b] = reg
    case (map isNum reg) of
      [True, True] -> do
        let (x, y) = (strToInt a, strToInt b)
            z = bool y (0-y) isSub
            newState = show (x + y)
        (RP ptr "" (x + y) newState True)

      [False, False] -> do
        let (rbase1, rindex1) = trace(ptr ++ " ("++head reg ++ ", " ++ last reg++")") baseIndex (head reg) 0 pList vList content
            (rbase2, rindex2) = baseIndex (last reg) 0 pList vList content

        if (isRegPointer rbase1 && isRegPointer rbase2)
          then do
            let sym = trace("> " ++ ptr ++ " ("++head reg ++ ", " ++ last reg++")") bool "+" "-" (isSub)
            (RP ptr "" 0 (unwords [a, sym, b]) True)
          else do
            let rbase = bool rbase1 rbase2 (isRegPointer rbase2)
                rindex = bool (rindex1 + rindex2) (rindex1 - rindex2) isSub
                sym = bool "+" "" (rindex < 0)
                newState = bool (bool (rbase ++ sym ++  show rindex) (show rindex) (null rbase)) rbase (rindex == 0)
            (RP ptr rbase rindex newState True)

      _ -> do
        let idx = strToInt (bool a b (isNum b))
            rdix = bool (idx) (0-idx) isSub
            (rbase, rindex) = baseIndex (bool b a (isNum b)) rdix pList vList content
            sym = bool "+" "" (rindex < 0)
            newState = bool (bool (rbase ++ sym ++  show rindex) (show rindex) (null rbase)) rbase (rindex == 0)
        (RP ptr rbase rindex newState True)

  --
  | (instrType var == "conversion") = do
    let (rbase, rindex) = baseIndex (head reg) 0 pList vList content
        sym = bool "+" "" (rindex < 0)
        newState = bool (bool (rbase ++ sym ++  show rindex) (show rindex) (null rbase)) rbase (rindex == 0)
        -- newState = newPtr --replaceLine (head reg) newPtr str_var state
    (RP ptr rbase rindex newState True)

  | (op var == "load") = do
    let (rbase, rindex) = baseIndex (head reg) 0 pList vList content
        sym = bool  "+" "" (rindex < 0)
        tmp = bool (rbase ++ sym ++ show rindex) (show rindex) (null rbase) -- bool ("[" ++ rbase ++ sym ++  show rindex ++ "]") (show rindex) (null rbase)
        newState = bool tmp rbase (rindex == 0)
    (RP ptr rbase rindex newState True)
  --
  | (instrType var == "bitwise") = do
    let newState = join " " [op var, ty var, head reg ++ ",", last reg]
    if (head reg == last reg)
      then (RP ptr "" 0 newState False)
      else (RP ptr ptr 0 newState False)

  | (op var == "alloca") = (RP ptr ptr 0 state False)

  | otherwise = (RP ptr ptr 0 state False)
    where (px, (var, reg)) = statement state
