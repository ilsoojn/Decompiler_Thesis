module RegisterPointer2 where

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

baseIndex :: String -> Integer -> [RP] -> [LeftVar] -> String -> (String, Integer)
baseIndex base idx pList vList (line : content)
  | (isBasicBlock line || isBlockLabel line || isEntryExit line) = baseIndex base idx pList vList content
  | (isInfixOf base line && isPrefixOf "store" line) = do
    -- STORE type v, type* BASE
    let s = storeStatement line
        val = value s
        loc = at s
    case (base == loc, isInitialPointer val) of
      (Ture, True) -> (base idx)
      (True, False) -> baseIndex val idx pList vList content
      (False, _ ) -> baseIndex base idx pList vList content

  | (isInfixOf base line && isInfixOf " = " line) = do
    let (lhs, (x, r)) = statement line
        v = fromJust lhs
        operator = op x

    if (base == v)
      then if (isBinary x && (operatr == "and" || operator == "sub"))
        then do
          let rp = filter (isPrefixOf str_var) r        -- Variable
              rn = filter (not . isPrefixOf str_var) r  -- Constant

          case (length rp) of
            0 -> do -- |rp| = 0 & |rn| = 2
              let off1 = read (filter isDigit $ head rp) :: Integer
                  off2 = read (filter isDigit $ last rp) :: Integer
              (base, idx + off1 + off2)
            1 -> do -- |rp| = 1 & |rn| = 1
              let n = bool 0 (strToInt $ unwords rn) (isNum (unwords rn))
              baseIndex (unwords rp) (idx + n) pList vList content
            _ -> do -- |rp| = 2 & |rn| = 0
              let (rBase1, rIdx1) = baseIndex (head rp) 0 pList vList
                  (rBase2, rIdx2) = baseIndex (last rp) 0 pList vList
              bool (rBase1, idx + rIdx1 + rIdx2) (rBase2, idx + rIdx1 + rIdx2) (isRegPointer rBase2)

        else
          case (instrType op, operator) of
            ("bitwise", "xor") -> do
              let [a, b] = values x
              if (a == b)
                then ("", idx)
                else (base, idx)
            ("conversion", _) -> baseIndex (unwords r) idx pList vList content
            ("memroy", _) -> baseIndex (unwords r) idx pList vList content
            -- ("other", "select") -> do
            --   let [a, b] = values x
            --
            --
            --   selty::String, cond::String, ty::String, values::[String]

            _ -> (base, idx)
      else baseIndex base idx pList vList content


  | otherwise = baseIndex base idx pList vList content

--
-- | (elem base (reg_32 ++ reg_64 ++ reg_ip ++ reg_other)) = (base, idx)
-- | (isRegPointer base && not (isInfixOf "_ptr" base || isInfixOf "_init" base)) = do
--   let p = lookupList_p base pList
--   if (isNothing p || not (getPermit $ fromJust p))
--     then (base, idx)
--     else baseIndex (getBase $ fromJust p) (idx + (getIndex $ fromJust p)) pList vList
--
-- | (isPrefixOf str_var base) = do
--   let v = lookupList base vList
--   if (isNothing v)
--     then (base, idx)
--     else do
--       let state = getState (fromJust v)
--           itype = getInstructionType (getInstr $ fromJust v)
--           (x, (var, reg)) = statement state
--
--       case itype of
--         "binary" -> do
--           let rp = filter (isPrefixOf str_var) reg
--           let rn = filter (not . isPrefixOf str_var) reg
--           if (null rp)
--             then do
--               let off1 = read (filter isDigit $ head rp) :: Integer
--                   off2 = read (filter isDigit $ last rp) :: Integer
--
--               (base, idx + off1 + off2)
--
--             else if (length rp == 1 && isRegPointer (unwords rp) && isNum (unwords rn))
--               then baseIndex (unwords rp) (idx + strToInt (unwords rn)) pList vList
--
--               else (base, idx) --do
--                 -- let (rBase1, rIdx1) = baseIndex (head rp) 0 pList vList
--                 --     (rBase2, rIdx2) = baseIndex (last rp) 0 pList vList
--                 --
--                 -- trace(rBase1 ++ " (" ++ show rIdx1 ++ ") & " ++ rBase2++ " (" ++ show rIdx2 ++ ")") bool (rBase1, idx + rIdx1 + rIdx2) (rBase2, idx + rIdx1 + rIdx2) (isRegPointer rBase2)
--
--         "conversion" -> baseIndex (head reg) idx pList vList content
--
--         "memory" -> baseIndex (head reg) idx pList vList-- alloca, load, getelementptr
--
--         _ -> (base, idx)
--
-- | otherwise = (base, idx)

pointerInfo ptr state pList vList content
  | (isInfixOf "_ptr" ptr) = (RP ptr ptr 0 state False)
  | (isBinary var) = do
    let [a, b] = reg
    case (map isNum reg) of
      [True, True] -> do
        let (m, n) = (strToInt a, strToInt b)
            newState = show (m + n)
        (RP ptr "" (m + n) newState True)

      [False, False] -> do
        let (rbase1, rindex1) = baseIndex (head reg) 0 pList vList (reverse content)
            (rbase2, rindex2) = baseIndex (last reg) 0 pList vList (reverse content)
            rbase = bool rbase1 rbase2 (isRegPointer rbase2)
            sym = bool "+" "" ((rindex1 + rindex2) < 0)
            newState = bool (bool (rbase ++ sym ++  show (rindex1 + rindex2)) (show (rindex1 + rindex2)) (null rbase)) rbase ((rindex1 + rindex2) == 0)
        (RP ptr rbase (rindex1 + rindex2) newState True)

      _ -> do
        let idx = strToInt (bool a b (isNum b))
            rdix = bool (0-idx) (idx) (op var == "add")
            (rbase, rindex) = baseIndex (bool b a (isNum b)) rdix pList vList (reverse content)
            sym = bool "+" "" (rindex < 0)
            newState = bool (bool (rbase ++ sym ++  show rindex) (show rindex) (null rbase)) rbase (rindex == 0)
        (RP ptr rbase rindex newState True)

  --
  | (isConv var) = do
    let (rbase, rindex) = baseIndex (head reg) 0 pList vList (reverse content)
        sym = bool "+" "" (rindex < 0)
        newState = bool (bool (rbase ++ sym ++  show rindex) (show rindex) (null rbase)) rbase (rindex == 0)
        -- newState = newPtr --replaceLine (head reg) newPtr str_var state
    (RP ptr rbase rindex newState True)

  | (isLoad var) = do
    let (rbase, rindex) = baseIndex (head reg) 0 pList vList (reverse content)
        sym = bool  "+" "" (rindex < 0)
        newState = bool (bool ("[" ++ rbase ++ sym ++  show rindex ++ "]") (show rindex) (null rbase)) rbase (rindex == 0)
    (RP ptr rbase rindex newState True)
  --
  | (isBitwise var) = do
    let newState = join " " [op var, ty var, head reg ++ ",", last reg]
    if (head reg == last reg)
      then (RP ptr "" 0 newState False)
      else (RP ptr ptr 0 newState False)

  | (isAlloca var) = (RP ptr ptr 0 state False)

  | otherwise = (RP ptr ptr 0 state False)
    where (px, (var, reg)) = statement state
