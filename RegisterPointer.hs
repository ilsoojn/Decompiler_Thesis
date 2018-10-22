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

baseIndex :: String -> Integer -> [RP] -> [LeftVar] -> (String, Integer)
baseIndex base idx pList vList
  | (elem base (reg_32 ++ reg_64 ++ reg_ip ++ reg_other)) = (base, idx)
  | (isRegPointer base && not (isInfixOf "_ptr" base || isInfixOf "_init" base)) = do
    let p = lookupList_p base pList
    if (isNothing p || not (getPermit $ fromJust p))
      then (base, idx)
      else baseIndex (getBase $ fromJust p) (idx + (getIndex $ fromJust p)) pList vList

  | (isPrefixOf str_var base) = do
    let v = lookupList base vList
    if (isNothing v)
      then (base, idx)
      else do
        let state = getState (fromJust v)
            itype = getInstructionType (getInstr $ fromJust v)
            (x, (var, reg)) = statement state

        case itype of
          "binary" -> do
            let rp = filter (isPrefixOf str_var) reg
            let rn = filter (not . isPrefixOf str_var) reg
            if (null rp)
              then do
                let off1 = read (filter isDigit $ head rp) :: Integer
                    off2 = read (filter isDigit $ last rp) :: Integer

                (base, idx + off1 + off2)

              else if (length rp == 1 && isRegPointer (unwords rp) && isNum (unwords rn))
                then baseIndex (unwords rp) (idx + strToInt (unwords rn)) pList vList

                else (base, idx) --do
                  -- let (rBase1, rIdx1) = baseIndex (head rp) 0 pList vList
                  --     (rBase2, rIdx2) = baseIndex (last rp) 0 pList vList
                  --
                  -- trace(rBase1 ++ " (" ++ show rIdx1 ++ ") & " ++ rBase2++ " (" ++ show rIdx2 ++ ")") bool (rBase1, idx + rIdx1 + rIdx2) (rBase2, idx + rIdx1 + rIdx2) (isRegPointer rBase2)

          "conversion" -> baseIndex (head reg) idx pList vList

          "memory" -> baseIndex (head reg) idx pList vList-- alloca, load, getelementptr

          _ -> (base, idx)

  | otherwise = (base, idx)

pointerInfo :: String -> String -> [RP] -> [LeftVar] -> RP
pointerInfo ptr state pList vList
  | (isInfixOf "_ptr" ptr) = (RP ptr ptr 0 state False)
  | (isBinary var && isNum (head reg) && isNum (last reg)) = do
    let m = strToInt (head reg)
        n = strToInt (last reg)
        newState = show (m + n)
    (RP ptr "" (m + n) newState True)

  | (isBinary var && isNum (head reg)) = do
    let idx = strToInt (head reg)
        rdix = bool (0-idx) (idx) (op var == "add")
        (rbase, rindex) = baseIndex (last reg) rdix pList vList
        sym = bool "+" "" (rindex < 0)
        newState = bool (bool (rbase ++ sym ++  show rindex) (show rindex) (null rbase)) rbase (rindex == 0)
    (RP ptr rbase rindex newState True)

  | (isBinary var && isNum (last reg)) = do
    let idx = strToInt (last reg)
        rdix = bool (0-idx) (idx) (op var == "add")
        (rbase, rindex) = baseIndex (head reg) rdix pList vList
        sym = bool "+" "" (rindex < 0)
        newState = bool (bool (rbase ++ sym ++  show rindex) (show rindex) (null rbase)) rbase (rindex == 0)
    (RP ptr rbase rindex newState True)

  | (isBinary var) = do
    let (rbase1, rindex1) = baseIndex (head reg) 0 pList vList
        (rbase2, rindex2) = baseIndex (last reg) 0 pList vList
        rbase = bool rbase1 rbase2 (isRegPointer rbase2)
        sym = bool "+" "" ((rindex1 + rindex2) < 0)
        newState = bool (bool (rbase ++ sym ++  show (rindex1 + rindex2)) (show (rindex1 + rindex2)) (null rbase)) rbase ((rindex1 + rindex2) == 0)
    (RP ptr rbase (rindex1 + rindex2) newState True)
  --
  | (isConv var) = do
    let (rbase, rindex) = baseIndex (head reg) 0 pList vList
        sym = bool "+" "" (rindex < 0)
        newState = bool (bool (rbase ++ sym ++  show rindex) (show rindex) (null rbase)) rbase (rindex == 0)
        -- newState = newPtr --replaceLine (head reg) newPtr str_var state
    (RP ptr rbase rindex newState True)

  | (isLoad var) = do
    let (rbase, rindex) = baseIndex (head reg) 0 pList vList
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
