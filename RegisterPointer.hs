module RegisterPointer where

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
import IsGetSet
import Lists

baseIndex :: String -> Integer -> [RP] -> [LeftVar] -> (String, Integer)
baseIndex base idx pList vList
  | trace("\t(" ++ base ++ ", " ++ show idx ++ ")")(elem base (reg_32 ++ reg_64 ++ reg_ip)) = (base, idx)
  | ((isPrefixOf "%R" base || isPrefixOf "%E" base) && not (isInfixOf "_ptr" base)) = do
    let p = lookupList_p base pList
    if (isNothing p)
      then (base, idx)
      else trace("\tP: " ++ show (getIndex $ fromJust p))baseIndex (getBase $ fromJust p) (idx + (getIndex $ fromJust p)) pList vList

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
            let r = filter (isPrefixOf str_var) reg
            if (null r)
              then do
                let off1 = read (filter isDigit $ head r) :: Integer
                    off2 = read (filter isDigit $ last r) :: Integer

                (base, idx + off1 + off2)

              else if (length r == 1)
                then baseIndex (head r) (idx + strToInt (filter isDigit $ last r)) pList vList
                else do
                  let (rBase1, rIdx1) = baseIndex (head r) 0 pList vList
                      (rBase2, rIdx2) = baseIndex (last r) 0 pList vList

                  bool (rBase1, idx + rIdx1 + rIdx2) (rBase2, idx + rIdx1 + rIdx2) (isPrefixOf "%R" rBase2 || isPrefixOf "%E" rBase2)

          "conversion" -> baseIndex (head reg) idx pList vList

          "memory" -> baseIndex (head reg) idx pList vList-- alloca, load, getelementptr

          _ -> (base, idx)

  | otherwise = (base, idx)

pointerInfo :: String -> String -> [RP] -> [LeftVar] -> RP
pointerInfo ptr state pList vList
  | trace(""++ ptr) (isInfixOf "_ptr" ptr) = (RP ptr ptr 0 state)
  | (isBinary var && isNum (head reg) && isNum (last reg)) = do
    let m = strToInt (head reg)
        n = strToInt (last reg)
    (RP ptr "" (m + n) "")

  | (isBinary var && isNum (head reg)) = do
    let idx = strToInt (head reg)
        rdix = trace("instruction: " ++ op var) bool (0-idx) (idx) (op var == "add")
        (rbase, rindex) = baseIndex (last reg) rdix pList vList
    (RP ptr rbase rindex "")

  | (isBinary var && isNum (last reg)) = do
    let idx = strToInt (last reg)
        rdix = trace("instruction: " ++ op var) bool (0-idx) (idx) (op var == "add")
        (rbase, rindex) = baseIndex (head reg) rdix pList vList
    (RP ptr rbase rindex "")

  | (isBinary var) = do
    let (rbase1, rindex1) = baseIndex (head reg) 0 pList vList
        (rbase2, rindex2) = baseIndex (last reg) 0 pList vList
        rbase = bool rbase1 rbase2 (isPrefixOf "%R" rbase2 || isPrefixOf "%E" rbase)
    (RP ptr rbase (rindex1 + rindex2) "")

  | (isLoad var || isConv var) = do
    let (rbase, rindex) = baseIndex (head reg) 0 pList vList
    (RP ptr rbase rindex "")

  | (isBitwise var) = do
    if (head reg == last reg)
      then (RP ptr "" 0 state)
      else (RP ptr ptr 0 state)

  | (isAlloca var) = (RP ptr ptr 0 "")

  | otherwise = (RP ptr ptr 0 state)
    where (px, (var, reg)) = statement state
