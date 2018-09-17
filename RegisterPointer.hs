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

baseIndex base idx pList vList
  | (elem base (reg_32 ++ reg_64)) = (base, idx)
  | (isPrefixOf "%R" base || isPrefixOf "%E" base) = do
    let p = fromJust $ lookupList_p base
        pIdx = ridx p
    baseIndex (rbase p) (idx ++ pIdx) pList vList
  | (isPrefixOf str_var base) = do
    let v = fromJust $ lookupList base



pointerInfo ptr var reg pList vList
  | (isBinary var) = do
    let instr = op var
        idx = read (last reg) :: Integer
        rdix = bool (0-idx) (idx) (instr == "add")
        (rbase, rindex) = baseIndex (head reg) rdix pList vList
    (RP ptr rbase rindex)

  | (isBitwise var) = do
    let

  | (isConv var) = do
    let (rbase, rindex) = baseIndex (head reg) 0 pList vList
    (RP ptr rbase rindex)

  | (isLoad var) = do
    let (rbase, rindex) = baseIndex (head reg) 0 pList vList
    (RP ptr rbase rindex)
  | (isAlloca var) = (RP ptr ptr 0)
  | otherwise =
