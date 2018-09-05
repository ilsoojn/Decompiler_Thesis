module Elimination where

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

isLHS line fname = (not.isFunction) line && (not.isBlock) line && (not.null) fname && (isInfixOf " = " line)

propagation :: [String] -> String -> [String] -> [(String, String)] -> ([String], [(String, String)])
propagation [] fname preCont vSet = (preCont, vSet)
propagation (line:nextCont) fname preCont vSet
  | (nextCont == [""]) = (preCont, vSet)
  -- Function line
  | (isFunction line) = propagation nextCont (getFunctionName line) (preCont ++ [line]) vSet
  -- LHS line
  | (isLHS line fname) = do

    let x = statement (strip line) fname
        v = fromJust (fst x)
        (var, reg) = snd x
        use = findUse v nextCont fname []

        (pre_v, (pre_var, pre_reg)) = statement (last preCont) fname
        (next_v, (next_var, next_reg)) =  statement (head nextCont) fname

    -- condition (def: cond, use: cond)
    if (isConv var && (not.null) use && is_useConv use)
      then do
        let from_1 = (ty var) ++ " " ++ v ++ " "
            to_1 = (ty1 var) ++ " " ++ (value var) ++ " "
            tmp_nextCont = lines $ replace from_1 to_1 (unlines nextCont)

            from_2 = (ty var) ++ " " ++ v ++ ","
            to_2 = (ty1 var) ++ " " ++ (value var) ++ ","
            new_nextCont = lines $ replace from_2 to_2 (unlines tmp_nextCont)
        propagation new_nextCont fname preCont vSet

      -- idiom 1: OR (ZEXT a) (AND x 0s)
      else if (isBitwise var)
        then do
          let constInt = map strToInt (map strip $ getConstantValues reg)
              is_bit0s = hasZeros constInt
          case (op var, is_bit0s) of --, is_1s) of
            ("and", True) -> do -- > 0s

              if ((op pre_var) == "zext" && (op next_var) == "or" && next_reg == [fromJust pre_v, v])
                then do
                  --let newLine = (fromJust next_v) ++ " = " ++ (head reg) ++ " : " ++ (head pre_reg)
                  -- trace("prop: "++ (fromJust next_v) ++ " = " ++ (head reg) ++ " : " ++ (head pre_reg) ++ "\n\t"++(last preCont) ++ "\n\t" ++ line ++ "\n\t" ++ (head nextCont))
                  let newLine = (fromJust next_v) ++ " = " ++ (head pre_reg)
                  propagation (tail nextCont) fname (init preCont ++ [newLine]) vSet

                else propagation nextCont fname (preCont ++ [line]) vSet
            -- ("and", False, True) -> ["and 1s"]  -- > x
            -- ("or", True, False) -> ["or 0s"]    -- > x
            -- ("or", False, True) -> ["or 1s"]    -- > 1s
            _ -> propagation nextCont fname (preCont ++ [line]) vSet

        -- idiom 2 :: (ADD/SUB %RSP, N)
        else if (isBinary var && isStackPtr line && isConv next_var)
          then do
            let (rsp, idx) = (head reg, read (last reg) :: Integer)
            case (op var, op next_var, idx < 0) of
              ("add", "inttoptr", True) -> do
                let new_nextCont = usePropagation (fromJust next_v) (rsp ++ (show idx)) (tail nextCont)
                propagation new_nextCont fname preCont vSet
              ("sub","inttoptr", True) -> do
                let new_nextCont = usePropagation (fromJust next_v) (rsp ++ "+" ++ (show $ abs idx)) (tail nextCont)
                propagation new_nextCont fname preCont vSet
              ("add","inttoptr", False) -> do
                let new_nextCont = usePropagation (fromJust next_v) (rsp ++ "+" ++ (show idx)) (tail nextCont)
                propagation new_nextCont fname preCont vSet
              ("sub","inttoptr", False) -> do
                let new_nextCont = usePropagation (fromJust next_v) (rsp ++ "-" ++ (show idx)) (tail nextCont)
                propagation new_nextCont fname preCont vSet
              --   let newLine = (fromJust next_v) ++ " = " ++ rsp ++ "+" ++ (show (abs idx))
              --   propagation (tail nextCont) fname (n + 1) (init preCont ++ [newLine])
              _ -> propagation nextCont fname (preCont ++ [line]) vSet
          else propagation nextCont fname (preCont ++ [line]) vSet

  | otherwise = propagation nextCont fname (preCont ++ [line]) vSet

-- extensionTrim :: [String] -> String -> Integer -> [String] -> [String]
-- extensionTrim [] fn n preCont = preCont
-- extensionTrim (line : nextCont) fname n preCont
--   | var_x && dec_x = do
--     let (vary, y) = statement (strip (head nextCont)) fname
--         (varz, z) = statement (strip (head $ tail nextCont)) fname
--
--     if (isDeclare $ fst y) && (isDeclare $ fst z)
--       then do
--         let v = low (fst x)
--             new_nextCont = trace(":"++fromJust varx ++ " -> " ++ v) rp (fromJust varx) v nextCont
--         extensionTrim new_nextCont fname n preCont
--
--       else extensionTrim nextCont fname (n + 1) (preCont ++ [line])
--   | otherwise = extensionTrim nextCont fname (n + 1) (preCont ++ [line])
--   where (varx, x) = statement (strip line) fname
--         var_x = isLHS line fname
--         dec_x = isDeclare (fst x)
--         strx = isNothing varx

elimination :: [String] -> String -> [String] -> [String]
elimination [] fname preCont = preCont
elimination (line : nextCont) fname preCont
  | (isFunction line) = elimination nextCont (getFunctionName line) (preCont ++ [line])
  | (isLHS line fname) = do
    let x = statement (strip line) fname
        v = fromJust (fst x)
        use = findUse v nextCont fname []

    if (null use)
      then elimination nextCont fname preCont
      else if (isAequalB line)
        then do
          let u = '%': (getB line)
              new_nextCont = map (replaceLine v u str_var) nextCont
          elimination new_nextCont fname preCont
        else elimination nextCont fname (preCont ++ [line])
  | otherwise = elimination nextCont fname (preCont ++ [line])

-- createVar :: [String] -> String -> Integer -> [String] -> [String]
-- createVar [] fname vc preCont = preCont
-- createvar (line:nextCont) fname vc
--   | (isFunction line) = creatVar nextCont (getFunctionName line) (n + 1) list
--   | (isStackPtr line) = do
--     let rsp_idx = convArith line fname
--         e = (fname, rsp_idx)
--     if (lookup e table)
--         then do
--           let v = "%v" ++ (show vc)
--               newList = list ++ []
--   | otherwise = createVar nextCont fname (n + 1) list
