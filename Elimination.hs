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

propagation :: [String] -> String -> Integer -> [String] -> [String]
propagation [] fname n preCont = preCont
propagation (line : nextCont) fname n preCont
  | (isFunction line) = propagation nextCont (getFunctionName line) (n + 1) (preCont ++ [line])

  | ((not.isFunction) line && (not.isBlock) line && (not.null) fname && (isInfixOf " = " line)) = do
    let x = statement (strip line) fname
        v = fromJust (fst x)
        (v_var, v_reg) = snd x
        use = findUse v nextCont fname (n + 1) []
    if (isConv v_var && (not.null) use && is_useConv use)
      then do
        let from_1 = (ty2 v_var) ++ " " ++ v ++ " "
            to_1 = (ty1 v_var) ++ " " ++ (value v_var) ++ " "
            tmp_nextCont = lines $ replace from_1 to_1 (unlines nextCont)

            from_2 = (ty2 v_var) ++ " " ++ v ++ ","
            to_2 = (ty1 v_var) ++ " " ++ (value v_var) ++ ","
            new_nextCont = lines $ replace from_2 to_2 (unlines tmp_nextCont)
        propagation new_nextCont fname n preCont  -- n

      else if (isBitwise v_var)
        then do
          let constInt = map round (map read (map strip $ getConstantValues v_reg) :: [Double])
              (is_bit0s, is_1s) = (hasZeros constInt, hasOnes constInt)
              is_num0s = (0 == head constInt) || (0 == last constInt)
          case (op v_var, is_bit0s, is_num0s) of --, is_1s) of
            ("and", True, False) -> do -- > 0s
              let (pre_v, (pre_var, pre_reg)) = statement (last preCont) fname
                  (next_v, (next_var, next_reg)) =  statement (head nextCont) fname

              if ((op pre_var) == "zext" && (op next_var) == "or" && next_reg == [fromJust pre_v, v])
                then do
                  let newLine = (fromJust next_v) ++ " = " ++ v ++ ":" ++ (fromJust pre_v)
                  propagation (tail nextCont) fname (n - 1) (init preCont ++ [newLine]) -- n

                else propagation nextCont fname (n + 1) (preCont ++ [line])
            -- ("and", False, True) -> ["and 1s"]  -- > x
            -- ("or", True, False) -> ["or 0s"]    -- > x
            -- ("or", False, True) -> ["or 1s"]    -- > 1s
            _ -> propagation nextCont fname (n + 1) (preCont ++ [line])
        else propagation nextCont fname (n + 1) (preCont ++ [line])
  | otherwise = propagation nextCont fname (n + 1) (preCont ++ [line])

elimination :: [String] -> String -> Integer -> [String] -> [String]
elimination [] fname n preCont = preCont
elimination (line : nextCont) fname n preCont
  | (isFunction line) = elimination nextCont (getFunctionName line) (n + 1) (preCont ++ [line])
  | ((not.isFunction) line && (not.isBlock) line && (not.null) fname && (isInfixOf " = " line)) = do
    let x = statement (strip line) fname
        v = fromJust (fst x)
        (v_var, v_reg) = snd x
        use = findUse v nextCont fname (n + 1) []

    if (null use)
      then elimination nextCont fname n preCont
      else elimination nextCont fname (n + 1) (preCont ++ [line])
  | otherwise = elimination nextCont fname (n + 1) (preCont ++ [line])

-- elimination :: [String] -> String -> Integer -> [String] -> [String]
-- elimination [] fname n preCont = preCont
-- elimination (line : nextCont) fname n preCont
--   | (null fname && (not $ isFunction line)) = trace(" "++ (show n) ++ "\t0 & none >> " ++ line) elimination nextCont fname (n+1) preCont --n
--   | (isFunction line) = do
--     let fn = (getFunctionName line)
--     trace(" "++ (show n) ++ "\t1.0 & " ++ fn ++ " >> " ++ line) elimination nextCont (getFunctionName line) (n + 1) (preCont ++ [line])
--   | (isBlock line) = trace(" " ++ (show n) ++ "\t2.0 & " ++ fname ++ " >> " ++ line) elimination nextCont fname (n + 1) (preCont ++ [line])
--   | (isInfixOf " = " line) = do
--     let x = statement (strip line) fname
--         v = fromJust (fst x)
--         (v_var, v_reg) = snd x
--         use = findUse v nextCont fname (n + 1) []
--
--     if (length use == 0)
--       then trace(" " ++ (show n) ++ "\t3.1 & " ++ fname ++ " >> " ++ line) elimination nextCont fname (n+1) preCont  -- n
--
--       else if (isConv v_var && is_useConv use)
--         then do
--           let from = (ty2 v_var) ++ " " ++ v
--               to = (ty1 v_var) ++ " " ++ (value v_var)
--               new_nextCont = lines $ replace from to (unlines nextCont)
--           trace(" " ++ (show n) ++ "\t3.2 & " ++ fname ++ " >> " ++ line) elimination new_nextCont fname (n+1) preCont  -- n
--     --  else trace(">> here2")elimination nextCont fname (n + 1) (preCont ++ [line]) -- << TEST CASE
--         else if (isBinary v_var || isBitwise v_var)
--           then do
--             let constVal = (map strip $ getConstantValues v_reg)
--                 constInt = map round (map read constVal :: [Double])
--                 is_0s = hasZeros constInt
--                 is_1s = hasOnes constInt
--             case (op v_var, is_0s) of --, is_1s) of
--               ("and", True) -> do -- > 0s
--                 let (pre_v, (pre_var, pre_reg)) = statement (last preCont) fname
--                     (next_v, (next_var, next_reg)) =  statement (head nextCont) fname
--
--                 if ((op pre_var) == "zext" && (op next_var) == "or" && next_reg == [fromJust pre_v, v])
--                   then do
--                     let newLine = (fromJust next_v) ++ " = " ++ v ++ ":" ++ (fromJust pre_v)
--                     trace(" " ++ (show n) ++ "\t3.3 & " ++ fname ++ " >> " ++ line) elimination (tail nextCont) fname (n+1) (init preCont ++ [newLine]) -- n
--
--                   else trace(" " ++ (show n) ++ "\t3.4 & " ++ fname ++ " >> " ++ line) elimination nextCont fname (n + 1) (preCont ++ [line])
--               -- ("and", False, True) -> ["and 1s"]  -- > x
--               -- ("or", True, False) -> ["or 0s"]    -- > x
--               -- ("or", False, True) -> ["or 1s"]    -- > 1s
--               _ -> trace(" " ++ (show n) ++ "\t3.5 & " ++ fname ++ " >> " ++ line) elimination nextCont fname (n + 1) (preCont ++ [line])
--
--           else trace(" " ++ (show n) ++ "\t3.6 & " ++ fname ++ " >> " ++ line) elimination nextCont fname (n + 1) (preCont ++ [line])
--   | otherwise = trace(" " ++ (show n) ++ "\t4.0 & " ++ fname ++ " >> " ++ line) elimination nextCont fname (n + 1) (preCont ++ [line])
