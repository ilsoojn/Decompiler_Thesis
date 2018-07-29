import System.Environment
import System.Process
import System.Directory
import System.IO
import Data.Bool
import Data.Char
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.Strings
import Data.Tuple
import Debug.Trace
import Text.Regex.Posix

import OtherFunction
import StatementParse
import StatementInstr

-- zeroPropagation :: [String] -> Integer -> [(Bool, (VAR, [String]))] -> [String]
-- zeroPropagation content iDef (firstUse:useList) = do
--   let (iUse, (use_v, use_reg)) = firstUse
--       use_op = op use_v
main = do

  let x = "%34 = and i256 %YMM0_0, -340282366920938463463374607431768211456"
      s = trace(":" ++ x)statement x "fname"
      v = fromJust (fst s)
      (v_var, v_reg) = snd s

  if (isBinary v_var || isBitwise v_var)
    then do
      let constInt = map round (map read (getConstantValues v_reg) :: [Double])
      putStrLn $ bool "F: 0s" "T: 0s" (hasZeros constInt)
      putStrLn $ bool "F: 1s" "T: 1s" (hasOnes constInt)
          -- is_1s = trace("1s")hasOnes constInt
    else putStrLn "none"
