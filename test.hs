import System.Directory
import System.Environment
import System.IO
import System.Process

import Data.Bool
import Data.Char
import Data.DeriveTH
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.String.Utils
import Data.Strings
import Data.Tuple
import Data.Typeable

import Debug.Trace
import Text.Regex.Posix

import OtherFunction
import StatementInstr
import StatementParse
import Elimination
import Propagation
import RegisterPointer
import RegexFunction
import Idioms
import Lists

main = do
  let rodata = ["400550 01000200 00000000 00000000 00002440", "400560 a4703d0a d7937340"]
      addr = (strHexToDec.head.words.head) rodata
      values = concat $ map (concat.tail.words) rodata
      x = 4195680
  print $ (show addr ++" "++ show (addr + length values))
  print $ splitByLength 2 values
  print $ getData 64 (fromIntegral addr) values x

-- main = do
--   let x = "[%RSP-43]"
--       y = get_regexLine x regex_var
--       [f, b] = get_regexLine_all x regex_fbpadding
--
--   putStrLn x
--   putStrLn (y ++ " >> " ++ f ++" :: " ++ b)

-- main = do
  --
  -- let d = 313.24
  --     h = "407393D70A3D70A4"
  --     (e, f) = splitAt 3 (filter (/= ' ') h)
  --     h' = strHexToDouble h
  --     e' = strHexToStrBin e
  --     f' = strHexToStrBin f
  --     x = filter (/=0) $ zipWith (\i b -> fromIntegral(b) * 2**(0-fromIntegral(i))) [1..(length f')] (map digitToInt f')
  --
  -- putStrLn (h ++ " -> " ++ show h')
  -- putStrLn $ bool "Incorrect" "Correct" (h' == d)
