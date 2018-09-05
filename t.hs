import Data.Bool
import Data.Char
import Data.DeriveTH
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Strings
import Data.Tuple
import Data.Typeable

import Debug.Trace
import Text.Regex.Posix
import Lists
import IsGetSet
import OtherFunction
--
-- f :: String -> String -> Bool
-- f r s = (not.null) $ map (head.tail) (s =~ r :: [[String]])
--
-- g :: String -> String -> Bool
-- g r s = bool False True (null (s =~ r :: [[String]]))
--
-- main = do
--   let x = "hello world!"
--       y = "(.*)o(.*)"
--       v = ".*zzz(.*)"
--
--   putStrLn $ "f: " ++ (bool "False" "True" (f v x))
--   putStrLn $ "g: " ++ (bool "False" "True" (g v x))

-- replaceWord :: String -> String -> [String] -> String -> [String]
-- replaceWord old new [] regex = []
-- replaceWord old new (str:ss) regex
--   | (old == str) = new : (replaceWord old new ss regex)
--   | otherwise = do
--     let after_pad = unwords $ map (head.tail) (str =~ regex :: [[String]])
--         tmp_word = head $ endBy after_pad str
--     (bool str (new ++ after_pad) (old == tmp_word)) : (replaceWord old new ss regex)
--
-- replaceLine :: String -> String -> String -> String -> String
-- replaceLine old new tyStr line
--   | (isInfixOf old line) = do
--     let s = split (startsWith old) line--no strip!!! ["front", "old...", "old..."]
--         r = tyStr ++ regex_after_padding
--     unwords $ replaceWord old new s r
--   | otherwise = line
--
-- main = do
--   let a = "bb_40041        ; preds = %entry_."
--       b = "bb_400410        ; preds = %entry_."
--       c = "br label %bb_400410"
--       d = "br label %bb_40041"
--       e = "br i1 %CC_BE_0, label %bb_40041, label %bb_400410"
--       f = "define void @fn_400410(%regset...) {"
--       g = "define void @fn_40041(%regset...) {"
--       h = "call void @fn_400410(%regset* %0)"
--       i = "call void @fn_40041(%regset* %0)"
--
--       x = "@fn_40041"
--       y = "%bb_40041"
--
--       s = [a, b, c, d, e, f, g, h, i]
--
--   putStrLn $ "\nA >> " ++ (replaceLine y "%BB" str_bb a)
--   putStrLn $ "B >> " ++ (replaceLine y "%BB" str_bb b)
--   putStrLn $ "C >> " ++ (replaceLine y "%BB" str_bb c)
--   putStrLn $ "D >> " ++ (replaceLine y "%BB" str_bb d)
--   putStrLn $ "E >> " ++ (replaceLine y "%BB" str_bb e)
--   putStrLn $ "F >> " ++ (replaceLine y "%BB" str_bb f)
--   putStrLn $ "G >> " ++ (replaceLine y "%BB" str_bb g)
--   putStrLn $ "H >> " ++ (replaceLine y "%BB" str_bb h)
--   putStrLn $ "I >> " ++ (replaceLine y "%BB" str_bb i)
--   putStrLn "---------------------------"
--   putStrLn $ "A >> " ++ (replaceLine x "@FN" str_fn a)
--   putStrLn $ "B >> " ++ (replaceLine x "@FN" str_fn b)
--   putStrLn $ "C >> " ++ (replaceLine x "@FN" str_fn c)
--   putStrLn $ "D >> " ++ (replaceLine x "@FN" str_fn d)
--   putStrLn $ "E >> " ++ (replaceLine x "@FN" str_fn e)
--   putStrLn $ "F >> " ++ (replaceLine x "@FN" str_fn f)
--   putStrLn $ "G >> " ++ (replaceLine x "@FN" str_fn g)
--   putStrLn $ "H >> " ++ (replaceLine x "@FN" str_fn h)
--   putStrLn $ "I >> " ++ (replaceLine x "@FN" str_fn i)

main = do
  let x = "3.1415"
      y = "31415"
      z = "256"

      [a, b, c] = [(strToFloat x), (strToFloat y), (strToFloat z)]
      [d, e, f] = [(strToInt x), (strToInt y), (strToInt z)]

  putStrLn " "
  -- putStrLn $ "is0s     (F): " ++ (bool "False" "True" (is0s a))
  -- putStrLn $ "is0s     (F): " ++ (bool "False" "True" (is0s b))
  -- putStrLn $ "is0s     (T): " ++ (bool "False" "True" (is0s c))
  -- putStrLn " "
  -- putStrLn $ "isZeros     (F): " ++ (bool "False" "True" (isZeros s))
  -- putStrLn $ "isZeros     (T): " ++ (bool "False" "True" (isZeros [c]))

  putStrLn $ "isInt   (F):\t" ++ (bool "False" "True" (isInt a))
  putStrLn $ "isInt   (T):\t" ++ (bool "False" "True" (isInt b))
  putStrLn $ "isInt   (T):\t" ++ (bool "False" "True" (isInt c))
  putStrLn ""
  putStrLn $ "is0s    (F):\t" ++ (bool "False" "True" (is0s d))
  putStrLn $ "is0s    (F):\t" ++ (bool "False" "True" (is0s e))
  putStrLn $ "is0s    (T):\t" ++ (bool "False" "True" (is0s f))
  putStrLn ""
  putStrLn $ "isZeros (F):\t" ++ (bool "False" "True" (isZeros [d, e, f]))
  putStrLn $ "isZeros (T):\t" ++ (bool "False" "True" (isZeros [f]))
