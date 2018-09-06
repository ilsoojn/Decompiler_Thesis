module OtherFunction where

import Data.Bits
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

import Lists
import IsGetSet


{- Architecture Instructions related -}

getInstructionType ::  String -> String
getInstructionType op
  | isNothing found_op = "none"
  | otherwise = fromJust found_op
  where found_op = lookup op (map swap instructions)

toSym ::  String -> String
toSym op
  | isNothing symOP = "call"
  | otherwise = fromJust symOP
  where symOP = lookup op (arithmetic ++ condition)

{-************** Other Regex Functions **************-}

isAequalB :: String -> Bool
isAequalB s = (not.null) (s =~ regex_ab :: [[String]])

getB :: String -> String
getB s = unwords $ map (head.tail) (s =~ regex_ab :: [[String]])

{-************** remove character or substring from String **************-}

rmChar :: String -> String -> String
rmChar ch str = (filter.flip notElem) ch str

rmStr :: String -> String -> String
rmStr st str = unwords $ splitOn st str

{-************** Pop Front & Back **************-}

popFirst :: [String] -> (String, [String])
popFirst x = (strip $ head x, map strip $ tail x)
popLast :: [String] -> (String, [String])
popLast x = (strip $ last x, map strip $ init x)

popFront :: String -> (String, String)
popFront x = (strip $ (head.words) x, strip $ (unwords.tail.words) x)
popBack :: String -> (String, String)
popBack x = (strip $ (last.words) x, strip $ (unwords.init.words) x)

{-************** Split By Bracket (nested) **************-}
splitBrackets [] str n list = list
splitBrackets (v:vs) str n list
  | (hasOpening v && hasEnding v && n > 0) = splitBrackets vs (str ++ " " ++ v) (n) list
  | (hasOpening v && hasEnding v && n == 0) = splitBrackets vs str (n) (list ++ [v])
  | (hasOpening v) = splitBrackets vs (str ++ " " ++ v) (n+1) list
  | (hasEnding v && n > 1) = splitBrackets vs (str ++ " " ++ v) (n-1) list
  | (hasEnding v && n == 1) = splitBrackets vs "" 0 (list ++ [str ++ " " ++ v])
  | otherwise = splitBrackets vs "" 0 (list ++ [v])

splitStartEndOneOf dlm_s dlm_e str = do
  let list = concat $ map (split (startsWithOneOf dlm_s)) $ split (endsWithOneOf dlm_e) str
      list' = filter (/= ",") $ filter (not.null) $ map strip list
  map strip (splitBrackets list' "" 0 [])

{-************** Modifing Functions **************-}
strSplit' dlm str = (strip x, strip xs) where (x, xs) = strSplit dlm str
sBreak' dlm str = (strip x, strip xs) where (x, xs) = sBreak dlm str
splitOn' dlm str = filter (not.null) $ map strip (splitOn dlm str)
splitOneOf' dlm str = filter (not.null) $ map strip (splitOneOf dlm str)

replaceWord :: String -> String -> [String] -> String -> [String]
replaceWord old new [] regex = []
replaceWord old new (str:ss) regex
  | (old == str) = new : (replaceWord old new ss regex)
  | otherwise = do
    let after_pad = unwords $ map (head.tail) (str =~ regex :: [[String]])
        tmp_word = head $ endBy after_pad str
    (bool str (new ++ after_pad) (old == tmp_word)) : (replaceWord old new ss regex)

replaceLine :: String -> String -> String -> String -> String
replaceLine old new tyStr line
  | (isInfixOf old line) = do
    let s = split (startsWith old) line--no strip!!! ["front", "old...", "old..."]
        r = tyStr ++ get_regexLine line regex_fb
    unwords $ replaceWord old new s r
  | otherwise = line
--
-- rpWord :: String -> String -> String -> String
-- rpWord old new "" = ""
-- rpWord old new line = do
--   let (x:xs) = words line
--       tmp = "%" ++ (get_regexLine x regex_fb)
--       --tmp = "%" ++ (filter isAlphaNum x)
--       word = bool x (replace old new x) (tmp == old)
--   strip (word ++ " " ++ (rpWord old new $ unwords xs))
--
-- rp :: String -> String -> [String] -> [String]
-- rp old new [] = []
-- rp old new (line: content) = (rpWord old new line):(rp old new content)

{-************** USE(variable) **************-}

usePropLine :: String -> String -> [String] -> [String]
usePropLine old new [] = []
usePropLine old new (x:xs)
  | (not.null) rs = do
    let (lpad : v : rpad :etc) = (tail.head) rs
        new_word = bool x (lpad ++ new ++ rpad) ("%" ++ v == old)
    (new_word : usePropLine old new xs)
  | otherwise = (x : usePropLine old new xs)
  where rs = (x =~ regex_padding :: [[String]])

usePropagation :: String -> String -> [String] -> [String]
usePropagation old new [] = []
usePropagation old new (x:xs) = ((unwords $ usePropLine old new $ words x):(usePropagation old new xs))

{- Type Conversion-}
strToInt :: String -> Integer
strToInt x = round (read x :: Double)

strToFloat :: String -> Double
strToFloat x = read x :: Double

{-************************************************************************
                            All Ones Byte
  *************************************************************************-}

-- pre: Double
isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

-- pre: Integer
is0s :: Integer -> Bool --, Bits a // (Num a, Integral a, Eq a, Floating a, RealFrac a) => a
is0s x = isInt $ logBase 2 (logBase 2 (fromInteger $ abs x))
-- is1s x = ((.&.) x (x+1)) == 0

is0s_sz :: (Num a, Integral a, Eq a, Floating a, RealFrac a) => a -> a -> Bool
is0s_sz sz x = sz == logBase 2 (logBase 2 (abs x))

areZeros, hasZeros :: [Integer] -> Bool --(Num a, Integral a, Eq a, Floating a, RealFrac a) => [a]
areZeros x = null $ dropWhile (==True) (map is0s x)
hasZeros x = null $ takeWhile (==True) (map is0s x)
