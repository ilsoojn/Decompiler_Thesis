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

{-************************************************************************
                Line Recursion to Remove a Line with 'str'
  *************************************************************************-}
-- param: a register, a code
-- return: new_code
rmLineRecursion :: String -> [String] -> [String] -> [String]
rmLineRecursion str [] preLines = preLines
rmLineRecursion str (line:nextLine) preLines = do
  if (isInfixOf str line)
    then rmLineRecursion str nextLine preLines
    else rmLineRecursion str nextLine (preLines ++ [line])

{-************************************************************************
                  List Recursion ( for element in List )
  *************************************************************************-}
-- param: list of registers, a code
-- return: new_code
regRecursion :: [String] -> [String] -> [String]
regRecursion [] content = content
regRecursion (x:xs) content = do
  regRecursion xs (rmLineRecursion x content [])

{-************************************************************************
    Remove Unnecessary Registers such as 8-bit, 16-bit, and IndexPointers
  *************************************************************************-}
rmReg :: [String] -> [String]
rmReg content  = regRecursion (reg_8 ++ reg_16 ++ reg_ip) content -- remove 8-bit & 16-bit register statements

{-************************************************************************
            Find Address-Based Function and Basic Block Names
                    Replace them to Simple Numeric Ones
  *************************************************************************-}

nameReplacement :: [String] -> Int -> Int -> [String] -> [String]
nameReplacement [] fcount bcount preContent = preContent
nameReplacement (line: nextContent) fcount bcount preContent
  | (startswith start_fn line) = do

    let (fname, new_name) = (getFunctionName line, "@fn_" ++ (show fcount))
        new_line = replace fname new_name line
        new_preContent = map (replaceWord fname new_name) preContent
        new_nextContent = map (replaceWord fname new_name) nextContent
    nameReplacement new_nextContent (fcount + 1) bcount (new_preContent ++ [new_line])

  | (startswith start_bb line) = do

    let (bbname, new_name) = (getBlockName line, "%bb_" ++ (show bcount))
        new_line = "; <label>:bb_" ++ (show bcount)
        new_preContent = map (replaceWord bbname new_name) preContent
        new_nextContent = map (replaceWord bbname new_name) nextContent
    nameReplacement new_nextContent fcount (bcount + 1) (new_preContent ++ [new_line])

  | otherwise = nameReplacement nextContent fcount bcount (preContent ++ [line])

{-************************************************************************
                          Function & Basic Block
  ************************************************************************-}

blockInfo :: [String] -> [String] -> String -> [(String, String)] -> [(String, String)]
blockInfo preContent (line:nextContent) name info
  | (is_RegexMatch regexEnd_fn line) = info ++ [(name, last preContent)] -- End Function
  | (is_RegexMatch regexInit_bb line) = do
    let newBlock = "bb_" ++ (get_RegexMatch regexInit_bb line)
        newInfo = info ++ [(name, last preContent)]
    blockInfo (preContent ++ [line]) nextContent newBlock newInfo
  | otherwise = blockInfo (preContent ++ [line]) nextContent name info

blockInfoBegin preContent (line:nextContent) = do
  if (is_RegexMatch regexInit_bb line)
    then blockInfo (preContent ++ [line]) nextContent ("bb_" ++ (get_RegexMatch regexInit_bb line)) []
    else blockInfoBegin (preContent ++ [line]) nextContent
--
-- functionInfo:: [String] -> [Function] -> [Function]
-- functionInfo [] info = info
-- functionInfo (line:content) info
--   | (isInfixOf start_fn line) = do
--     let fname = "fn_" ++ (get_RegexMatch regexInit_fn line)
--     functionInfo content (info ++ [Function fname (blockInfoBegin [] content)])
--   | (isVarDeclare line fname) = do
--       let s = statement (strip line) fname
--           v = fromJust (fst s)
--           x = fst (snd s)
--       if (isCatchSwitch x)
--         then do
--           addVariable v "cs"
--
--   | otherwise = functionInfo content info

-- splitFunction :: [String] -> [String] -> [[String]] -> [[String]]
-- splitFunction [] fline fset = fset
-- splitFunction (line:content) fline fset
--   | (isFunction line) = splitFunction content [line] fset
--   | (isFunctionEnd line) = splitFunction content [] (fset ++ [fline ++ [line]])
--   | otherwise = splitFunction content (fline ++ [line]) fset


splitFunction :: [String] -> String -> [String] -> [(String, String)] -> [[String]] -> [[(String, String)]] -> ([[String]], [[(String, String)]])
splitFunction [] fname conts vars fnCont fnVar = (fnCont, fnVar)
splitFunction (line:nextCont) fname conts vars fnCont fnVar
  | (isFunction line) = splitFunction nextCont (getFunctionName line) [line] [] fnCont fnVar
  | (isFunctionEnd line) = splitFunction nextCont fname [] [] (fnCont ++ [conts ++ [line]]) (fnVar ++ [vars])
  | otherwise = do
    if (isVarDeclare line fname)
      then do
        let (v, (var, reg)) = statement line fname
            new_vars = addVariable (fromJust v) (variableType var) vars
        splitFunction nextCont fname (conts ++ [line]) new_vars fnCont fnVar
      else splitFunction nextCont fname (conts ++ [line]) vars fnCont fnVar

-- splitFunction :: [String] -> [String] -> [(String, String)] -> [[String]] -> ([[String]], [[(String, String)]])
-- splitFunction [] fname ls vs fls fvs = (fls, fvs)
-- splitFunction (line:content) fname ls vs fls fvs
--   | (isFunction line) = splitFunction content (getFunctionName line) [line] [] fls fvs
--   | (isFunctionEnd line) = splitFunction content fname [] [] (fls ++ [ls ++ [line]]) (fvs ++ [vs])
--   | otherwise = do
--     if (isVarDeclare line fname)
--       then do
--         let (v, (var, reg)) = statement line fname
--             new_vs = addVariable (fromJust v) (variableType var) vs
--         splitFunction content (fline ++ [line]) new_vs fls fvs
--       else splitFunction content (fline ++ [line]) vs fls fvs

{-************************************************************************
                          Elimination & Propagation
  *************************************************************************-}
byFn_Elimination :: [[String]] -> [[(String, String)]] -> [[String]]
byFn_Elimination [] [] = []
-- byFn_Elimination (f:fs) = (propagation f "" 0 []):(byFn_Elimination fs)
byFn_Elimination (f:fs) (v:vs) = do
  let cont_p = fst (propagation f "" [] v)
      --cont_t = (extensionTrim cont_p "" 0 [])
  (elimination cont_p "" []):(byFn_Elimination fs vs)

{-************************************************************************
                            BackTracking
  *************************************************************************-}
--
-- backtrackList :: String -> [String] -> [String] -> [(String, VAR)] -> [(String, VAR)]
-- backtrackList fname [] content stack = stack
-- backtrackList fname (v:vs) content stack = do
--   backtrackList fname vs content $ backtrack fname v content stack
--
-- backtrack :: String -> String -> [String] -> [(String, VAR)] -> [(String, VAR)]
-- backtrack fname v content stack
--   | (isInfixOf start_fn $ last content) = (stack) -- ++ (head $ last stack))
--   | not.null $ unwords $ map (head.tail) ((dropWhile isSpace $ last content) =~ (v ++ " = (.*)") :: [[String]]) = do
--     let (lhs, rhs) = strSplit'" = " $ last content
--         op = head $ words rhs
--     if isInfixOf "%RSP" $ last content
--       then do
--         let (rsp_v, idx) = strSplit ", " (last content)
--         stack ++ [(strip v, RSP fname (last $ words rsp_v) idx)]
--       else
--         case op of
--           "load" -> stack ++ [(strip lhs, loadStatement rhs fname)]
--           "alloca" -> stack ++ [(strip lhs, allocaStatement rhs fname)]
--           _ -> do
--             let parse_s = parseStatement (strip lhs) rhs -- Def(v) <- conversion/computation
--                 new_stck = bool (stack ++ [(v, fst parse_s)]) (stack ++ [(v, Other)]) (isConv $ fst parse_s)
--                 vList = getSSAssignValues (snd parse_s)
--             backtrackList fname vList (init content) new_stck
--   | otherwise = backtrack fname v (init content) stack -- not Def(v); maybe Use(v) but it doesn't matter
--
-- strVaraible :: [(String, VAR)] -> String -> [String] -> [(String, VAR)]
-- strVaraible (v:vs) fname content
--   | isConst(snd v) = [v]
--   | otherwise = backtrack fname (fst v) content []
--
-- findInstr :: [String] -> String -> [String] -> [STORE] -> [STORE]
-- findInstr [] fname content stack = stack
-- findInstr (line:nextLine) fname pre_content stack = do
--   if isInfixOf "store" line
--     then do
--       -- Extract value and location variables from the the IR 'store' format
--       -- store [...] <type> <value>, <type ptr> <location ptr> [, ...]
--       let s = storeStatement line     -- statement
--           v = strVaraible (str_v s) fname pre_content  -- store variable
--           at = backtrack fname (fst $ head $ str_at s) pre_content []  -- store location
--
--           -- update statement info and add to the STORE stack
--           -- new_s = s{str_v = v, str_at = at}
--           new_s = s{str_v = v, str_at = at}
--           new_stack = stack ++ [new_s]
--
--       findInstr nextLine fname (pre_content ++ [line]) new_stack
--
--     else if isInfixOf start_fn line
--       then do
--         let fnum = unwords $ map (head.tail) (line =~ regexInit_fn :: [[String]])
--             new_fname = "@fn_" ++ fnum
--         findInstr nextLine new_fname (pre_content ++ [line]) stack
--       else findInstr nextLine fname (pre_content ++ [line]) stack

{-************************************************************************
  main():
    arguments: executable or binary/object fileE
  *************************************************************************-}
main = do

  (prog_file:_) <- getArgs
  existProgram <- doesFileExist prog_file

  if existProgram
    then do

      let decir = prog_file ++ "_decir"
      let disas = prog_file ++ "_disasm"

      system $ "llvm-dec " ++ prog_file ++ " >> " ++ decir
      system $ "objdump -M intel -D " ++ prog_file ++ " >>" ++ disas

      -- OPEN & READ FILES
      handleIr <- openFile decir ReadMode
      handleAsm <- openFile disas ReadMode
      contentIr <- hGetContents handleIr
      contentAsm <- hGetContents handleAsm

      if (length contentIr /= 0)
        then do
          -- TEMPORARY FILE
          (tmpFile, handleTmp) <- openTempFile "." "tmpIR"

          let srcIR = rmReg $ (init.lines.fst) (strSplit "@main"  contentIr)
              readyIR = filter (not.null) $ map strip (nameReplacement srcIR 0 0 [])
              (fset,vset) = splitFunction readyIR "" [] [] [] []
              -- storeLists = findInstr (readyIR) "" [] []
              -- result = elimination (propagation readyIR "" 0 []) "" 0 []
              result = byFn_Elimination fset vset

          hPutStr handleTmp $ unlines (map unlines result)
          -- BEGIN EDITION

          -- CLOSE FILES & TERMINATE
          hClose handleIr
          hClose handleAsm
          hClose handleTmp

          system $ "rm " ++ decir ++ " " ++ disas
          renameFile tmpFile "resultIR"
          -- putStrLn $ "Open: " ++ prog_file
          -- print $ bool "ok" "fail" (null srcIR)

          print "ok"
        else do

          -- CLOSE FILES & TERMINATE
          hClose handleIr
          hClose handleAsm

          system $ "rm " ++ decir ++ " " ++ disas
          putStrLn $ "\nError: Unable to convert " ++ prog_file ++ " into LLVM IR code\n"

        -- hClose tmpHandle
    else do
      putStrLn $ "Error: " ++ prog_file
