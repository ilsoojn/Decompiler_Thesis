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
import IsGetSet
import Lists

{-************************************************************************
                  Remove Unnecessary Line Before Process
  *************************************************************************-}
remove_st :: String -> [String] -> [String] -> [String]
remove_st str [] preLines = preLines
remove_st str (line:nextLine) preLines = do
  if (isInfixOf str line)
    then remove_st str nextLine preLines
    else remove_st str nextLine (preLines ++ [line])

remove_r :: [String] -> [String] -> [String]
remove_r [] content = content
remove_r (r:rs) content = do
  remove_r rs (remove_st r content [])

{-************************************************************************
            Find Address-Based Function and Basic Block Names
                    Replace them to Simple Numeric Ones
  *************************************************************************-}

replace_addr :: [String] -> Int -> Int -> [String] -> [String]
replace_addr [] fcount bcount preContent = preContent
replace_addr (line: nextContent) fcount bcount preContent

  | (startswith strStart_fn line) = do

    let (fn, new_name) = (getFunctionName line, str_fn ++ (show fcount))
        new_line = replace fn new_name line
        new_preContent = map (replaceLine fn new_name str_fn) preContent
        new_nextContent = map (replaceLine fn new_name str_fn) nextContent
    replace_addr new_nextContent (fcount + 1) bcount (new_preContent ++ [new_line])

  | (startswith strStart_bb line) = do

    let (bn, new_name) = (getBlockName line, str_bb ++ (show bcount))
        new_line = line_bb ++ (show bcount)
        new_preContent = map (replaceLine bn new_name str_bb) preContent
        new_nextContent = map (replaceLine bn new_name str_bb) nextContent
    replace_addr new_nextContent fcount (bcount + 1) (new_preContent ++ [new_line])

  | otherwise = replace_addr nextContent fcount bcount (preContent ++ [line])

{-************************************************************************
      Split Up Contents by Function and Create a list of Varaibles
  ************************************************************************-}

fnSplit :: [String] -> String -> [String] -> [(String, String)] -> [ ([String] , [(String, String)]) ] -> [ ([String] , [(String, String)]) ]
fnSplit [] fn cont var set = set
fnSplit (line: nextC) fn cont var set
  | (isFunction line) = fnSplit nextC (getFunctionName line) [line] [] set
  | (isFunctionEnd line) = fnSplit nextC fn [] [] $ set ++ [(cont ++ [line], var)]
  | (isLHS line fn) = do
    let (x, (des, reg)) = statement line
        newList = addVariable (fromJust x) (variableType des) var
    fnSplit nextC fn (cont ++ [line]) newList set
  | otherwise = fnSplit nextC fn (cont ++ [line]) var set

{-************************************************************************
                          Elimination & Propagation
  *************************************************************************-}
fnElimination :: [([String], [(String, String)])] -> [[String]]
fnElimination [] = []
-- fnElimination (f:fs) = (propagation f "" 0 []):(fnElimination fs)
fnElimination (f:fs) = do
  let (conts, vars) = f
      cont_p = fst (propagation conts "" [] vars)
      --cont_t = (extensionTrim cont_p "" 0 [])
  (elimination cont_p "" []):(fnElimination fs)

{-************************************************************************
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

          let unnecessaryReg = reg_8 ++ reg_16 ++ reg_ip
              srcIR = remove_r unnecessaryReg $ (init.lines.fst) (strSplit str_main  contentIr)
              readyIR = filter (not.null) $ map strip (replace_addr srcIR 0 0 [])
              -- storeLists = findInstr (readyIR) "" [] []
              -- result = elimination (propagation readyIR "" 0 []) "" 0 []
              result = fnElimination $ fnSplit readyIR "" [] [] []

          hPutStr handleTmp $ unlines (map unlines result)
          -- BEGIN EDITION

          -- CLOSE FILES & TERMINATE
          hClose handleIr
          hClose handleAsm
          hClose handleTmp

          system $ "rm " ++ decir ++ " " ++ disas
          renameFile tmpFile "sampleOutput.ll"
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
