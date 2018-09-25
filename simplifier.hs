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
import Propagation
import RegisterPointer
import IsGetSet
import Idioms
import Lists

{-************************************************************************
                  Remove Unnecessary Line Before Process
  *************************************************************************-}

-- IF use(str), remove line
remove_uses :: String -> [String] -> [String] -> [String]
remove_uses str [] preLine = preLine
remove_uses str (line:nextLine) preLine
  | (isUse str line) = do
    if (isLHS line "caller_remove_uses")
      then do
        let v = (strip.fst) (strSplit " = " line)
        remove_uses str (remove_uses v nextLine []) preLine
    else remove_uses str nextLine preLine
  | otherwise = remove_uses str nextLine (preLine ++ [line])

-- IF
remove_st :: String -> [String] -> [String] -> [String]
remove_st str [] preLines = preLines
remove_st str (line:nextLines) preLines
  | (isInfixOf str line) = do
    if (isLHS line "caller_remove_st")
      then do
        let v = (strip.fst) (strSplit " = " line)
        remove_st str (remove_uses v nextLines []) preLines
      else
        remove_st str nextLines preLines
  | otherwise = remove_st str nextLines (preLines ++ [line])

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

  -- | (startswith strStart_fn line) = do
  --
  --   let (fn, new_name) = (getFunctionName line, str_fn ++ (show fcount))
  --       new_line = replace fn new_name line
  --       new_preContent = map (replaceLine fn new_name str_fn) preContent
  --       new_nextContent = map (replaceLine fn new_name str_fn) nextContent
  --   replace_addr new_nextContent (fcount + 1) bcount (new_preContent ++ [new_line])

  | (startswith strStart_bb line) = do

    let (bn, new_name) = (getBlockName line, str_bb ++ (show bcount))
        new_line = line_bb ++ (show bcount)
        new_preContent =  map (replaceLine bn new_name str_bb) preContent
        new_nextContent = map (replaceLine bn new_name str_bb) nextContent
    replace_addr new_nextContent fcount (bcount + 1) (new_preContent ++ [new_line])

  | otherwise = replace_addr nextContent fcount bcount (preContent ++ [line])

{-************************************************************************
      Split Up Contents by Function and Create a list of Varaibles
  ************************************************************************-}
createPtrTable [] = []
createPtrTable (p:ps) = do
  let s = rname p ++ ": " ++ getBase p ++ " + " ++ show (getIndex p) ++ "(" ++ getRstate p ++ ")"
  s:(createPtrTable ps)

fnStartEndBlock (line: nextCont)
  | (isBasicBlock line || isBlockLabel line) = (line:nextCont)
  | otherwise = fnStartEndBlock nextCont

splitFn :: [String] -> String -> [String] -> [LeftVar] -> [RP] -> [([String] , ([LeftVar],[RP]))] -> [([String] , ([LeftVar],[RP]))]
splitFn [] fn c v p set = set
splitFn (line: nextC) fn cList vList pList set
  | (isFunction line) = splitFn nextC (getFunctionName line) [line] [] [] set
  | (isFunctionEnd line) = splitFn nextC fn [] [] [] $ set ++ [(cList ++ [line], (vList, pList))]
  -- | (isPrefixOf "entry_fn_" line || isPrefixOf "exit_fn_" line) =  splitFn (fnStartEndBlock (line:nextC)) fn cList vList pList set
  | (isLHS line fn) = do
    let (x, (des, reg)) = statement line
        (v, state) = strSplit' "=" line

        variable = fromJust x
        vtype = variableType des
        instr = head $ words state

    case (isRegPointer $ fromJust x) of
      True -> do
        let rp = pointerInfo variable state pList vList
            newList_ptr = pList ++ [rp]
            newList_var = addVariable variable vtype instr state vList
            newLine = concat[v, " = ", getRstate rp]
        splitFn nextC fn (cList ++ [line]) newList_var newList_ptr set
        -- splitFn nextC fn (cList ++ [newLine]) newList_var newList_ptr set

      _ -> do
        let newList = addVariable variable vtype instr state vList
        splitFn nextC fn (cList ++ [line]) newList pList set

  | otherwise = splitFn nextC fn (cList ++ [line]) vList pList set

fnElimination :: [([String] , ([LeftVar],[RP]))] -> [([String] , ([LeftVar],[RP]))]
fnElimination [] = []
fnElimination ((content, (vList, pList)):fs) =  do
  let (content_i, vList_i) = (detectIdiom content "" [] vList)
      (content_p, (vList_p, pList_p)) = propagation content_i "" vList_i pList []
      --(content_e, vList_e) = elimination False content_p "" vList_p pList []
  (content_p, (vList_p, pList_p)):fnElimination fs
  -- (elimination False content_p "" vList_p pList []) : (fnElimination fs)

printRP [] = []
printRP (p:ps) =
  (show (length ps) ++ " > " ++ rname p ++ " ("++ getBase p ++ ", " ++ show (getIndex p) ++ ") " ++ getRstate p) : (printRP ps)

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

          let unnecessaryReg = reg_8 ++ reg_16 ++ flags ++ ["%IP"]
              sourceIR = remove_r unnecessaryReg $ (init.lines.fst) (strSplit str_main  contentIr)
              nameIR = filter (not.null) $ map strip sourceIR -- (replace_addr sourceIR 0 0 [])
              functionIR = splitFn nameIR "" [] [] [] []
              contentIR = map fst functionIR
              -- pointerList = createPtrTable $ snd $ snd $ head (splitFn readyIR "" [] [] [] [])
              -- result = map fst $ fnElimination $ fnSplit readyIR "" [] [] []
              ((resultFn, (vlist, plist)):_)  = fnElimination functionIR
              result = [resultFn]
              --result = map fst $ fnElimination functionIR

          -- hPutStr handleTmp $ unlines sourceIR
          hPutStr handleTmp $ unlines (map unlines result)

          -- BEGIN EDITION

          -- CLOSE FILES & TERMINATE
          hClose handleIr
          hClose handleAsm
          hClose handleTmp

          system $ "rm " ++ decir ++ " " ++ disas
          renameFile tmpFile (prog_file ++ "Output_prop.ll")
          -- renameFile tmpFile "sampleOutput.ll"
          -- putStrLn $ "Open: " ++ prog_file
          -- print $ bool "ok" "fail" (null srcIR)

          print "ok"
          -- mapM_ print (printRP plist)
        else do

          -- CLOSE FILES & TERMINATE
          hClose handleIr
          hClose handleAsm

          system $ "rm " ++ decir ++ " " ++ disas
          putStrLn $ "\nError: Unable to convert " ++ prog_file ++ " into LLVM IR code\n"

        -- hClose tmpHandle
    else do
      putStrLn $ "Error: " ++ prog_file
