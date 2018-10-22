import System.Directory
import System.Environment
import System.IO
import System.Process

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

import OtherFunction
import StatementInstr
import StatementParse
import Elimination
import Propagation
import ControlFlow
import RegisterPointer2
import RegexFunction
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
      Split Up Contents by Function and Create a list of Varaibles
  ************************************************************************-}
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

      _ -> do
        let newList = addVariable variable vtype instr state vList
        splitFn nextC fn (cList ++ [line]) newList pList set

  | otherwise = splitFn nextC fn (cList ++ [line]) vList pList set

-- fnRun :: [([String] , ([LeftVar],[RP]))] -> Int -> String -> [([String] , ([LeftVar],[RP]))]
-- fnRun [] _ _ = []
-- fnRun ((content, (vList, pList)):fs) dataAddr dataValue=  do
--   let (functionName, (content_i, vList_i)) = (detectIdiom content "" [] vList)
--       (content_p, (vList_p, pList_p)) = propagation functionName content_i vList_i pList []
--       (content_e, vList_e) = elimination content_p vList_p pList_p
--   -- (content_e, (vList_e, pList_p)):fnRun fs dataAddr dataValue
--   (content_p, (vList_p, pList_p)):fnRun fs dataAddr dataValue
--   -- (content_i, (vList_i, pList)):fnRun fs dataAddr dataValue

forMod (f:fs) (v:vs) addr val = do
  let tmp1 = precisionConversion f addr val []
      (tmp2, v2) = variableName tmp1 [] v []
      tmp3 = nameConversion tmp2 []
  tmp3 : (forMod fs vs addr val)

forFunction :: String -> [ ([String] , ([LeftVar],[RP])) ] -> Int -> String -> [( [String], [LeftVar] )]
forFunction _ [] _ _ = []
forFunction run (f : fs) addr val = do
  let (fname, (iContent, vIdiom)) = trace("IDIOM:")detectIdiom content "" [] vlist
      (pContent, (vProp, pProp)) = trace("PROPAGATION:")propagation fname iContent vIdiom plist []
      (eContent, vElim) = trace("ELIMINATION:")elimination pContent vProp pProp

      pcContent = trace("PRECISION:")precisionConversion eContent addr val []

      (vnContent, (vName, nameList)) = trace("NAME LIST:")variableName pcContent [] vElim []
      ncContent = trace("NAME CONVERSION:")nameConversion vnContent nameList

      -- bBlock = splitBasicBlock ncContent "" [] [] []

  case trace("-----" ++ fname ++ "----")run of
    -- let (content, (vlist, plist)) = f
    "idiom" -> (iContent, vIdiom) : (forFunction run fs addr val)
    "prop"  -> (pContent, vProp) : (forFunction run fs addr val)
    "elim"  -> (eContent, vElim) : (forFunction run fs addr val)
    "vname" -> (ncContent, vName) : (forFunction run fs addr val)
    _ -> (ncContent, vName) : (forFunction run fs addr val)
    -- _ -> do
    --   let (fname, (iContent, vIdiom)) = detectIdiom content "" [] vlist
    --       (pContent, (vProp, pProp)) = propagation fname iContent vIdiom plist []
    --       (eContent, vElim) = elimination pContent vProp pProp
    --       ()
  where (content, (vlist, plist)) = f

printRP [] = []
printRP (p:ps) =
  (show (length ps) ++ " > " ++ rname p ++ " ("++ getBase p ++ ", " ++ show (getIndex p) ++ ") " ++ getRstate p) : (printRP ps)

printLV [] = []
printLV (p:ps) =
  (show (length ps) ++ " > " ++ variable p ++ " ("++ getType p ++ ", " ++ show (getInstr p) ++ ") " ++ getState p) : (printLV ps)

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
      --system $ "objdump -M intel -D " ++ prog_file ++ " >>" ++ disas
      system $ "objdump -s --section=.rodata " ++ prog_file ++ " >> " ++ disas
      -- OPEN & READ FILES
      handleIr <- openFile decir ReadMode
      handleAsm <- openFile disas ReadMode -- get .data & .rodata
      contentIr <- hGetContents handleIr
      contentAsm <- hGetContents handleAsm

      if (length contentIr /= 0)
        then do
          -- TEMPORARY FILE
          (tmpFile, handleTmp) <- openTempFile "." "tmpIR"

          -- Assembly
          let rodata = last $ splitWhen (isInfixOf ".rodata") (lines contentAsm)
              address = (strHexToDec.head.words.head) rodata
              values = map toUpper $ concat $ map (concat.init.tail.words) rodata

          -- Intermediate Representation
          let unnecessaryReg = reg_8 ++ reg_16 ++ flags ++ ["%IP"]
              sourceIR = remove_r unnecessaryReg $ (init.lines.fst) (strSplit str_main  contentIr)
              nameIR = filter (not.null) $ map strip sourceIR
              functionIR = splitFn nameIR "" [] [] [] []

              -- runOption = "idiom"
              -- runOption = "prop"
              runOption = "vname"
              trimS = forFunction runOption functionIR address values
              trimContent = map fst trimS -- [(content)]
              trimListV = map snd trimS

              -- result = forMod trimContent trimListV address values

          -- hPutStr handleTmp $ unlines sourceIR
          hPutStr handleTmp $ unlines (map unlines trimContent)

          -- BEGIN EDITION

          -- CLOSE FILES & TERMINATE
          hClose handleIr
          hClose handleAsm
          hClose handleTmp

          system $ "rm " ++ decir ++ " " ++ disas
          -- renameFile tmpFile (prog_file ++ "Output_idiom.ll")
          renameFile tmpFile (prog_file ++ "Output_" ++ runOption ++ ".ll")
          -- renameFile tmpFile (prog_file ++ "Output_elim.ll")
          -- renameFile tmpFile "sampleOutput.ll"
          -- putStrLn $ "Open: " ++ prog_file
          -- print $ bool "ok" "fail" (null srcIR)
          --
          -- mapM_ print (printRP plist)
          -- mapM_ print (printLV vlist)
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
