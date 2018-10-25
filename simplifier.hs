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
-- splitFn :: [String] -> [String]-> String -> [String] -> [LeftVar] -> [RP] -> [([String] , ([LeftVar],[RP]))] -> [([String] , ([LeftVar],[RP]))]
-- splitFn [] content fn c v p set = set
-- splitFn (line: nextC) preC fn cList vList pList set
--   | (isFunction line) = splitFn nextC (preC ++ [line]) (getFunctionName line) [line] [] [] set
--   | (isFunctionEnd line) = splitFn nextC (preC ++ [line]) fn [] [] [] $ set ++ [(cList ++ [line], (vList, pList))]
--   -- | (isPrefixOf "entry_fn_" line || isPrefixOf "exit_fn_" line) =  splitFn (fnStartEndBlock (line:nextC)) fn cList vList pList set
--   | (isLHS line fn) = do
--     let (x, (des, reg)) = statement line
--         (v, state) = strSplit' "=" line
--
--         variable = fromJust x
--         vtype = variableType des
--         instr = head $ words state
--
--     case (isRegPointer $ fromJust x) of
--       True -> do
--         let rp = pointerInfo variable state pList vList preC
--             newList_ptr = pList ++ [rp]
--             newList_var = addVariable variable vtype instr state vList
--             newLine = concat[v, " = ", getRstate rp]
--         splitFn nextC (preC ++ [line]) fn (cList ++ [line]) newList_var newList_ptr set
--
--       _ -> do
--         let newList = addVariable variable vtype instr state vList
--         splitFn nextC (preC ++ [line]) fn (cList ++ [line]) newList pList set
--
--   | otherwise = splitFn nextC (preC ++ [line]) fn (cList ++ [line]) vList pList set

splitFn :: [String] -> String -> [String] -> [LeftVar] -> [RP] -> [([String] , ([LeftVar],[RP]))] -> [([String] , ([LeftVar],[RP]))]
splitFn [] fn c v p set = set
splitFn (line: content) fn cList vList pList set
  | (isFunction line) = splitFn content (getFunctionName line) [] [] [] $ set ++ [(line:cList, (vList, pList))]
  | (isFunctionEnd line) = splitFn content fn [line] [] [] set
  | (isBasicBlock line || isBlockLabel line || isEntryExit line) =  splitFn content fn (line:cList) vList pList set
  | (isLHS line "temporary") = do
    let (x, (des, reg)) = statement line
        (v, state) = strSplit' "=" line

        variable = fromJust x
        vtype = variableType des
        instr = head $ words state

    case (isRegPointer v) of
      True -> do
        let rp = pointerInfo variable state pList vList content
            newList_ptr = pList ++ [rp]
            newList_var = addVariable variable vtype instr state vList
            newLine = concat[v, " = ", getRstate rp]--trace(v ++ ": (" ++ getBase rp ++ ", " ++ show (getIndex rp) ++ ") : " ++ getRstate rp)
        splitFn content fn (newLine:cList) newList_var newList_ptr set

      _ -> do
        let newList = addVariable variable vtype instr state vList
        splitFn content fn (line:cList) newList pList set

  | otherwise = splitFn content fn (line:cList) vList pList set

forFunction :: String -> [ ([String] , ([LeftVar],[RP])) ] -> Int -> String -> [( [String], ([LeftVar], [RP]) )]
forFunction _ [] _ _ = []
forFunction run (f : fs) addr val = do
  let (fname, (iContent, vIdiom)) = detectIdiom content "" [] vlist
      (pContent, (vProp, pProp)) = propagation fname iContent vIdiom plist []

      (vnContent, (vName, nameList)) = variableName pContent [] vProp []
      ncContent = propagateName vnContent (sort nameList)

      pcContent = precisionConversion ncContent addr val []

      (eContent, vElim) = elimination pcContent vName pProp

  case trace("-----" ++ fname ++ "----")run of
    -- let (content, (vlist, plist)) = f
    "idiom" -> (iContent, (vIdiom, plist)) : (forFunction run fs addr val)
    "prop"  -> (pContent, (vProp, pProp)) : (forFunction run fs addr val)
    "vname" -> (ncContent, (vName, pProp)) : (forFunction run fs addr val)
    "elim"  -> (eContent, (vElim, pProp)) : (forFunction run fs addr val)
    _ -> (ncContent, (vName, pProp)) : (forFunction run fs addr val)
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
              functionIR = splitFn (reverse nameIR) "" [] [] [] []
              -- functionIR = splitFn nameIR [] "" [] [] [] []

              -- runOption = "idiom"
              -- runOption = "prop"
              -- runOption = "vname"
              runOption = "elim"
              trimS = forFunction runOption functionIR address values
              trimContent = map fst trimS -- [(content)]
              trimListV = map fst $ map snd trimS -- map snd trimS
              trimListP = map snd $ map snd trimS -- map snd trimS

          -- mapM_ print (printRP $ head trimListP)
          -- putStrLn ""
          -- mapM_ print (printLV $ head trimListV)
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
