import System.Directory
import System.Environment
import System.IO
import System.Process

import Data.Array
import Data.Bool
import Data.Char
import Data.DeriveTH
import Data.Graph
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

import Block
import Lists
import RegexFunction
import OtherFunction
import StatementInstr
import StatementParse
import Idioms
import RegisterPointer
import NamingPrecision
import Propagation
import Elimination
import HLLgenerator

{-************************************************************************
                  Remove Unnecessary Line Before Process
  *************************************************************************-}
-- IF use(str), remove line
remove_uses :: String -> [String] -> [String] -> [String]
remove_uses str [] preLine = preLine
remove_uses str (line:nextLine) preLine
  | (isUse str line) = do
    if (isLHS line "temporary")
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
    if (isLHS line "temporary")
      then do
        let v = (strip.fst) (strSplit " = " line)
        remove_st str (remove_uses v nextLines []) preLines --trace("  " ++ str ++ " (" ++ v ++ ") : " ++ line)
      else
        remove_st str nextLines preLines --trace("  " ++ str ++ " ( n/a ) : " ++ line)
  | otherwise = remove_st str nextLines (preLines ++ [line]) --trace("  " ++ str ++ " (safe) : " ++ line)

remove_r :: [String] -> [String] -> [String]
remove_r [] content = content
remove_r (r:rs) content = do
  remove_r rs (remove_st r content [])

{-************************************************************************
      Disassembled Code
  ************************************************************************-}

collectAddressname :: [String] -> [(String, String)]
collectAddressname [] = []
collectAddressname (line : content) = do
  let temp = getAddressName line

  if (isNothing temp)
    then collectAddressname content
    else do
      let address =  map toUpper $ dropWhile (<='0') $ head (fromJust temp)
          name = takeWhile (/= '@') $ last (fromJust temp)
      (address, name) : (collectAddressname content)

splitFn :: [String] -> [String] -> [LeftVar] -> [RP] -> [Function]
splitFn [] codTxt v p = []
splitFn (line: content) codeTxt vList pList
  | (isFunction line) = do
    let tmpFunc = getFunctionInfo line
        faddr = getFunctionAddr line
    if (isNothing tmpFunc)
      then splitFn content (codeTxt) vList pList
      else do
        let (fnInfo: functionName: arguments: others: _) = fromJust tmpFunc
            returnType = last $ words fnInfo
            args = map strip (strSplitAll "," arguments)
            currentFn = Function faddr functionName returnType args [] pList vList codeTxt -- blocks regs vars txt
        currentFn : splitFn content [] [] []

  | (isFunctionEnd line) = splitFn content [] [] []
  | (isBasicBlock line || isBlockLabel line || isEntryExit line) = splitFn content (line:codeTxt) vList pList
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
        splitFn content (newLine:codeTxt) newList_var newList_ptr

      _ -> do
        let newList = addVariable variable vtype instr state vList
        splitFn content (line:codeTxt) newList pList

  | otherwise = splitFn content (line:codeTxt) vList pList

forFunction :: String -> [Function] -> Int -> String -> [(String, String)] -> [Function]
forFunction _ [] _ _ _ = []
forFunction run (f : fs) addr val asmT = do
  let count = 0
      emptyTxt = []

      funcName = lookup (address f) asmT

    -- where (content, (vlist, plist)) = f

      -- IDIOM
  let f_idiom = detectIdiom (code f) emptyTxt f count count
      --
      -- -- PROPAGATION
      f_prop = propagation f_idiom (code f_idiom) emptyTxt

      -- -- Variable Name
      (f_tmp, nameList) = variableName f_prop (code f_prop) emptyTxt [] count
      f_name = propagateName f_tmp (sort nameList)
      -- -- Percision
      fpTxt = precisionConversion (code f_name) addr val []
      f_fp = f_name { code = fpTxt }

      -- -- ELIMINATION
      f_elim= elimination f_fp

      -- -- SSA Format
      -- ssaContent = (ssaLLVM (generateHLL eContent) [] 1)
      -- -- Function Name
      -- fnContent = functionName ssaContent asmT
      --
      -- -- HLL Generation
      f_hll = generateHLL f_elim funcName

  case run of
    -- let (content, (vlist, plist)) = f
    "preprocess"    -> f : (forFunction run fs addr val asmT)
    "idiom"         -> f_idiom : (forFunction run fs addr val asmT)
    "propagation"   -> f_prop: (forFunction run fs addr val asmT)
    "variable_name" -> f_name : (forFunction run fs addr val asmT)
    "precision"     -> f_fp : (forFunction run fs addr val asmT)
    "elimination"   -> f_elim : (forFunction run fs addr val asmT)
    -- "SSA_format"   -> (ssaContent, (vElim, pProp)) : (forFunction run fs addr val asmT)
    -- "func_name"     -> (fnContent, (vName, pProp)) : (forFunction run fs addr val asmT)
    "HLL"           -> f_hll : (forFunction run fs addr val asmT)
    -- _ -> (ncContent, (vName, pProp)) : (forFunction run fs addr val asmT)
    _-> f : (forFunction run fs addr val asmT)


printRP [] = [ "", "--- REGISTER ---", ""]
printRP (p:ps) =
  (printRP ps) ++ [(rname p ++ " ("++ getBase p ++ ", " ++ show (getIndex p) ++ ") : " ++ getRstate p)]
  -- (rname p ++ " ("++ getBase p ++ ", " ++ show (getIndex p) ++ ") : " ++ getRstate p) : (printRP ps)
  -- (show (length ps) ++ " > " ++ rname p ++ " ("++ getBase p ++ ", " ++ show (getIndex p) ++ ") " ++ getRstate p) : (printRP ps)

printLV [] = [ "", "--- VARIABLE ---", ""]
printLV (p:ps) =
  (printLV ps) ++ [(variable p ++ " ("++ getType p ++ ") : " ++ getState p)]
  -- (variable p ++ " ("++ getType p ++ ") : " ++ getState p) : (printLV ps)
  --(show (length ps) ++ " > " ++ variable p ++ " ("++ getType p ++ ", " ++ show (getInstr p) ++ ") " ++ getState p) : (printLV ps)

printPair [] = []
printPair ((a, b):xs) = (a ++ " -> " ++ b) : (printPair xs)


{-************************************************************************
            arguments: executable or binary/object fileE
  *************************************************************************-}
main = do

  (prog_file : runOption : _) <- getArgs
  -- (prog_file : _) <- getArgs
  existProgram <- doesFileExist prog_file

  if existProgram
    then do

      let decir = prog_file ++ "_decir"
          disas = prog_file ++ "_disasm"
          asmRodata = prog_file ++ "_rodata"

      system $ "llvm-dec " ++ prog_file ++ " >> " ++ decir
      system $ "objdump -D " ++ prog_file ++ " >> " ++ disas
      system $ "objdump -s --section=.rodata " ++ prog_file ++ " >> " ++ asmRodata
      --system $ "objdump -M intel -D " ++ prog_file ++ " >>" ++ disas

      -- OPEN FILES
      handleIr <- openFile decir ReadMode
      handleAsm <- openFile disas ReadMode -- get .data & .rodata
      handleRodata <- openFile asmRodata ReadMode

      -- FILE SIZE
      szBinary <- withFile prog_file ReadMode hFileSize
      szAsm <- hFileSize handleAsm
      szDaggerIR <- hFileSize handleIr

      -- READ FILES
      contentIr <- hGetContents handleIr
      contentAsm <- hGetContents handleAsm
      contentRodata <- hGetContents handleRodata

      if ((length contentIr) /= 0 && (length contentAsm) /= 0)
        then do
          -- TEMPORARY FILE
          (tmpFile, handleTmp) <- openTempFile "." "tmpIR"

          {-********************
              PRE-PROCESSING
          *********************-}
          -- Disassembly
          let asmTable = collectAddressname (filter (not.null) $ lines $ contentAsm)

          -- Disassembly Rodata
          let rodata = last $ splitWhen (isInfixOf ".rodata") (lines contentRodata)
              rodataAddress = (strHexToDec.head.words.head) rodata
              rodataValue = map toUpper $ concat $ map (concat.init.tail.words) rodata

          -- Intermediate Representation
          let unnecessaryReg = reg_8 ++ reg_16 ++ ["%IP"]
              tempIR = (init.lines.fst) (strSplit str_main contentIr)
              sourceIR = remove_r unnecessaryReg tempIR
              nameIR = filter (not.null) $ map strip sourceIR
              functionS = splitFn (reverse nameIR) [] [] [] --((reverse nameIR)) splitFn (line: content) codeTxt vList pList fnSet

            {-********************
                MAJOR METHODS
            *********************-}
          -- let runOption = "preprocess"
              -- runOption = "idiom"
              -- runOption = "propagation"
              -- runOption = "variable_name"
              -- runOption = "precision"
              -- runOption = "elimination"
              -- runOption = "SSA_format"
              -- runOption = "func_name"
              -- runOption = "HLL"

              functionS_new = forFunction runOption functionS rodataAddress rodataValue asmTable
              trimContent = map code functionS_new -- [(content)]
              trimListV = map variables functionS_new-- map fst $ map snd trimS -- map snd trimS
              trimListP = map registers functionS_new --map snd $ map snd trimS -- map snd trimS

          -- WRITE A FILE
          -- hPutStr handleTmp $ unlines (map unlines $ map fst functionIR)
          hPutStr handleTmp $ unlines (map unlines trimContent)

          szFinal <- hFileSize handleTmp

          -- CLOSE FILES & TERMINATE
          hClose handleIr
          hClose handleAsm
          hClose handleRodata
          hClose handleTmp

          system $ "rm " ++ decir ++ " " ++ disas ++ " " ++ asmRodata
          renameFile tmpFile (prog_file ++ "_" ++ runOption ++ ".ll")

          -- mapM_ print (printRP $ head trimListP)-- $ map snd $ map snd functionIR) --(printRP plist)
          -- mapM_ print (printLV $ head trimListV)-- $ map fst $ map snd functionIR)-- (printLV vlist)
          -- mapM_ print $ printRP $ head (map registers functionS_new)
          -- mapM_ print $ printLV $ head (map fst $ map snd functionIR)
          -- mapM_ print (printPair asmTable)
          -- mapM_ print asmTable



          let infoAssembly = "Assembly: " ++ show szAsm ++ " bytes (" ++ (show $ length $ lines contentAsm) ++ ")"
              infoDaggerIR = "Dagger-IR: " ++ show szDaggerIR ++ " bytes (" ++ (show $ length $ lines contentIr) ++ ")"
              infoFinal = (map toUpper runOption) ++ ": " ++ show szFinal ++" bytes (" ++ (show $ length $ lines $ unlines $ map unlines trimContent) ++ ")"

          mapM_ print [infoAssembly, infoDaggerIR, infoFinal]
          -- mapM_ print (map code functionS)
        else do

          -- CLOSE FILES & TERMINATE
          hClose handleIr
          hClose handleAsm
          hClose handleRodata

          system $ "rm " ++ decir ++ " " ++ disas ++ " " ++ asmRodata
          putStrLn $ "\nError: Unable to convert " ++ prog_file ++ " into LLVM IR code\n"

        -- hClose tmpHandle
    else do
      putStrLn $ "Error: " ++ prog_file
