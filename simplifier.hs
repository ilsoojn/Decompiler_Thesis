import System.Directory
import System.Environment
import System.IO
import System.Process

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

import OtherFunction
import StatementInstr
import StatementParse
import Elimination
import Propagation
import Block
import NamingPrecision
import RegisterPointer
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



{-************************************************************************
      Split Up Contents by Function and Create a list of Varaibles
  ************************************************************************-}
splitFn :: [String] -> String -> [String] -> [LeftVar] -> [RP] -> [([String] , ([LeftVar],[RP]))] -> [([String] , ([LeftVar],[RP]))]
splitFn [] fn c v p set = set
splitFn (line: content) fn cList vList pList set
  | (isFunction line) = do
    let newSet = (line:cList, (vList, pList))
    splitFn content (getFunctionName line) [] [] [] (set ++ [newSet])
  | (isFunctionEnd line) = splitFn content fn [line] [] [] set
  | (isBasicBlock line || isBlockLabel line || isEntryExit line) = splitFn content fn (line:cList) vList pList set
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

forFunction :: String -> [ ([String] , ([LeftVar],[RP])) ] -> Int -> String -> [(String, String)] -> [( [String], ([LeftVar], [RP]) )]
forFunction _ [] _ _ _ = []
forFunction run (f : fs) addr val asmT = do
  let (fnName, (iContent, vIdiom)) = detectIdiom content "" [] vlist
      (pContent, (vProp, pProp)) = propagation fnName iContent vIdiom plist []

      (vnContent, (vName, nameList)) = variableName pContent [] vProp []
      ncContent = propagateName vnContent (sort nameList)
      pcContent = precisionConversion ncContent addr val []
      -- pcContent = precisionConversion ncContent addr val []

      (eContent, vElim) = elimination pcContent vName pProp
      ssaContent = (ssaLLVM (orderingContent eContent) [] 1)
      fnContent = functionName ssaContent asmT

  case trace("-----" ++ fnName ++ "----") run of
    -- let (content, (vlist, plist)) = f
    "idiom"     -> (iContent, (vIdiom, plist)) : (forFunction run fs addr val asmT)
    "prop"      -> (pContent, (vProp, pProp)) : (forFunction run fs addr val asmT)
    "vname"     -> (ncContent, (vName, pProp)) : (forFunction run fs addr val asmT)
    "precision" -> (pcContent, (vName, pProp)) : (forFunction run fs addr val asmT)
    "elim"      -> (eContent, (vElim, pProp)) : (forFunction run fs addr val asmT)
    "SSAform"   -> (ssaContent, (vElim, pProp)) : (forFunction run fs addr val asmT)
    "fname"     -> (fnContent, (vName, pProp)) : (forFunction run fs addr val asmT)
    -- _ -> (ncContent, (vName, pProp)) : (forFunction run fs addr val asmT)
    _-> (content, (vlist, plist)) : (forFunction run fs addr val asmT)

  where (content, (vlist, plist)) = f

printRP [] = []
printRP (p:ps) =
  (show (length ps) ++ " > " ++ rname p ++ " ("++ getBase p ++ ", " ++ show (getIndex p) ++ ") " ++ getRstate p) : (printRP ps)

printLV [] = []
printLV (p:ps) =
  (show (length ps) ++ " > " ++ variable p ++ " ("++ getType p ++ ", " ++ show (getInstr p) ++ ") " ++ getState p) : (printLV ps)

printPair [] = []
printPair ((a, b):xs) = (a ++ " -> " ++ b) : (printPair xs)
{-************************************************************************
            arguments: executable or binary/object fileE
  *************************************************************************-}
main = do

  (prog_file:_) <- getArgs
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

      -- READ FILES
      contentIr <- hGetContents handleIr
      contentAsm <- hGetContents handleAsm
      contentRodata <- hGetContents handleRodata

      if (length contentIr /= 0 && length contentAsm /= 0)
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
              address = (strHexToDec.head.words.head) rodata
              values = map toUpper $ concat $ map (concat.init.tail.words) rodata

          -- Intermediate Representation
          let unnecessaryReg = reg_8 ++ reg_16 ++ ["%IP"]
              tempIR = (init.lines.fst) (strSplit str_main contentIr)
              sourceIR = remove_r unnecessaryReg tempIR
              nameIR = filter (not.null) $ map strip sourceIR
              functionIR = splitFn (reverse nameIR) "" [] [] [] []

            {-********************
                MAJOR METHODS
            *********************-}
          -- let runOption = "idiom"
              -- runOption = "prop"
              -- runOption = "vname"
              -- runOption = "precision"
              runOption = "elim"
              -- runOption = "SSAform"
              -- runOption = "fname"

              trimS = forFunction runOption functionIR address values asmTable
              trimContent = map fst trimS -- [(content)]
              trimListV = map fst $ map snd trimS -- map snd trimS
              trimListP = map snd $ map snd trimS -- map snd trimS

          -- WRITE A FILE
          hPutStr handleTmp $ unlines (map unlines trimContent)

          -- CLOSE FILES & TERMINATE
          hClose handleIr
          hClose handleAsm
          hClose handleRodata
          hClose handleTmp

          system $ "rm " ++ decir ++ " " ++ disas ++ " " ++ asmRodata
          renameFile tmpFile (prog_file ++ "Output_" ++ runOption ++ ".ll")

          -- mapM_ print (printRP plist)
          -- mapM_ print (printLV vlist)
          -- mapM_ print (printPair asmTable)
          print "ok"
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
