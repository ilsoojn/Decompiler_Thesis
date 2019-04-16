module HLLgenerator where

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

import StatementParse
import StatementInstr
import OtherFunction
import RegexFunction
import Lists
import Block

{-
BasicBlock{blockName::String, preds::[String], bcontent::[String]}

orderingContent :: [String] -> [String]
orderingContent content = do
  let fnEntry = filter (isFunction) content
      fnExit = "}"
      b = contentToBlock content "" [] []
      b' = orderingBlock b
      c = blockToContent b'
  (fnEntry ++ c ++ [fnExit])

blockToContent :: [BasicBlock] -> [String]
blockToContent [] = []
blockToContent (b : bs) = (bcontent b) ++ blockToContent bs
-}
-- controlFlowConversion :: [BasicBlock] -> String -> [String] -> [String]
-- controlFlowConversion [] function_starting_Address code = code
-- controlFlowConversion (b : bs) addr code = do
--   let block_label = blockName b
--       predecessor = preds b
--       successorLine = last (txt b)
--       content = trace(block_label ++ " >> " ++ (snd $ strSplit str_bb block_label) ++ " & " ++ addr) init (tail (txt b))
--
--   if (isInfixOf "entry_fn_" block_label || isInfixOf "exit_fn_" block_label) -- entry or exit Block
--     then (controlFlowConversion bs addr (code ++ content))
--   else if ((snd $ strSplit str_bb block_label) == addr)  -- initial Block
--     then (controlFlowConversion bs addr (code ++ content))
--   else do
--     let loopBlock = filter (> block_label) predecessor
--     if (length loopBlock > 0) -- LOOP
--       then (controlFlowConversion bs addr (code ++ content))-----
--     else (controlFlowConversion bs addr (code ++ content))----
--
-- cfgContent :: [String] -> [String]
-- cfgContent content = do
--   let fnEntry = filter (isFunction) content
--       fnExit = "}"
--       b = contentToBlock content "" [] []
--       b' = orderingBlock b
--       c = controlFlowConversion b' (getFunctionAddr $ head fnEntry) []
--   (fnEntry ++ c ++ [fnExit])

irConversion :: Maybe String -> VAR -> [String] -> String -> String
irConversion x xInfo xReg line
  | (isAlloca xInfo) = do
    let variableName = (fromJust x)

    case (ty xInfo) of
      "i16" -> "short " ++ variableName
      "i32" -> "int " ++ variableName
      "i64" -> "long " ++ variableName
      _ -> (ty xInfo) ++ " " ++ variableName

  | (isStore xInfo) = (at xInfo) ++ " = " ++ (value xInfo)
  | (isBinary xInfo || isBitwise xInfo) = unwords [(fromJust x),"=", (head xReg), (sym xInfo), (last xReg)]
  | (isRet xInfo) = "return " ++ (value xInfo)
  | (isVoidRet xInfo) = "return void"
  | (isCmpi xInfo || isCmpf xInfo) = do
    case (cond xInfo) of
      "ord"   -> (fromJust x) ++ " = " ++ (bool "false" "true" (not ("QNAN" `elem` xReg)))
      "uno"   -> (fromJust x) ++ " = " ++ (bool "false" "true" ("QNAN" `elem` xReg))
      "false" -> (fromJust x) ++ "= false"
      "true"  -> (fromJust x) ++ "= true"
      _ -> unwords [(fromJust x), "=", (head xReg), (sym xInfo), (last xReg)]
  | otherwise = line

propagateLoad :: [String] -> [String]
propagateLoad [] = []
propagateLoad (line : nextContents) = do
  let (x, (xInfo, xReg)) = statement line
  if (isLoad xInfo)
    then (propagateLoad (replace' (fromJust x) (ptr xInfo) nextContents))
    else line : (propagateLoad nextContents)

removeSSA_variables :: [String] -> [String]
removeSSA_variables [] = []
removeSSA_variables (line : nextContents) = do
  let (x, (xInfo, xReg)) = statement line
  if (isJust x && (not.null) (filter isDigit $ fromJust x))
    then do
      let v = strip $ fromJust x
          rhs = strip $ snd $ strSplit "=" line
      trace(v ++ " -> " ++ rhs)(removeSSA_variables (replace' v rhs nextContents))
    else line : (removeSSA_variables nextContents)

fromIRtoHLL :: Function -> [String] -> [String] -> Function
fromIRtoHLL f [] newCode = f{code = newCode}
fromIRtoHLL f (line : nextTxt) oldTxt = do
  let (x, (var, reg)) = statement line
      newLine = irConversion x var reg line

  if (isNothing x && (not.null) newLine)
    then fromIRtoHLL f nextTxt (oldTxt ++ [newLine])
    else do
      let tmpV = lookupList (fromJust x) (variables f)

      if (isNothing tmpV)
        then fromIRtoHLL f nextTxt (oldTxt ++ [newLine])
        else do
          let v = fromJust tmpV
              rhs = strip $ snd (strSplit "=" newLine)
              newList = updateList (setState rhs v) (variables f) []

          fromIRtoHLL (f{variables = newList}) nextTxt (oldTxt ++ [newLine])


generateHLL :: Function -> Maybe String -> Function -- contents -> newContents
generateHLL f funcName= do
  let tmpIR = propagateLoad (code f)
      f' = fromIRtoHLL f tmpIR []

      (cfg, blockTable) = createCFG f'
      tmp = removeSSA_variables$ map ("\t" ++) (blockToStr f' cfg blockTable)
  -- let (cfg, blockTable) =  createCFG f
  --     tmp = removeSSA_variables $ fromIRtoHLL $ propagateLoad $ blockToText cfg blockTable
      new_fname = bool (fromJust funcName) (fname f) (isNothing funcName)

      startFn = unwords [(retType f), new_fname, "(", unwords (args f), ") {"]
      newCode = (startFn : tmp) ++ ["}"]
  (f' { code = newCode })
