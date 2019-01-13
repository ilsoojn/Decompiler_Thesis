module Block where

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

data BasicBlock = BasicBlock{blockName::String, preds::[String], bcontent::[String]} deriving (Ord, Eq, Show, Read)

printBlock [] = []
printBlock (bb:bbs) = do
  let name = blockName bb
      p = preds bb
      c = bcontent bb
      str = (intercalate ", " p) ++ " -> " ++ name
  c ++ (printBlock bbs)

getBlock :: [BasicBlock] -> String -> Maybe BasicBlock
getBlock [] _ = Nothing
getBlock (b:bs) name = do
  if (isInfixOf name (blockName b))
    then Just b
    else getBlock bs name

removeBlock :: [BasicBlock] -> [BasicBlock] -> BasicBlock -> [BasicBlock]
removeBlock (b:nextB) preB x = do
  if (blockName b == blockName x)
    then (preB ++ nextB)
    else removeBlock nextB (preB ++ [b]) x

contentToBlock :: [String] -> String -> [String] -> [String] -> [BasicBlock]
contentToBlock (line : nextContent) label predecessor content
  | (isFunctionEnd line) = [BasicBlock label predecessor content]
  | (isFunction line) = contentToBlock nextContent (getFunctionName line) predecessor []
  | (isBasicBlock line || isBlockLabel line || isEntryExit line) = do

      let prevBasicBlock = (BasicBlock label predecessor content)

      let eLabel = "%" ++ takeWhile (/= ':') line-- (bool ("entry_") ("exit_") (isPrefixOf "exit" line))
          newLabel = bool ((getBlockName line)) eLabel (isEntryExit line)
          newPreds = getBlockPreds line
      prevBasicBlock : (contentToBlock nextContent newLabel newPreds [line])

  | otherwise = contentToBlock nextContent label predecessor (content ++ [line])

getDefVariables, multiDef :: [String] -> [String]
getDefVariables [] = []
getDefVariables (x:xs) = do
  if (isInfixOf " = " x)
    then (fst $ strSplit' " = " x) : getDefVariables xs
    else getDefVariables xs

multiDef [] = []
multiDef (x:xs)
  | (isInfixOf " = " x) = do
    let v = fst $ strSplit' "=" x
        vs = getDefVariables xs
    if (elem v vs)
      then multiDef xs
      else x : multiDef xs
  | otherwise = x : multiDef xs

orderingBlock :: [BasicBlock] -> [BasicBlock]
orderingBlock blocks = do
  let b = sort blocks
      funcB = fromJust $ getBlock b "@fn"
      exitB = fromJust $ getBlock b "exit"
      entryB = fromJust $ getBlock b "entry"

      (entryBegin: entryC) = init (bcontent entryB)
      entryContent =  multiDef (entryC ++ reverse (bcontent funcB))
      entryLast =last (bcontent entryB)

      newContent =[entryBegin]++ entryContent ++ [entryLast]

      newEntryB = funcB {bcontent=newContent}
      blist = removeBlock (removeBlock (removeBlock b [] funcB) [] exitB) [] entryB
  (newEntryB : blist ++ [exitB])

blockToContent :: [BasicBlock] -> [String]
blockToContent [] = []
blockToContent (b : bs) = (bcontent b) ++ blockToContent bs

orderingContent :: [String] -> [String]
orderingContent content = do
  let fnEntry = filter (isFunction) content
      fnExit = "}"
      b = contentToBlock content "" [] []
      b' = orderingBlock b
      c = blockToContent b'
  (fnEntry ++ c ++ [fnExit])

-- content block is in ORD
ssaLLVM :: [String] -> [String] -> Integer -> [String]
ssaLLVM [] preContent _ = preContent
ssaLLVM (line : nextContent) preContent count
  | (isFunction line || isFunctionEnd line) = ssaLLVM nextContent (preContent ++ [line]) count
  | (isBasicBlock line || isBlockLabel line || isEntryExit line) = do
    let eLabel = "%" ++ takeWhile (/= ':') line-- (bool ("entry_") ("exit_") (isPrefixOf "exit" line))
        label = bool ((getBlockName line)) eLabel (isEntryExit line)
        newPre = replace' label newV preContent
        newline = "; <label>:" ++ show count ++ ":"
        newNext = replace' label newV nextContent
    ssaLLVM newNext (newPre ++ [newline]) (count + 1)
  | otherwise = do
    if (isLHS line "temporary")
      then do
        let v = fst $ strSplit' "=" line
            newPre = replace' v newV preContent
            newline = replace v newV line
            newNext = replace' v newV nextContent
        ssaLLVM newNext (newPre ++ [newline]) (count + 1)
      else ssaLLVM nextContent (preContent ++ [line]) count
    where newV = ("%" ++ show count)