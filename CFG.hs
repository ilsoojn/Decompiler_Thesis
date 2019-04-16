module CFG where

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
import Data.Tree
import Data.Tuple
import Data.Typeable

import Debug.Trace
import Text.Regex.Posix

import StatementParse
import StatementInstr
import OtherFunction
import RegexFunction
import Lists

data Block = Block{preds::[String], txt::[String], succs::[String]} deriving (Ord, Eq, Show, Read)
type BasicBlock = (String, Block)

getSuccessor :: String -> [String]
getSuccessor line
  | (isVoidRet xVar || isRet xVar) = []
  | (isBranch xVar) = [label xVar]
  | (isCondBranch xVar) = [trueLabel xVar, falseLabel xVar]
  | (isIndirBranch xVar) = (labels xVar)
  | (isSwitch xVar) = (label xVar) : (map snd (table xVar))
  | otherwise = []
  where (x, (xVar, xReg)) = statement line

createBlock :: [String] -> Integer -> String -> [String] -> [String] -> [String] -> [(Integer, BasicBlock)]
createBlock (line: nextLines) i blockName predecessor txt successor
  | (isFunction line) = createBlock nextLines i (getFunctionName line) predecessor [] successor
  | (isFunctionEnd line) = [(i, (blockName, Block predecessor txt (getSuccessor $ last txt)))]
  | (isBasicBlock line || isBlockLabel line || isEntryExit line) = do

    let preBlock = (blockName, Block predecessor txt (getSuccessor $ last txt))

    let eLabel = "%" ++ takeWhile (/= ':') line
        newLabel = bool ((getBlockName line)) eLabel (isEntryExit line)
        newPreds = getBlockPreds line
    (i, preBlock) : (createBlock nextLines (i+1) newLabel newPreds [line] [])

  | otherwise = createBlock nextLines i blockName predecessor (txt ++ [line]) successor

graphEdges :: [(Integer, BasicBlock)] -> [Edge]
graphEdges table = do
  let (m, (name, block)) = head table
      blockTable = map snd table
      successor = succs block -- list of successor of current block
      nSet = trace(successor)map fromJust (map (`lookup` blockTable) successor)
      edges = (zip (replicate (length nSet) m) nSet)
  trace(edges) edges
  -- (zip (replicate (length nSet) m) nSet) : graphEdges (tail table)

createCFG :: [String] -> Graph
createCFG content = do
  let blockTable = createBlock content 0 "" [] [] []
  (buildG (0, (length blockTable)+1) graphEdges)
--
-- visitList [] str = str
-- visitList (v: vs) str = visitList vs (blockName v ++ " -> " ++ str)
--
-- notVisitList [] str = str
-- notVisitList (v: vs) str = notVisitList vs (blockName v ++ " " ++ str)
--
-- subGetNode [] _ = Nothing
-- subGetNode (n : ns) str
--   | ((blockName n) == str) = Just n
--   | otherwise = subGetNode ns str
--
-- getNode :: CFG -> String -> BasicBlock
-- getNode g str = fromJust $ subGetNode (graph g) str
--
-- subPrintGraph :: [BasicBlock] -> [String]
-- subPrintGraph [] = []
-- subPrintGraph (n : ns) = do
--   let str = (blockName n) ++ " -> " ++ "(" ++ (unwords $ suc n) ++ ")"
--   str : (subPrintGraph ns)
--
-- printGraph :: CFG -> [String]
-- printGraph g = subPrintGraph (graph g)
--
--
-- hasCyclic :: BasicBlock -> (CFG, [BasicBlock]) -> Bool
-- hasCyclic n (g, visit)
--   | trace(blockName n)(elem n visit) = True
--   -- | (or $ map (`elem` visit) nextNodes) = True
--   | (null $ suc n) = False
--   | otherwise = trace(" -> " ++ (unwords $ suc n)) or $ map (`hasCyclic` (g, n: visit)) nextNodes
--   where nextNodes = map (getNode g) (suc n)
--
-- isCyclic :: CFG -> Bool
-- isCyclic g = hasCyclic (head $ graph g) (g, [])
--
-- subGetCyclic :: CFG -> [BasicBlock] -> [BasicBlock] -> BasicBlock -> [[BasicBlock]]
-- subGetCyclic g visit cyclic n = do
--   | (elem n visit) = map (subGetCyclic g (n:visit) (n:cyclic))
--
-- getCyclic :: BasicBlock -> (CFG, [BasicBlock]) -> [BasicBlock]
-- getCyclic n (g, visit)
--   | (elem n visit) = subGetCyclic n [] [] []
--   -- | (or $ map (`elem` visit) nextNodes) = subGetCyclic (n : visit) []
--   | (null $ suc n) = []
--   | otherwise = trace(" -> " ++ (unwords $ suc n)) or $ map (`getCyclic` (g, n: visit)) nextNodes
--   where nextNodes = map (getNode g) (suc n)


-- loop :: CFG ->

-- hasCyclic :: [Node] -> [Node] -> Bool
-- hasCyclic [] visit = False
-- hasCyclic (n: ns) visit
--   | trace("-> " ++ label n)(elem n visit) = True
--   | (or $ map (`elem` visit) (next n)) = True
--   | (null $ next n) = False
--   | otherwise = do
--       let adjacent = trace("(next n) " ++ (unwords $ map label (next n))) (hasCyclic (next n) (n : visit))
--           other = trace("(  ns  )" ++ (unwords$ map label ns)) (hasCyclic ns (n : visit))
--       (adjacent || other)
-- --trace (label n ++ " :: "++ visitList visit "" ++ "\n\tNEXT: " ++ notVisitList (next n) "" ++ "\n\tNotVisit: " ++ notVisitList ns "")
-- isCyclic :: DirectGraph -> Bool
-- isCyclic g = hasCyclic (graph g) []
