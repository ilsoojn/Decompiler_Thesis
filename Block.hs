module Block where

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

-- define [linkage] [PreemptionSpecifier] [visibility] [DLLStorageClass]
--      [cconv] [ret attrs]
--      <ResultType> @<FunctionName> ([argument list])
--      [(unnamed_addr|local_unnamed_addr)] [AddrSpace] [fn Attrs]
--      [section "name"] [comdat [($name)]] [align N] [gc] [prefix Constant]
--      [prologue Constant] [personality Constant] (!name !N)* { ... }

{- FUNCTION LINE -}

--map snd $ filter (\x-> (fst x) > 0) (zip (map length $ map subForest $ scc g) (scc g))
-- data BasicBlock = BasicBlock{blockName::String, preds::[String], txt::[String], succs::[String]} deriving (Ord, Eq, Show, Read)
-- data Block = Block{preds::[String], txt::[String], succs::[String]} deriving (Ord, Eq, Show, Read)
-- data BasicBlock = (String, Block)

zipList _ [] = []
zipList [] _ = []
zipList (x:xs) (y:ys) = (x, y) : (zipList xs ys)

-- intersectList :: Eq a => [a] -> [[a]] -> [a]
-- intersectList _ [] = []
-- intersectList v (x:xs)
--   | (v == x) = x
--   | otherwise = intersectList v xs

minusList ::  (Eq a , Show a, Ord a)=> [[a]] -> [a] -> [[a]] -> [[a]]
minusList [] _ y = y
minusList (x:xs) v y
  | ((sort x) == (sort v)) = (y ++ xs) --trace((unwords $ map show x) ++" == " ++ (unwords $ map show v))
  | otherwise = minusList xs v (x : y) --trace((unwords $ map show x) ++" != " ++ (unwords $ map show v))

printBlock [] = []
printBlock (bb:bbs) = do
  let name = blockName bb
      p = preds bb
      c = txt bb
      str = (intercalate ", " p) ++ " -> " ++ name
  c ++ (printBlock bbs)

getBlock :: [(BasicBlock)] -> String -> Maybe BasicBlock
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

getSuccessor :: String -> (String, [String])
getSuccessor "" = ("", [])
getSuccessor line
  | (isVoidRet xVar || isRet xVar) = (line, [])
  | (isBranch xVar) = ("", [label xVar])
  | (isCondBranch xVar) = ("", [trueLabel xVar, falseLabel xVar])
  | (isIndirBranch xVar) = ("", labels xVar)
  | (isSwitch xVar) = ("", (label xVar) : (map snd $ table xVar))
  | otherwise = (line, [])
  where (x, (xVar, xReg)) = statement line

createBlock :: [String] -> Vertex -> String -> [String] -> [String] -> [String] -> [(Vertex, BasicBlock)]
createBlock [] _ _ _ _ _ = []
createBlock (line: nextLines) i blockName predecessor txt successor
  | (isFunction line) = createBlock nextLines i (getFunctionName line) [] [] []
  | (isFunctionEnd line) = do
    let (lastLine, strSuccessor) = getSuccessor $ last txt
    [(i, BasicBlock blockName predecessor (filter (not.null) $ init txt ++ [lastLine]) strSuccessor)]
  | (isBasicBlock line || isBlockLabel line || isEntryExit line) = do

    let (lastLine, strSuccessor) = getSuccessor (last txt)
        preBlock = BasicBlock blockName predecessor (filter (not.null) $ init txt ++ [lastLine]) strSuccessor

    let eLabel = "%" ++ takeWhile (/= ':') line
        newLabel = bool ((getBlockName line)) eLabel (isEntryExit line)
        newPreds = getBlockPreds line
    (i, preBlock) : (createBlock nextLines (i+1) newLabel newPreds [] [])

  | otherwise = createBlock nextLines i blockName predecessor (txt ++ [line]) successor

-- condToTxt :: Bool -> [Vertex] -> [Vertex]-> [(Vertex, BasicBlock)] -> [String]

loopToTxt :: Bool -> [Vertex] -> [Vertex]-> [(Vertex, BasicBlock)] -> [String]
loopToTxt _ [] _ _ = []
loopToTxt loopExist (v :vs) mOutV t
  | (loopExist == False) = do-- beginning
    let openLoop =  "do{"
    (openLoop : content) ++ (loopToTxt True vs mOutV t)

  | (elem v mOutV) = do
  -- then do -- end
    let condition ="condition"
    (content ++ ["} while( " ++ condition ++ " );"]) ++ (loopToTxt True vs mOutV t)

  |otherwise = content ++ (loopToTxt True vs mOutV t) -- middle
   where bb = fromJust (v `lookup` t)
         content = txt bb


subBlockToText :: [Vertex] -> [Vertex] -> [Vertex] -> [[Vertex]] -> [(Vertex, BasicBlock)] -> [String]
subBlockToText [] _ _ _ _ = []
subBlockToText (v : vs) outV inV cycleV t
  | (or $ map (elem v) cycleV) = do
    let loopV = head $ filter (\x -> elem v x) cycleV
        otherLoopV = minusList cycleV loopV []
        nextVs = vs \\ loopV

        loopText = loopToTxt False loopV outV t
    loopText ++ (subBlockToText nextVs outV inV otherLoopV t)

  | (elem v outV) =(content) ++ (subBlockToText vs outV inV cycleV t)-- Condiitonal Jump
  | otherwise =content ++ (subBlockToText vs outV inV cycleV t)
    where bb = fromJust (v `lookup` t)
          content = txt bb

blockToText :: Graph -> [(Vertex, BasicBlock)] -> [String]
blockToText g t = do
  let orderV = (topSort g)

      mOutE = filter (\e -> snd e > 1) (assocs $ outdegree g)
      mOutV = map fst mOutE
      mInE = filter (\e -> snd e > 1) (assocs $ indegree g)
      mInV = map fst mInE

      loopV = getCyclic g
  subBlockToText orderV mOutV mInV loopV t

{-**************************************************
              Control Flow Graph
  ************************************************** -}

getAdjacentV :: Graph -> Vertex -> [Vertex]
getAdjacentV g v = map snd $ filter (\x -> fst x == v) (edges g)

{- getKey_BlockName
    INPUT   [(BasicBlock, Vertex)] blockName
    OUTPUT  indicate vertex
-}
getKey_BlockName :: [(BasicBlock, Vertex)] -> String -> Maybe Vertex
getKey_BlockName [] _ = Nothing
getKey_BlockName (v : vs) valueName
  | (blockName b == valueName) = Just i
  | otherwise = getKey_BlockName vs valueName
    where (b, i) = v

graphEdges :: [(Vertex, BasicBlock)] -> [(Vertex, BasicBlock)] -> [Edge]
graphEdges [] _ = []
graphEdges (v: vs) table = do
    let (m, block) = v
        successor = succs block
        maybeN = map (getKey_BlockName $ map swap table) successor
        adjacentV = map fromJust (filter (not.isNothing) maybeN)
        edges = zip (replicate (length adjacentV) m) adjacentV
    edges ++ (graphEdges vs table)


createCFG :: [String] -> ( Graph, [(Vertex, BasicBlock)] )
createCFG content = do
  let blockTable = createBlock content 0 "" [] [] []
      verteces = (0, (length blockTable)-1)
      edges = (0, 1) : (graphEdges blockTable blockTable)
  (buildG verteces edges, blockTable)
--
-- condJumpV (v : vs) boolean start end
--   | (boolean == True)
--   | (v `elem` start)
--   | otherwise
--
-- getCondJump :: Graph -> [[Vertex]]
-- getCondJump g = do
--   let order = topSort g
--       startV =  map fst $ filter (\e -> snd e > 1) (assocs $ outdegree g)
--       endV = map fst $ filter (\e -> snd e > 1) (assocs $ indegree g)
--
-- subLoopCond :: (Eq a, Ord a, Show a, Read a) => Graph -> [a] -> [a] -> [a] -> Integer -> Integer -> [a] -> [a] -> [[a]] -> [[a]] -> ([[a]], [[a]])
-- subLoopCond _ [] _ _ _ _ _ _ loop jmp = (loop, jmp)
-- subLoopCond g (v : vs) vout vin opLoop opJmp loop condjmp setL setJ
--   | (elem v vout && elem v vin)
--   | (elem v vout) = do
--     case (opLoop, opJmp) of
--       -- Open If Statement Block
--       (0, 0) -> subLoopCond g vs vout vin opLoop  loop (condjmp ++ [v]) setL setJ
--       -- Open Nested-If Statement Block
--       (0, True) -> subLoopCond g vs vout vin opLoop opJmp loop (condjmp ++ [v]) setL setJ
--       _ -> do
--         let loopStart = head loop
--             loopEnd = v
--         if (path g loopEnd loopStart)
--           -- Close Loop Block
--           then subLoopCond g vs vout vin 0 opJmp [] condjmp (setL ++ (loop ++ [v])) setJ
--           -- Open either If or Nested-If Statement Block
--           else subLoopCond g vs vout vin opLoop opJmp loop (condjmp ++ [v]) setL setJ
--
--   | (elem v vin) = do
--     case (opLoop, opJmp) of
--       -- Open Loop Block
--       (0, 0) -> subLoopCond g vs vout vin 1 opJmp (loop ++ [v]) condjmp setL setJ
--       -- Open Nested-Loop Block
--       (True, 0) -> subLoopCond g vs vout vin opLoop opJmp (loop ++ [v]) condjmp setL setJ
--       _ -> do
--         let g' = transposeG g
--         if (or $ map (path g' v) vs)
--           -- Open either Loop or Nested-Loop Block
--           -- Close If
--   | otherwise =
--     where isOut = elem v vout
--           isIn = elem v vin

partitionList :: Graph -> [Vertex] -> [Vertex] -> [Vertex] -> Integer -> Integer -> [Vertex] -> ([Vertex],[Vertex])
partitionList _ [] _ _ _ _ list = ([], list)
partitionList g (v : vs) vout vin numL numJ list
  | (length list > 0 && numL == 0 && numJ == 0) = ((v:vs), list)
  | (elem v vout) = do

    case (numL, numJ) of
      -- Open Condition Jump
      (0, _) -> trace(show numL ++ " " ++ show numJ ++ " open cond at " ++ show v)partitionList g vs vout vin numL (numJ+1) (list ++ [v])
      _ -> do
        let possible_e = zip (replicate (length list) v) list
            upEdge = intersect (edges g) possible_e
            isLoopEdge = bool False True ((length upEdge) > 0)

        if (isLoopEdge)
          -- Close Loop
          then trace(show numL ++ " " ++ show numJ ++ " close loop at " ++ show v) partitionList g vs vout vin (numL-1) numJ (list ++ [v])
          -- Open Condition Jump
          else trace(show numL ++ " " ++ show numJ ++ " open cond at " ++ show v)partitionList g vs vout vin numL (numJ+1) (list ++ [v])

  | (elem v vin) = do

    case (numL, numJ) of
      -- open Loop
      (_, 0) -> trace(show numL ++ " " ++ show numJ ++ " open loop at " ++ show v)partitionList g vs vout vin (numL+1) numJ (list ++ [v])
      _ -> do
        let possible_e = zip vs (replicate (length list) v)
            upEdge = intersect (edges g) possible_e
            isLoopEdge = bool False True ((length upEdge) > 0)

        if (isLoopEdge)
          -- Open Loop
          then trace(show numL ++ " " ++ show numJ ++ " open loop at " ++ show v)partitionList g vs vout vin (numL+1) numJ (list ++ [v])
          --Close Condition Jump
          else trace(show numL ++ " " ++ show numJ ++ " close cond at " ++ show v)partitionList g (v:vs) vout vin numL (numJ-1) list
  | otherwise = partitionList g vs vout vin numL numJ (list ++ [v])

subLoopCond ::  Graph -> [Vertex] -> [Vertex] -> [Vertex] -> [[Vertex]] -> [[Vertex]]
subLoopCond g [] _ _ list = list
subLoopCond g order vout vin list = do
  let (vs, v) = partitionList g order vout vin 0 0 []
      partList = list ++ [v]
  subLoopCond g vs vout vin partList

getLoopCond :: Graph -> [[Vertex]]
getLoopCond g = subLoopCond g order vout vin []
  where order = topSort g
        vout = map fst $ filter (\e -> snd e > 1) (assocs $ outdegree g)
        vin = map fst $ filter (\e -> snd e > 1) (assocs $ indegree g)



subForestV :: Forest Vertex -> [Vertex]
subForestV [] = []
subForestV (t : ts) = (rootLabel t : (subForestV (subForest t)))

cyclicV :: Forest Vertex -> [[Vertex]]
cyclicV [] = []
cyclicV (t : ts) = reverse (rootLabel t : subForestV (subForest t)) : cyclicV ts

getCyclic :: Graph -> [[Vertex]]
getCyclic g = do
  let strongCC = scc g
      loops = map snd $ filter (\x-> (fst x) > 0) (zip (map length $ map subForest strongCC) strongCC)
  cyclicV loops

isCyclic :: Graph -> Bool
isCyclic g = bool True False (null (getCyclic g))

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
