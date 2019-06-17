module Block where

import Data.Array
import Data.Bool
import Data.Char
import Data.DeriveTH
import Data.Graph
import Data.List
import Data.List.Split hiding (split)
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

subIntersect :: (Eq a , Show a, Ord a) => [a] -> [[a]] -> [a]
subIntersect _ [] = []
subIntersect v (x:xs)
  | ((sort v) == (sort x)) = x
  | otherwise = subIntersect v xs

sub_intersectList :: (Eq a , Show a, Ord a) => [[a]] -> [[a]] -> [[a]]
sub_intersectList [] _  = []
sub_intersectList (v:vs) x = (subIntersect v x) : (sub_intersectList vs x)

intersectList :: (Eq a , Show a, Ord a) => [[a]] -> [[a]] -> [[a]]
intersectList x y = filter (not.null) (sub_intersectList x y)

minusList :: (Eq a , Show a, Ord a) => [[a]] -> [a] -> [[a]] -> [[a]]
minusList [] _ y = y
minusList (x:xs) v y
  | ((sort x) == (sort v)) = (y ++ xs) --trace((unwords $ map show x) ++" == " ++ (unwords $ map show v))
  | otherwise = minusList xs v (x : y) --trace((unwords $ map show x) ++" != " ++ (unwords $ map show v))

printBlock :: [BasicBlock] -> [String]
printBlock [] = []
printBlock (bb:bbs) = do
  let name = blockName bb
      p = preds bb
      c = txt bb
      str = (intercalate ", " p) ++ " -> " ++ name
  ((str ++ "\n-----------------\n"): (c ++ ["\n-----------------\n-----------------"])) ++ (printBlock bbs)

printVertexBlock [] = []
printVertexBlock (x:xs) = do
  let (i, b) = x
      bname = blockName b
      str = "(" ++ show i ++ ", " ++ bname ++ ")"
  str : (printVertexBlock xs)

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
  | (isVoidRet xVar) = (line, [])
  | (isRet xVar) = ("return " ++ (value xVar), [])
  | (isBranch xVar) = ("", [label xVar])
  | (isCondBranch xVar) = ("", [trueLabel xVar, falseLabel xVar])
  | (isIndirBranch xVar) = ("", labels xVar)
  | (isSwitch xVar) = ("", (label xVar) : (map snd $ table xVar))
  | otherwise = (line, [])
  where (x, (xVar, xReg)) = statement line

subCreateBlock :: [String] -> [String] -> ([String], [String]) -- (blockContent, RestOfContent)
subCreateBlock [] prevLines = (prevLines, [])
subCreateBlock (line: nextLines) prevLines = do
  if (isBasicBlock line || isBlockLabel line || isEntryExit line)
    then (prevLines, (line:nextLines))
    else subCreateBlock nextLines (prevLines ++ [line])

createBlock :: [String] -> Vertex -> [(Vertex, BasicBlock)]
createBlock [] _ = []
createBlock (line: nextLines) i
  -- | (null nextLines) = do
  --   let (lastLine, strSuccessor) = getSuccessor $ last txt
  --   [(i, BasicBlock blockName predecessor (filter (not.null) $ init txt ++ [lastLine]) strSuccessor)]

  | (isBasicBlock line || isBlockLabel line || isEntryExit line) = do
    let eLabel = "%" ++ takeWhile (/= ':') line
        blockName = bool ((getBlockName line)) eLabel (isEntryExit line)
        predecessor = getBlockPreds line

        (blockContent, nextContent) = subCreateBlock nextLines []
        (lastLine, successor) = getSuccessor (last blockContent)
        bCode = filter (not.null) (init blockContent ++ [lastLine])

        iBlock = BasicBlock blockName predecessor bCode successor (last blockContent)

    (i, iBlock) : (createBlock nextContent (i+1))
  | otherwise = createBlock nextLines i

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

condStr :: Function -> Graph -> [Vertex] -> [(Vertex, BasicBlock)] -> [String]
condStr f g [] _ = []
condStr f g (v:vs) t
  | (isNothing tmp) = (condStr f g vs t)
  | (isCondBranch info) = do --trace("c: " ++ (blockName basicB))
    let condition = cond info --trace("HERE" ++ cond info)
        tmp_v = lookupList condition (variables f)
    if (isNothing tmp_v)
      then (txt basicB) ++ (condStr f g vs t)
      else do
        let tmpCond = state (fromJust tmp_v)
            reverseCond = ["false", "^ true"]
            isConditionFalse = or $ map (`isInfixOf` tmpCond) reverseCond
            xorTrue = strip $ head (split "^ true" tmpCond)
            condStr = bool tmpCond (bool xorTrue "true" (tmpCond == "false")) (isConditionFalse)

            (open, mid, end) = ("if (" ++ condStr ++ ") {", "} else {", "}")
            ifCode = txt basicB

            tbname = trueLabel info
            fbname = falseLabel info
            trueB = getBlock (map snd t) tbname
            falseB = getBlock (map snd t) fbname

            ntrue = bool (lookup (fromJust trueB) (map swap t)) Nothing (isNothing trueB)
            nfalse = bool (lookup (fromJust falseB) (map swap t)) Nothing (isNothing falseB)

            --trace((bool (blockName $ fromJust trueB) "none" (isNothing trueB))++ " & " ++ bool (blockName $ fromJust falseB) "none" (isNothing falseB))
            splitVertext_TF = bool (break (== (fromJust nfalse)) vs) (vs, []) (isNothing nfalse)

            (vsT, vsF) = bool  splitVertext_TF ([fromJust ntrue], [fromJust nfalse]) (null vs)
            (vsTrue, vsFalse) = bool (vsT, vsF) (vsF, vsT) (isConditionFalse)
            --trace("--- IF ---\n" ++ (unlines ifCode) ++ "--- THEN ---\n" ++ (unlines $ map show vsTrue) ++ "--- ELSE ---\n" ++ (unlines $ map show vsFalse))

            thenCode = trace("AT " ++ show v ++ " :: " ++ (unwords $ map show vsT) ++ " AND " ++ (unwords $ map show vsF))concat $ subBlockToStr f g [vsTrue] t []
              --bool (otherStr vsT t) (otherStr vsF t) (isConditionFalse)
            elseCode = trace("AT " ++ show v ++ ", vs empty: " ++ (bool "True" (show $ length vs) (null vs)) ++ " => " ++ (bool "EMPTY" ("[" ++ (unwords $ map show vs)++"]") (null vs)))concat $  subBlockToStr f g [vsFalse] t []
              --bool (otherStr vsF t) (otherStr vsT t) (isConditionFalse)

        if ((not.null) elseCode) --trace((show v) ++ " T(" ++ (unwords $ map show vsTrue) ++ ") F(" ++ (unwords $ map show vsFalse) ++")")
          then ifCode ++ [open] ++ (map ("\t"++) thenCode) ++ [mid] ++ (map ("\t"++) elseCode) ++ [end]
          else ifCode ++ [open] ++ (map ("\t"++) thenCode) ++ [end]

  | otherwise = (txt basicB) ++ (condStr f g vs t)
    where tmp = lookup v t
          basicB = fromJust tmp
          terminalLine = bool (terminal basicB) "" (isNothing tmp)
          (x, (info, reg)) = statement terminalLine

loopStr :: Function -> Graph -> [Vertex]-> [(Vertex, BasicBlock)] -> [String]
loopStr f g [] _ = []
loopStr f g (v:vs) t = do
  let b = lookup v t
  if (isNothing b)
    then (loopStr f g vs t)
    else (txt $ fromJust b) ++ (loopStr f g vs t)

otherStr :: [Vertex] -> [(Vertex, BasicBlock)] -> [String]
otherStr [] _ = []
otherStr (v:vs) t = do
  let b = lookup v t
  if (isNothing b)
    then (otherStr vs t)
    else (txt $ fromJust b) ++ (otherStr vs t) --trace("o: " ++ (blockName $fromJust b))
{-
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
        vin = map fst $ filter (\e -> snd e > 1) (assocs $ indegree g)-}
subBlockToStr :: Function -> Graph -> [[Vertex]] -> [(Vertex, BasicBlock)] -> [String] -> [[String]]
subBlockToStr f g [] _ _ = []
subBlockToStr f g (v:vs) t txt-- = (otherStr v t) ++ (subBlockToStr f vs t cyc outV txt)
  | (length v == 0) = subBlockToStr f g vs t txt
  -- | (subOrder /= [v]) = (subBlockToStr f g subOrder t []) ++ (subBlockToStr f g vs t txt)
  | (elem v cyc) = (loopStr f g v t) : (subBlockToStr f g vs t txt)
  | (elem (head v) vout) = (condStr f g v t) : (subBlockToStr f g vs t txt)
  | otherwise = (otherStr v t) : (subBlockToStr f g vs t txt)
    where vout = map fst $ filter (\e -> snd e > 1) (assocs $ outdegree g)
          vin = map fst $ filter (\e -> snd e > 1) (assocs $ indegree g)
          cyc = getCyclic g
          subOrder = subLoopCond g v vout vin []

blockToStr :: Function -> Graph -> [(Vertex, BasicBlock)] -> [String]
blockToStr f g t = do
  let orderV = getLoopCond g
  --     cycleV = getCyclic g
  --     outV = map fst $ filter (\e -> snd e > 1) (assocs $ outdegree g)
  --     inV = map fst $ filter (\e -> snd e > 1) (assocs $ indegree g)
  concat (subBlockToStr f g orderV t [])

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


createCFG :: Function -> ( Graph, [(Vertex, BasicBlock)] )
createCFG fn = do
  let trimCode = filter (not.null) (code fn)
  let blockTable = createBlock trimCode 0

      verteces = (0, (length blockTable)-1)
      edges = (0, 1) : (graphEdges blockTable blockTable)
  (buildG verteces edges, blockTable)

printEdges :: [Edge] -> [String]
printEdges [] = []
printEdges (e:es) = do
  let (x, y) = e
  ("(" ++ show x ++ "," ++ show y ++ ")") : (printEdges es)


partitionList :: Graph -> [Vertex] -> [Vertex] -> [Vertex] -> Integer -> Integer -> [Vertex] -> ([Vertex],[Vertex])
partitionList _ [] _ _ _ _ list = ([], list)
partitionList g (v : vs) vout vin numL numJ list
  -- | (length list > 0 && numL == 0 && numJ == 0) = ((v:vs), list)
  | (length list == 0) = do
      if (elem v vin && isLoopEdge)
        -- Open Loop
        then partitionList g vs vout vin (numL+1) numJ (list ++ [v])
        else if (elem v vout && not isLoopEdge)
          -- Open Condition Jump
          then partitionList g vs vout vin numL (numJ+1) (list ++ [v])
          -- Nothing
          else partitionList g vs vout vin numL numJ (list ++ [v])

  | (elem v vout) = do
    if (numL == 0 || not isLoopEdge)
      -- Open Condition Jump
      then if (length list > 0 && numL == 0 && numJ == 0)
        then ((v:vs), list)
        else partitionList g vs vout vin numL (numJ+1) (list ++ [v])
      -- Close Loop
      else if (length list > 0 && numL == 1 && numJ == 0)
        then (vs, (list ++ [v]))
        else partitionList g vs vout vin (numL-1) numJ (list ++ [v])

  | (elem v vin) = do
    if (numJ == 0 || isLoopEdge)
      -- Open Loop
      then if (length list > 0 && numL == 0 && numJ == 0)
        then ((v:vs), list)
        else partitionList g vs vout vin (numL+1) numJ (list ++ [v])
      -- Close Cond
      else if (length list > 0 && numL == 0 && numJ == 1)
        then ((v:vs), list)
        else partitionList g (v:vs) vout vin numL (numJ-1) list

  | otherwise = partitionList g vs vout vin numL numJ (list ++ [v])
    where retCondition = length list > 0 && numL == 0 && numJ == 0
          e = bool (zip vs (replicate (length vs) v)) (zip (replicate (length list) v) list) (elem v vout)
          upEdge = intersect (edges g) e
          isLoopEdge = bool False True ((length upEdge) > 0)

subLoopCond ::  Graph -> [Vertex] -> [Vertex] -> [Vertex] -> [[Vertex]] -> [[Vertex]]
subLoopCond g [] _ _ list = list
subLoopCond g order vout vin list = do
  let (vs, v) = partitionList g order vout vin 0 0 []
      partList = list ++ [v]
  subLoopCond g vs vout vin partList

getLoopCond :: Graph -> [[Vertex]]
getLoopCond g = subLoopCond g order vout vin []
  where order = topSort g
        vout = trace("ORDER: " ++ (unwords $ map show order)) map fst $ filter (\e -> snd e > 1) (assocs $ outdegree g)
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
