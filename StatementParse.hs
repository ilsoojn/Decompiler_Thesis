module StatementParse where

import Data.Bool
import Data.Char
import Data.DeriveTH
import Data.List
import Data.List.Split
import Data.List.Utils  hiding (split)
import Data.Maybe
import Data.String.Utils hiding (split)
import Data.Strings
import Data.Tuple
import Data.Typeable

import Debug.Trace
import Text.Regex.Posix

import StatementInstr
import IsGetSet
import Lists
import OtherFunction

{-************************************************************************
                                  Memory
  *************************************************************************-}

-- store [atomic] [volatie] <type> <value>, <type ptr> <loc pointer>
storeStatement :: String -> STORE
storeStatement s
  | s_type = STORE atomic volatile t [(v, Undef v)] [(at, Undef at)]
  | otherwise = STORE atomic volatile t [(v, Const v)] [(at, Undef at)]
  where (vInfo: vpInfo: _) = splitOn' "," s
        (v, tmp) = popBack vInfo
        (t, options) = popBack tmp
        atomic = isInfixOf "atomic" options
        volatile = isInfixOf "volatile" options
        at = fst $ popBack vpInfo
        s_type = isInfixOf "%" v

-- <result> = <ty>, <ty>* <pointer>
-- <result> = <ty>, <ty>* <pointer> [syncscope("<target-scope>")] <ordering>
-- v = load [atomic] [volatile] <type>, <type ptr> <ptr>
--  | LOAD {fname::String, atomic::Bool, volatile::Bool, ty::String, ptr::String, alignment::Maybe String, syncscope::Maybe String, ordering::Maybe String}

loadStatement, allocaStatement:: String -> VAR
loadStatement line
  | (hasOpening loadInfo) = do
    let (ty: ptrInfo)= splitStartEndOneOf "{[<" ">]}" loadInfo
        ptr = unwords $ filter (hasAny "%@") $ splitOn' " " $ unwords ptrInfo
    LOAD atomic volatile ty ptr align syncscope order

  | otherwise = do
    let (ty: ptrInfo) = filter (not.null) (splitOneOf' " ," loadInfo)
        ptr = unwords $ filter (isPrefixOf "%") ptrInfo
    LOAD atomic volatile ty ptr align syncscope order

    where (op, info) = popFront line
          align = bool Nothing (Just $ last $ words $ unwords $ filter (isInfixOf "align") $ splitOn' "," info) (isInfixOf " align" info)
          clearInfo = join "," $ filter (not.isInfixOf "!") $ filter (not.isInfixOf "align") $ splitOn' "," info

          atomic = isInfixOf "atomic" clearInfo
          volatile = isInfixOf "volatile" clearInfo

          syncscope = getSyncscope clearInfo
          order = bool Nothing (Just $ last $ words clearInfo) (atomic)

          loadInfo = unwords $ filter (/= "volatile") $ filter (/= "atomic") (words clearInfo)

-- v = alloca [...] <type> [, <type> <num element>] [, align <alignment>] [, addrspace <addrspace>]
allocaStatement line = do
  let (infoLine, option) = strSplit "," line
      t = fst $ popBack infoLine
      inalloca = isInfixOf "inalloca" infoLine
  if (null option)
    then (Alloca inalloca t Nothing Nothing Nothing)
    else do
      let num_element = words $ snd $ strSplit t option
          alignment = words $ snd $ strSplit "align " option
          addr_space = words $ snd $ strSplit "addrspace " option
          numE = bool (Just $ head num_element) Nothing (null num_element)
          align = bool (Just $ head alignment) Nothing (null alignment)
          addrS = bool (Just $ head addr_space) Nothing (null addr_space)
      (Alloca inalloca t numE align addrS)

fenceStatement line = Fence syncscope ordering
  where syncscope = getSyncscope line
        ordering = last $ words line

cmpxchgStatement line = do
  let fail_ordering = (last.words) line               -- cmpxchg ... <succ_ordering> <fail_ordering>
      succ_ordering = (last.init.words) line
      syncscope = getSyncscope line
      weak = isInfixOf "weak" line
      volatile = isInfixOf "volatile" line
      (pInfo: cmpInfo: newInfo: _) = splitOn' "," line
      (ptype, ptr) = strSplit' "*" pInfo  -- cmpxchg ... <ty>* <pointer>
      (cmp, ty) = popBack cmpInfo    -- <ty> <cmp>
      new = (head.words) (snd $ strSplit ty newInfo)  -- <ty> <new> [syncscope(...)] <succ> <fail>
  Cmpxchg weak volatile ty ptr cmp new syncscope succ_ordering fail_ordering

atomicrmwStatement line = do
  let ordering = (last.words) line
      syncscope = getSyncscope line
      volatile = isInfixOf "volatile" line
      (operation: infoLine) = bool ((tail.words) line) ((tail.tail.words) line) volatile
      (pInfo, vInfo) = strSplit' "," (unwords infoLine)
      (ty, ptr) = strSplit' "*" pInfo
      v = (head.words) (snd $ strSplit ty vInfo)
  Atomicrmw volatile operation ty ptr v syncscope ordering

getElemIndex :: [String] -> [(Bool, (String, String))] -> [(Bool, (String, String))]
getElemIndex [] elements = elements
getElemIndex (ty:elist) e
  | (hasOpening ty) = do
    let idx = head elist
        new_list = tail elist
    getElemIndex new_list (e ++ [(inrange, (ty, idx))])

  | otherwise = do
    let current_eList = filter (not.null) $ splitOn' "," ty
        (new_type, new_idx) = popFront (head current_eList)
        new_list = bool (tail current_eList ++ elist) elist (null $ tail current_eList)
    getElemIndex new_list (e ++ [ (inrange, (new_type, new_idx)) ])
    where inrange = isInfixOf "inrange" ty
  --     tyIndex = bool x (snd $ popFront x) inrange
  --     (idx, ty) = popBack tyIndex
  -- getElemIndex xs (e ++ [(inrange, (ty, idx))])

-- <ty>, <ty>* <ptrval>{, [inrange] <ty> <idx>}*
-- <ty>, <ty>* <ptrval>{, [inrange] <ty> <idx>}*
-- <ty>, <ptr vector> <ptrval>, [inrange] <vector index type> <idx>
getelementptrStatement :: String -> VAR
getelementptrStatement line = do
  let (op, info) = popFront line
      inbound = isInfixOf "inbounds" info
      clearInfo = unwords $ filter (/= "inbounds") $ splitOn " " info

      (ty: pLine: eLine) = splitOn' "," clearInfo
      ptr = head $ filter (hasAny "%@") $ words pLine
      element = getElemIndex (splitStartEndOneOf "{<[" "]>}" $ join ", " eLine) []


  GetElemPtr inbound ty ptr element

-- v = op [operator type] <value type> <value 1>, <value 2>
-- return (op symbole types v1 v2)
binaryStatement :: String -> VAR
binaryStatement line
  | (hasOpening info) = do
    let set = splitStartEndOneOf "{[<" ">]}" info
        (v, tmp) = popLast set
        (u, tyInfo) = popLast tmp
    Binary op (toSym op) (unwords tyInfo) [u, v]

  | otherwise = do
    let set = splitOneOf' ", " info
        (v, tmp) = popLast set
        (u, tyInfo) = popLast tmp
    Binary op (toSym op) (unwords tyInfo) [u, v]
  where (op,info) = popFront line

-- v = op [operator type] <value type> <value 1>, <value 2>
-- return (op symbole types v1 v2)
bitwiseStatement :: String -> VAR
bitwiseStatement line
  | (hasOpening info) = do
    let set = splitStartEndOneOf "{[<" ">]}" info
        (v, tmp) = popLast set
        (u, tyInfo) = popLast tmp
    Bit op (toSym op) (unwords tyInfo) [u, v]

  | otherwise = do
    let set = splitOneOf' ", " info
        (v, tmp) = popLast set
        (u, tyInfo) = popLast tmp
    Bit op (toSym op) (unwords tyInfo) [u, v]
  where (op,info) = popFront line

-- v = icmp <condition> <value type> <value 1>, <value 2>
-- return (cond symbole vType v1 v2)
compareIntStatement :: String -> VAR
compareIntStatement line = Cmpi cond (toSym cond) ty [vOne, vTwo]
  where (op:cond:ty:vOne:vTwo:_) = words $ rmChar "," line

-- v = fcmp [flag] <condition> <value type> <value 1>, <value 2>
-- return (condition flags symbole vType v1 v2)
compareFloatStatement :: String -> VAR
compareFloatStatement line = Cmpf cond flag (toSym cond) ty [vOne, vTwo]
  where (tmp_one, vTwo) = strSplit'"," ((unwords.tail.words) line)
        (vOne, tmp_two) = popBack tmp_one
        (ty, tmp) = popBack tmp_two
        (cond, flagInfo) = popBack tmp
        flag = bool (Just flagInfo) Nothing (null flagInfo)

getPhiLabel :: [String] -> [(String, String)] -> [(String, String)]
getPhiLabel [] phiList = phiList
getPhiLabel (v:vs) phiList = do
  getPhiLabel vs (phiList ++ [strSplit' "," v])

phiStatement :: String -> VAR
phiStatement line = Phi ty vs
  where (ty:labelInfo) = splitOneOf' "[]" ((unwords.tail.words) line)
        labels = map strip (filter (/=",") (filter (not.null) labelInfo))
        vs = getPhiLabel labels []

selectStatement :: String -> VAR
selectStatement line
  | (isInfixOf "x" cLine) = do --vector
    let (selty: cond) = splitOneOf' "{}" cLine
        tmp1 = filter (not.null) (splitOneOf' "{<>}"line_1)
        tmp2 = filter (not.null) (splitOneOf' "{<>}"line_2)
        (ty, vOne) = bool (popFront line_1) (head tmp1, unwords $ tail tmp1) (isInfixOf "x" line_1)
        (ty2, vTwo) = bool (popFront line_2) (head tmp2, unwords $ tail tmp2) (isInfixOf "x" line_2)
    Select selty (unwords cond) ty [vOne, vTwo]
  | otherwise = do -- singlec
    let (selty, cond) = popFront cLine
        (ty, vOne) = popFront line_1
        (ty2, vTwo) = popFront line_2
    Select selty cond ty [vOne, vTwo]
  where (op, info) = popFront line
        (cLine: line_1: line_2: other) = splitOn' "," info

va_argStatement :: String -> VAR
va_argStatement line = VaArg va_list arg_list arg_ty
  where (list, arg_ty) = strSplit' "," ((unwords.tail.words) line)
        (va_list, arg_list) = strSplit "*" list

-- getClauses :: String -> [Clause] -> [Clause]
-- getClauses [] list = list
-- getClauses line list = do
--   let (ctype, cline) = popFront line
--   case ctype of
--     "catch" -> do
--       let (ty, tmp) = popFront cline
--           (cv, cline) = popFront tmp
--           cty = rmChar ",;*" ty
--       getClauses cline (list ++ [Catch cty cv])
--     "filter" -> do
--       let tmp = get_regexLine_all cline regex_array
--           (cty: cv: _) = map ("["++) (map (++"]") tmp)
--       getClauses cline (list ++ [Filter cty cv])
--     _ -> list

-- <clause> := catch <type> <value>
-- <clause> := filter <array constant type> <array constant>
getClauses :: [String] -> [Clause]
getClauses [] = []
getClauses (x:xs) = do
  let (ctype, cline) = popFront x
  case ctype of
    "catch" -> do
      let [vtype, value] = bool (splitOn' " " cline) (splitStartEndOneOf "{[<" ">]}" cline) (hasOpening cline)
      [Catch vtype value] ++ (getClauses xs)
    "filter" -> do
      let [vtype, value] = splitStartEndOneOf "{[<" ">]}" cline
      [Filter vtype value] ++ (getClauses xs)
    _ -> []

landingpadStatement :: String -> VAR
landingpadStatement line = do
  let (op, info) = popFront line
      cleanup = isInfixOf "cleanup" info
      (resultty, clause_line) = popFront (unwords $ filter (/="cleanup") $ words info)

      split_filter =  split (startsWith "filter") clause_line
      split_catch = map (split (startsWith "catch")) split_filter
      clause = getClauses (concat split_catch)
  LandingPad resultty cleanup clause
  --       (resultty:tmpLine:_) = get_regexLine_all line regex_landpad
  --       cleanup = isInfixOf "cleanup" tmpLine
  --       new_tmpLine = bool tmpLine (rmStr "cleanup" tmpLine) cleanup
  --       clause = getClauses new_tmpLine []

catchpadStatement :: String -> VAR
catchpadStatement line = do
  let tmp = unwords $ tail $ tail $ words $ line
      (catchswitch: arg: _) = get_regexLine_all tmp regex_pad
  CatchPad catchswitch arg

cleanuppadStatement :: String -> VAR
cleanuppadStatement line = do
  let tmp = unwords $ tail $ tail $ words $ line
      (parent: arg: _) = get_regexLine_all tmp regex_pad
  CleanUpPad parent arg

conversionStatement :: String -> VAR
conversionStatement line = Conv conv_op ty1 ty2 r
  where (conv_op, conv_s) = popFront line
        (tmp:ty2:_) = map strip $ splitOn' "to" conv_s
        -- (ty1, r) = bool (popFront tmp) (sBreak' " <" tmp) (isInfixOf "<" tmp)
        (r, ty1) = popBack tmp

retStatement :: String -> VAR
retStatement line
  | (line == "ret void") = RetVoid
  | (hasOpening info) = do
    let [ty, v] = map strip (split (startsWithOneOf "{<") info)
    (Ret ty v)
  | otherwise = do
    let (ty:v:_) = words info
    (Ret ty v)
  where (op, info) = popFront line

brStatement :: String -> VAR
brStatement line
  | (brType == "i1") = do
    let (cond: ifture: iffalse: _) = splitOn' "," info
    CondBranch cond (getLabel ifture) (getLabel iffalse)
  | otherwise = Branch info
  where (op, tmp) = popFront line
        (brType, info) = popFront tmp

switchInfo :: [String] -> [(String, String)]
switchInfo [] = []
switchInfo (x:xs) = do
  let (v, label) = strSplit' "," x  -- <value> || label <dest>
  (v, getLabel label): (switchInfo xs)

-- switch <intty> <value>, label <defaultdest> [ <intty> <val>, label <dest> ... ]
switchStatement :: String -> VAR
switchStatement line = do
  let (op, info) = popFront line                              -- switch || <intty>....
      (defaultInfo: listInfo) = filter (not.null) (splitOneOf' "[]" info)

      --default
      (type_value, label) = strSplit' "," defaultInfo
      (ty, v)  = popFront type_value

      --list
      table = bool (switchInfo $ filter (not.null) (splitOn' ty $ unwords listInfo)) [] (null listInfo)

  Switch ty v (getLabel label) table

list_of_Label :: [String] -> [String] -> [String]
list_of_Label [] list = list
list_of_Label (x:xs) list = list_of_Label xs (list ++ [getLabel x])

indirectbrStatement :: String -> VAR
indirectbrStatement line = do
  let (op, info) = popFront line
      (addressInfo, labelInfo) = strSplit' "," info         -- <ty>* <address> || [...]
      (ty, addr) = strSplit' "*" addressInfo
      dstInfo = splitOn' "," (get_regexLine labelInfo "\\[(.*)\\]")
      labels = list_of_Label dstInfo []
  IndirBranch ty addr labels

resumeStatement :: String -> VAR
resumeStatement line = do
  let (op, info) = popFront line
      (v, ty) = popBack info
  Resume ty v

catchswitchStatement :: String -> VAR
catchswitchStatement line = do
  let (pInfo: hInfo: unwindInfo: _) = splitOneOf' "[]" line
      parent = (last.words) pInfo
      handlers = list_of_Label (splitOn' "," hInfo) []
      unwindLabel = (last.words) unwindInfo
  CatchSwitch parent handlers unwindLabel

catchretStatement :: String -> VAR
catchretStatement line = do
  let (op, info) = strSplit' " from " line
      (token, label) = strSplit' " to " info
  CatchRet token (getLabel label)

cleanupretStatement :: String -> VAR
cleanupretStatement line = do
  let (op, info) = strSplit' " from " line
      (value, labelInfo) = strSplit' " unwind " info
      unwind = (last.words) labelInfo
  CleanUpRet value unwind

parseStatement :: String -> (VAR, [String])
parseStatement line
  | (opty == "binary") = do
    let s = binaryStatement line
    (s, sort (values s))
  | (opty == "bitwise") = do
    let s = bitwiseStatement line
    (s, sort (values s))
  | (opty == "conversion") = do
    let s = conversionStatement line
    (s, [value s])
  | otherwise = do
    case op of
      --terminator
      "ret" -> do
        let s = retStatement line
        if (isVoidRet s) then (s, []) else (s, [value s])

      "br" -> do
        let s = brStatement line
        if (isBranch s) then (s, [label s]) else (s, [cond s, trueLabel s, falseLabel s])

      "indirectbr" -> do
        let s = indirectbrStatement line
            r = [addr s] ++ (labels s)
        (s, r)

      "switch" -> do
        let s = switchStatement line
            r = [value s, label s] ++ (map snd $ table s)
        (s, r)

      "resume" -> do
        let s = resumeStatement line
        (s, [value s])

      "catchswitch" -> do
        let s = catchswitchStatement line
            r = [parent s] ++ (handlers s) ++ [unwind s]
        (s, r)

      "catchret" -> do
        let s = catchretStatement line
        (s, [token s, normal s])

      "cleanupret" -> do
        let s = cleanupretStatement line
        (s, [token s, normal s])

      "unreachable" -> (Unreachable, [])

      -- memory
      "load" -> do
        let s = loadStatement line
        (s, [ptr s])

      "alloca" -> (allocaStatement line, [])

      "fence" -> (fenceStatement line, [])

      "cmpxchg" -> do
        let s = cmpxchgStatement line
        (s, [ptr s, cmp s, new s])

      "atomicrmw" -> do
        let s = atomicrmwStatement line
        (s, [ptr s, value s])

      "getelementptr" -> do
        let s = getelementptrStatement line
        (s, [ptr s])

      -- other
      "icmp" -> do
        let s = compareIntStatement line
        (s, sort (values s))

      "fcmp" -> do
        let s = compareFloatStatement line
        (s, sort (values s))

      "phi" -> do
        let s = phiStatement line
            r = (map fst (vlabels s)) ++ (map snd (vlabels s))
        (s, r)

      "select" -> do
        let s = selectStatement line
            r = [cond s] ++ (values s)
        (s, r)

      "va_arg" -> do
        let s = va_argStatement line
        (s, [arglist s])

      "landingpad" -> do
        let s = landingpadStatement line
            r = [resultty s] ++ map cvalue (clause s)
        (s, r)

      "catchpad" -> do
        let s = catchpadStatement line
        (s, [catchswitch s])

      "cleanuppad" -> do
        let s = cleanuppadStatement line
        (s, [parent s])

      _ -> do
        if (isSemiColon line)
          then do
            let (v1:v2:_) = getHighLowVariable line
                hs = '%':v1
                ls = '%':v2
            (SemiColon hs ls, [hs, ls])
          else (Other, [])
  where op = head $ words line
        opty = getInstructionType op

statement :: String -> (Maybe String, (VAR, [String]))
statement line = do
  let (lhs, rhs) = (strSplit " = " line)
  if (null rhs)
    then (Nothing, parseStatement lhs)
    else (Just lhs, parseStatement rhs)

{- LeftSideVar List -}

addVariable :: String -> String -> String -> String -> [LeftVar] -> [LeftVar]
addVariable v t i s varList = varList ++ [(LeftVar v t i s)]

removeVariable :: LeftVar -> [LeftVar] -> [LeftVar] -> [LeftVar]
removeVariable vInfo [] px = px
removeVariable vInfo (x:xs) px
  | (variable vInfo == variable x) = (px ++ xs) -- found matching info
  | otherwise = removeVariable vInfo xs (px ++ [x])

lookupList :: String -> [LeftVar] -> Maybe LeftVar
lookupList v [] = Nothing
lookupList v (x:xs)
  | (v == variable x) = Just x
  | otherwise = lookupList v xs

updateList :: LeftVar -> [LeftVar] -> [LeftVar] -> [LeftVar]
updateList vInfo [] px = px
updateList vInfo (x:xs) px
  | (variable vInfo == variable x) = (px ++ [vInfo] ++ xs) -- found matching info
  | otherwise = updateList vInfo xs (px ++ [x])

setType, setInstr, setState :: String -> LeftVar -> LeftVar
setType new_type x =  x { vtype=new_type }
setInstr new_instr x = x { instruction=new_instr }
setState new_state x = x { state=new_state }

getType, getInstr, getState :: LeftVar -> String
getType x = fromJust $ Just (vtype x)
getInstr x = fromJust $ Just (instruction x)
getState x = fromJust $ Just (state x)


{- RP List -}

addPointer :: String -> String -> String -> String -> [RP] -> [RP]
addPointer ptr base idx ptrList = ptrList ++ [(RP ptr base idx)]

removePointer :: RP -> [RP] -> [RP] -> [RP]
removePointer pInfo [] px = px
removePointer pInfo (x:xs) px
  | (rname pInfo == rname x) = (px ++ xs) -- found matching info
  | otherwise = removeVariable pInfo xs (px ++ [x])

lookupList_p :: String -> [RP] -> Maybe RP
lookupList_p p [] = Nothing
lookupList_p p (x:xs)
  | (p == rname x) = Just x
  | otherwise = lookupList_p p xs

updateList_p :: RP -> [RP] -> [RP] -> [RP]
updateList_p pInfo [] px = px
updateList_p pInfo (x:xs) px
  | (rname pInfo == rname x) = (px ++ [pInfo] ++ xs) -- found matching info
  | otherwise = updateList_p pInfo xs (px ++ [x])

setName, setBase, setIndex :: String -> RP -> RP
setName name x =  x { rname=name }
setBase base x = x { rbase=base }
setIndex idx x = x { ridx=idx }

getName, getBase, getIndex :: LeftVar -> RP
getName x = fromJust $ Just (rname x)
getBase x = fromJust $ Just (rbase x)
getIndex x = fromJust $ Just (ridx x)

-- setType_List v t (x:xs) px
--   | (v == variable x) = do
--       x' = x { vtype=t }
--
--   | otherwise = setType v t xs (px ++ [x])
--
-- getType v [] = Nothing
-- getType v (x:xs)
--   | (v == variable x) = Just (vtype x)
--   | otherwise = getType_v v xs
--
-- getInstr v [] = Nothing
-- getInstr v (x:xs)
--   | (v == variable x) = Just (instruction x)
--   | otherwise = getInstr v xs
--
-- getState v [] = Nothing
-- getState v (x:xs)
--   | (v == variable x) = Just (state x)
--   | otherwise = getState v xs

{-************** Variables **************-}
--
-- addVariable :: String -> String -> [(String, String)] -> [(String, String)]
-- addVariable v t varList = varList ++ (v, t)
--
-- removeVariable :: String -> [(String, String)] -> [(String, String)]
-- removeVariable v varList = filter (/= (v, getType v varList)) varList
--
-- getType :: String -> [(String, String)] -> String
-- getType v varList = bool (fromJust t) "none" (isNothing t)
--   where t = (lookup v varList)

variableType :: VAR -> String
variableType var
  | (isVaArg var) = (argty var)
  | (isCatchPad var) || (isCatchSwitch var) || (isCleanUpPad var) = "token"
  | otherwise = do
    if ((isAlloca var) || (isBinary var) || (isBitwise var) || (isCmpf var) || (isCmpi var) || (isConv var) || (isGetElemPtr var) || (isLoad var) || (isPhi var) || (isSelect var)) --(isInvoke var) ||
      then (ty var)
      else "none"

-- detectVariable :: [String] -> String -> [(String, String)] -> [(String, String)]
-- detectVariable [] fname varSet = varSet
-- detectVariable (line:nextCont) fname varSet
--   | (isLHS line fname) = do
--     let (v, (var, reg)) = statement line fname
--         newList = addVariable (fromJust v) (variableType var) varSet
--     detectVariable nextCont fname newList
--   | otherwise = detectVariable nextCont fname varSet

{-************************************************************************
                              Def and Use
  *************************************************************************-}

findDef :: String -> [LeftVar] -> String
findDef v vList
  | (not.isNothing) vInfo = getState (fromJust vInfo)
  | otherwise = ""
  where vInfo = lookupList v vList

-- value content fname line_number recursive_list
-- -> (line_number, (used variable info))
findUse :: String -> [String] -> [String]
findUse v [] = []
findUse v (line:nextCont)
  | (isFunctionEnd line) = []
  | (isUse v tmp) = line : filter (not.null) (findUse v nextCont)
  | otherwise = "" : filter (not.null) (findUse v nextCont)
    where tmp = last $ splitOn' " = " line


-- findDef :: String -> Integer -> (Integer, VAR) -- line_number and Operator Type
-- findDef cLine line_num = (line_num, parseStatement cLine)
--
-- chainDefUse :: [String] -> Integer -> [Chain] -> [Chain]
-- chainDefUse [] line_num chainList = chainList
-- chainDefUse (line:content)  line_num chainList = do
--   if isInfixOf " = " $ fst $ strSplit ";" line
--     then do
--       let (v, cLine) = strSplit'" = " line
--           def = findDef cLine line_num
--           use = findUse v content (line_num + 1) []
--       chainDefUse content (line_num + 1) $ chainList ++ [Chain v def use]
--     else chainDefUse content (line_num + 1) chainList
