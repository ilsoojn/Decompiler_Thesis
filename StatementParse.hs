module StatementParse where

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
import StatementInstr
import OtherFunction

{-************************************************************************
                                  Memory
  *************************************************************************-}

-- store [atomic] [volatie] <type> <value>, <type ptr> <loc pointer>
storeStatement :: String -> STORE
storeStatement s
  | s_type = STORE atomic volatile t [(v, Undef v)] [(at, Undef at)]
  | otherwise = STORE atomic volatile t [(v, Const v)] [(at, Undef at)]
  where (vInfo: vpInfo: etc) = splitOn' "," s
        (v, tmp) = popBack vInfo
        (t, options) = popBack tmp
        atomic = isInfixOf "atomic" options
        volatile = isInfixOf "volatile" options
        at = fst $ popBack vpInfo
        s_type = isInfixOf "%" v

-- v = load [atomic] [volatile] <type>, <type ptr> <ptr>
loadStatement, allocaStatement:: String -> String -> VAR
loadStatement line fname = LOAD fname atomic volatile t ptr
  where (fstLine: sndLine: option) = filter (not.null) (splitOn' "," line)
        t = fst $ popBack fstLine
        (ptr, etc) = popFront $ snd $ popFront sndLine
        atomic = isInfixOf "atomic" fstLine
        volatile = isInfixOf "volatile" fstLine

-- v = alloca [...] <type> [, <type> <num element>] [, align <alignment>] [, addrspace <addrspace>]
allocaStatement line fname = do
  let (infoLine, option) = strSplit "," line
      t = fst $ popBack infoLine
      inalloca = isInfixOf "inalloca" infoLine
  if (null option)
    then (Alloca fname inalloca t Nothing Nothing Nothing)
    else do
      let num_element = words $ snd $ strSplit t option
          alignment = words $ snd $ strSplit "align " option
          addr_space = words $ snd $ strSplit "addrspace " option
          numE = bool (Just $ head num_element) Nothing (null num_element)
          align = bool (Just $ head alignment) Nothing (null alignment)
          addrS = bool (Just $ head addr_space) Nothing (null addr_space)
      (Alloca fname inalloca t numE align addrS)

fenceStatement line fname = Fence fname syncscope ordering
  where syncscope = bool Nothing (Just $ get_RegexMatch regex_sync line) (isInfixOf "syncscope" line)
        ordering = last $ words line

cmpxchgStatement line fname = do
  let fail_ordering = (last.words) line               -- cmpxchg ... <succ_ordering> <fail_ordering>
      succ_ordering = (last.init.words) line
      syncscope = bool Nothing (Just $ get_RegexMatch regex_sync line) (isInfixOf "syncscope" line)
      weak = isInfixOf "weak" line
      volatile = isInfixOf "volatile" line
      (pInfo: cmpInfo: newInfo: etc) = splitOn' "," line
      (ptype, ptr) = strSplit' "*" pInfo  -- cmpxchg ... <ty>* <pointer>
      (cmp, ty) = popBack cmpInfo    -- <ty> <cmp>
      new = (head.words) (snd $ strSplit ty newInfo)  -- <ty> <new> [syncscope(...)] <succ> <fail>
  Cmpxchg fname weak volatile ty ptr cmp new syncscope succ_ordering fail_ordering

atomicrmwStatement line fname = do
  let ordering = (last.words) line
      syncscope = bool Nothing (Just $ get_RegexMatch regex_sync line) (isInfixOf "syncscope" line)
      volatile = isInfixOf "volatile" line
      (operation: infoLine) = bool ((tail.words) line) ((tail.tail.words) line) volatile
      (pInfo, vInfo) = strSplit' "," (unwords infoLine)
      (ty, ptr) = strSplit' "*" pInfo
      v = (head.words) (snd $ strSplit ty vInfo)
  Atomicrmw fname volatile operation ty ptr v syncscope ordering

getElemIndex :: [String] -> [(Bool, String, String)] -> [(Bool, String, String)]
getElemIndex [] elements = elements
getElemIndex (x:xs) e = do
  let inrange = isInfixOf "inrange" x
      tyIndex = bool x (snd $ popFront x) inrange
      (idx, ty) = popBack tyIndex
  getElemIndex xs (e ++ [(inrange, ty, idx)])

getelementptrStatement line fname = do
  let (tyLine: pLine: eLine) = splitOn' "," ((unwords.tail.words) line)
      inbound = isInfixOf "inbounds" tyLine
      ty = bool tyLine (last $ words tyLine) inbound
      ptr = snd $ strSplit' "*" pLine
      element = getElemIndex eLine []
  GetElemPtr fname inbound ty ptr element

binaryStatement :: String -> VAR
-- v = op [operator type] <value type> <value 1>, <value 2>
-- return (op symbole types v1 v2)
binaryStatement line = Binary op (toSym op) tyInfo [vOne, vTwo]
  where (op,info) = popFront line
        (tmp, vTwo) = strSplit'"," info
        (vOne, tyInfo) = popBack tmp

-- v = op [operator type] <value type> <value 1>, <value 2>
-- return (op symbole types v1 v2)
bitwiseStatement :: String -> VAR
bitwiseStatement line = Bit op (toSym op) tyInfo [vOne, vTwo]
  where (op,info) = popFront line
        (tmp, vTwo) = strSplit'"," info
        (vOne, tyInfo) = popBack tmp

-- v = icmp <condition> <value type> <value 1>, <value 2>
-- return (cond symbole vType v1 v2)
compareIntStatement :: String -> VAR
compareIntStatement line = Cmpi cond (toSym cond) ty [vOne, vTwo]
  where (cond:ty:vOne:vTwo:etc) = words $ rmChar "," line

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

getClauses :: String -> [Clause] -> [Clause]
getClauses [] list = list
getClauses line list = do
  let (ctype, cline) = popFront line
  case ctype of
    "catch" -> do
      let (ty, tmp) = popFront cline
          (cv, cline) = popFront tmp
          cty = rmChar ",;*" ty
      getClauses cline (list ++ [Catch cty cv])
    "filter" -> do
      let tmp = get_allRegexMatch regex_array cline
          (cty: cv: etc) = map ("["++) (map (++"]") tmp)
      getClauses cline (list ++ [Filter cty cv])
    _ -> list

landingpadStatement :: String -> VAR
landingpadStatement line = LandingPad resultty cleanup clause
  where (resultty:tmpLine:etc) = get_allRegexMatch regex_landpad line
        cleanup = isInfixOf "cleanup" tmpLine
        new_tmpLine = bool tmpLine (rmStr "cleanup" tmpLine) cleanup
        clause = getClauses new_tmpLine []

catchpadStatement :: String -> VAR
catchpadStatement line = do
  let tmp = unwords $ tail $ tail $ words $ line
      (catchswitch: arg: etc) = get_allRegexMatch regex_pad tmp
  CatchPad catchswitch arg

cleanuppadStatement :: String -> VAR
cleanuppadStatement line = do
  let tmp = unwords $ tail $ tail $ words $ line
      (parent: arg: etc) = get_allRegexMatch regex_pad tmp
  CleanUpPad parent arg

conversionStatement :: String -> VAR
conversionStatement line = Conv conv_op ty1 ty2 r
  where (conv_op, conv_s) = popFront line
        (tmp:ty2:etc) = map strip $ splitOn' "to" conv_s
        -- (ty1, r) = bool (popFront tmp) (sBreak' " <" tmp) (isInfixOf "<" tmp)
        (r, ty1) = popBack tmp

retStatement :: String -> VAR
retStatement line
  | (line == "ret void") = RetVoid
  | (isInfixOf " <" infoLine) = do
    let (ty, v) = sBreak' " <" infoLine
    (Ret ty v)
  | (isInfixOf " {" infoLine) = do
    let (ty, v) = sBreak' " <" infoLine
    (Ret ty v)
  | (isInfixOf " [" infoLine) = do
    let (ty, v) = sBreak' " <" infoLine
    (Ret ty v)
  | otherwise = do
    let (ty:v:etc) = words infoLine
    (Ret ty v)
  where (op, infoLine) = popFront line

brStatement :: String -> VAR
brStatement line
  | (brType == "i1") = do
    let (cond: ifture: iffalse: etc) = splitOn' "," info
    CondBranch cond (getLabel ifture) (getLabel iffalse)
  | otherwise = Branch info
  where (op, tmp) = popFront line
        (brType, info) = popFront tmp

switchInfo :: [String] -> [(String, String)] -> [(String, String)]
switchInfo [] table = table
switchInfo (x:xs) table = do
  let (v, dstInfo) = strSplit' "," x  -- <value> || label <dest>
      label = getLabel dstInfo
  switchInfo xs (table ++ [(v, label)])

-- switch <intty> <value>, label <defaultdest> [ <intty> <val>, label <dest> ... ]
switchStatement :: String -> VAR
switchStatement line = do
  let (op, info) = popFront line                              -- switch || <intty>....
      (defaultInfo, otherInfo) = sBreak' "[" info              -- <intty> <value>, label <defaultdest> || [...]
      (value, label) = strSplit' "," defaultInfo  -- <intty> <value> || label <defaultdest>
      (ty, v) = popFront value                                -- <intty> || <value>                                 -- <defaultdest>
      dstInfo = splitOn' ty (get_RegexMatch "\\[(.*)\\]" otherInfo)
      table = switchInfo dstInfo []
  Switch ty v (getLabel label) table

list_of_Label :: [String] -> [String] -> [String]
list_of_Label [] list = list
list_of_Label (x:xs) list = list_of_Label xs (list ++ [getLabel x])

indirectbrStatement :: String -> VAR
indirectbrStatement line = do
  let (op, info) = popFront line
      (addressInfo, labelInfo) = strSplit' "," info         -- <ty>* <address> || [...]
      (ty, addr) = strSplit' "*" addressInfo
      dstInfo = splitOn' "," (get_RegexMatch "\\[(.*)\\]" labelInfo)
      labels = list_of_Label dstInfo []
  IndirBranch ty addr labels

resumeStatement :: String -> VAR
resumeStatement line = do
  let (op, info) = popFront line
      (v, ty) = popBack info
  Resume ty v

catchswitchStatement :: String -> VAR
catchswitchStatement line = do
  let (pInfo: hInfo: unwindInfo: etc) = splitOneOf' "[]" line
      parent = (last.words) pInfo
      handlers = list_of_Label (splitOn' "," hInfo) []
      unwindLabel = (last.words) unwindInfo
  CatchSwitch parent handlers unwindLabel

catchretStatement :: String -> VAR
catchretStatement line = do
  let (op, info) = strSplit' "from" line
      (token, labelInfo) = strSplit' "to" info
      normal = getLabel labelInfo
  CatchRet token normal

cleanupretStatement :: String -> VAR
cleanupretStatement line = do
  let (op, info) = strSplit' "from" line
      (value, labelInfo) = strSplit' "unwind" info
      unwind = (last.words) labelInfo
  CleanUpRet value unwind

parseStatement :: String -> String -> (VAR, [String])
parseStatement line fname
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

      "cleanupret" -> do
        let s = cleanupretStatement line
        (s, [token s, normal s])

      "unreachable" -> (Unreachable, [])

      -- memory
      "load" -> do
        let s = loadStatement line fname
        (s, [ptr s])

      "alloca" -> (allocaStatement line fname, [])

      "fence" -> (fenceStatement line fname, [])

      "cmpxchg" -> do
        let s = cmpxchgStatement line fname
        (s, [ptr s, cmp s, new s])

      "atomicrmw" -> do
        let s = atomicrmwStatement line fname
        (s, [ptr s, value s])

      "getelementptr" -> do
        let s = getelementptrStatement line fname
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
        if (not.null) (line =~ regex_semi :: [[String]])
          then do
            let (v1:v2:etc) = (tail.head) (line =~ regex_semi :: [[String]])
                hs = '%':v1
                ls = '%':v2
            (Declare hs ls, [hs, ls])
          else (Other, [])
  where op = head $ words line
        opty = getInstructionType op

statement :: String -> String -> (Maybe String, (VAR, [String]))
statement line fname = do
  let (lhs, rhs) = (strSplit " = " line)
  if (null rhs)
    then (Nothing, parseStatement lhs fname)
    else (Just lhs, parseStatement rhs fname)

{-************** Variables **************-}

addVariable :: String -> String -> [(String, String)] -> [(String, String)]
addVariable v t varList = varList ++ [(v, t)]

removeVariable :: String -> [(String, String)] -> [(String, String)]
removeVariable v varList = filter (/= (v, getType v varList)) varList

getType :: String -> [(String, String)] -> String
getType v varList = bool (fromJust t) "none" (isNothing t)
  where t = (lookup v varList)

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
--   | (isVarDeclare line fname) = do
--     let (v, (var, reg)) = statement line fname
--         newList = addVariable (fromJust v) (variableType var) varSet
--     detectVariable nextCont fname newList
--   | otherwise = detectVariable nextCont fname varSet

{-************************************************************************
                              Def and Use
  *************************************************************************-}

-- value content fname line_number recursive_list
-- -> (line_number, (used variable info))
findUse :: String -> [String] -> String -> [(VAR, [String])] -> [(VAR, [String])]
findUse v [] fname uselist = uselist
findUse v (line:nextContent) fname uselist
  | (isFunction line) = bool (uselist) (findUse v nextContent (getFunctionName line) uselist) (null "")
  | (isInfixOf v line) = do
    let (lhs, rhs) = strSplit' " = " line
        useVar = bool (parseStatement rhs fname) (parseStatement lhs fname) (null rhs)
    findUse v nextContent fname  (uselist ++ [useVar])
  -- |
  | otherwise = findUse v nextContent fname  uselist

{--}
-- line #, (VAR, [Strings])
is_useConv :: [(VAR, [String])] -> Bool
is_useConv [x] = isConv (fst x)
is_useConv (x:xs) = isConv (fst x) && (is_useConv xs)--(map (fst.snd) x)

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
