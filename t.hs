import System.Directory
import System.Environment
import System.IO
import System.Process

import Data.Bool
import Data.Bits
import Data.Char
import Data.DeriveTH
import Data.List
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
import Idioms
import IsGetSet
import Idioms
import Lists
--
-- f :: String -> String -> Bool
-- f r s = (not.null) $ map (head.tail) (s =~ r :: [[String]])
--
-- g :: String -> String -> Bool
-- g r s = bool False True (null (s =~ r :: [[String]]))
--
-- main = do
--   let x = "hello world!"
--       y = "(.*)o(.*)"
--       v = ".*zzz(.*)"
--
--   putStrLn $ "f: " ++ (bool "False" "True" (f v x))
--   putStrLn $ "g: " ++ (bool "False" "True" (g v x))

-- replaceWord :: String -> String -> [String] -> String -> [String]
-- replaceWord old new [] regex = []
-- replaceWord old new (str:ss) regex
--   | (old == str) = new : (replaceWord old new ss regex)
--   | otherwise = do
--     let after_pad = unwords $ map (head.tail) (str =~ regex :: [[String]])
--         tmp_word = head $ endBy after_pad str
--     (bool str (new ++ after_pad) (old == tmp_word)) : (replaceWord old new ss regex)
--
-- replaceLine :: String -> String -> String -> String -> String
-- replaceLine old new tyStr line
--   | (isInfixOf old line) = do
--     let s = split (startsWith old) line--no strip!!! ["front", "old...", "old..."]
--         r = tyStr ++ regex_after_padding
--     unwords $ replaceWord old new s r
--   | otherwise = line
--
-- main = do
--   let a = "bb_40041        ; preds = %entry_."
--       b = "bb_400410        ; preds = %entry_."
--       c = "br label %bb_400410"
--       d = "br label %bb_40041"
--       e = "br i1 %CC_BE_0, label %bb_40041, label %bb_400410"
--       f = "define void @fn_400410(%regset...) {"
--       g = "define void @fn_40041(%regset...) {"
--       h = "call void @fn_400410(%regset* %0)"
--       i = "call void @fn_40041(%regset* %0)"
--       x = "@fn_40041"
--       y = "%bb_40041"
--
--       s = [a, b, c, d, e, f, g, h, i]
--
--   putStrLn $ "\nA >> " ++ (replaceLine y "%BB" str_bb a)
--   putStrLn $ "B >> " ++ (replaceLine y "%BB" str_bb b)
--   putStrLn $ "C >> " ++ (replaceLine y "%BB" str_bb c)
--   putStrLn $ "D >> " ++ (replaceLine y "%BB" str_bb d)
--   putStrLn $ "E >> " ++ (replaceLine y "%BB" str_bb e)
--   putStrLn $ "F >> " ++ (replaceLine y "%BB" str_bb f)
--   putStrLn $ "G >> " ++ (replaceLine y "%BB" str_bb g)
--   putStrLn $ "H >> " ++ (replaceLine y "%BB" str_bb h)
--   putStrLn $ "I >> " ++ (replaceLine y "%BB" str_bb i)
--   putStrLn "---------------------------"
--   putStrLn $ "A >> " ++ (replaceLine x "@FN" str_fn a)
--   putStrLn $ "B >> " ++ (replaceLine x "@FN" str_fn b)
--   putStrLn $ "C >> " ++ (replaceLine x "@FN" str_fn c)
--   putStrLn $ "D >> " ++ (replaceLine x "@FN" str_fn d)
--   putStrLn $ "E >> " ++ (replaceLine x "@FN" str_fn e)
--   putStrLn $ "F >> " ++ (replaceLine x "@FN" str_fn f)
--   putStrLn $ "G >> " ++ (replaceLine x "@FN" str_fn g)
--   putStrLn $ "H >> " ++ (replaceLine x "@FN" str_fn h)
--   putStrLn $ "I >> " ++ (replaceLine x "@FN" str_fn i)

-- fnSplit :: [String] -> String -> [String] -> [(String, String)] -> [ ([String] , [(String, String)]) ] -> [ ([String] , [(String, String)]) ]
-- fnSplit [] fn cont var set = set
-- fnSplit (line: nextC) fn cont var set
--   | (isFunction line) = fnSplit nextC (getFunctionName line) [line] [] set
--   | (isFunctionEnd line) = fnSplit nextC fn [] [] $ set ++ [(cont ++ [line], var)]
--   | (isLHS line fn) = do
--     let (x, (des, reg)) = statement line
--         newList = addVa%157 = %RBP_2-8

--     fnSplit nextC fn (cont ++ [line]) newList set
--   | otherwise = fnSplit nextC fn (cont ++ [line]) var set
--
-- printTuple :: [(String, String)] -> [String]
-- printTuple [] = []
-- printTuple (x:xs) = do
--   let (f, b) = x
--       s = "("++ f ++ ", " ++ b ++ ")"
--   s:(printTuple xs)
--
-- main = do
--   let a = "define void @fn_4004D0(%regset* noalias nocapture) {"
--
--       b = "%RBP_10 = load i64, i64* %RBP"
--       c = "%114 = add i64 %RBP_10, -24"
--       d = "%115 = inttoptr i64 %114 to i32*"
--
--       e = "%EAX_18 = load i32, i32* %115, align 1"
--       f = "%RAX_23 = zext i32 %EAX_18 to i64"
--
--       g = "%117 = add i64 %RSP_10, -8"
--       h = "%118 = inttoptr i64 %117 to i32*"
--
--       i = "%119 = load i32, i32* %118, align 1"
--       j = "%CC_GE_0 = icmp sge i32 %EAX_18, %119"
--       k = "%120 = sub i32 %EAX_18, %119"
--       l = "%ZF_015 = icmp eq i32 %120, 0"
--       m = "%SF_016 = icmp slt i32 %120, 0"
--       n = "%202 = sub i64 %RSP_0, 8"
--
--       o = "%203 = zext i128 %XMM1_2 to i256"
--       p = "%204 = and i256 %YMM1_1, -340282366920938463463374607431768211456"
--       q = "%YMM1_2 = or i256 %203, %204"
--
--       r = "%205 = trunc i256 %YMM1_2 to i64"
--       s = "store i64 %YMM1_2, i64* %202"
--       t = "}"
--
--       content = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t]
--
--   -- print $ elimination (fst $ propagation s "fn_0" [] vset) "fn_0" []
--   -- mapM_ ptrtoin
--       set = fnSplit content "" [] [] []
--       fs = head $ map fst set -- [String]
--       vs  =head $ map snd set--[(String, String)]
--
--   putStrLn ""
--   mapM_ print fs
--   putStrLn ""
--   mapM_ print (printTuple vs)
--   putStrLn ""

-- printBool x = (bool "false" "true" x)
-- printElem [] = ""
-- printElem (v:vs) = do
--   let x = fst v
--       y = fst $ snd v
--       z = snd $ snd v
--       s = (printBool x) ++ "(" ++ y ++ ", " ++ z ++ ")"
--   s ++ " & " ++ (printElem vs)
--
-- printClauses :: [Clause] -> String
-- printClauses [] = ""
-- printClauses (v:vs) = do
--   let ty = cty v
--       val = cvalue v
--   ("("++ ty ++ ", " ++ val ++ ")") ++ (printClauses vs)
--
-- printStatement :: [(Maybe String,(VAR, [String]))] -> [String]-> [String]
-- printStatement [] s = s
-- printStatement (x:xs) s
--   | (isUndef v) = printStatement xs (s ++ [("Undef)" ++ " value: " ++ value v)])
--   | (isConst v) = printStatement xs (s ++ [("Const)" ++ " value: " ++ value v)])
--   | (isDeclare v) = printStatement xs (s ++ [("Declare)" ++ " high: " ++ high v ++ ", low: " ++ low v)])
--   | (isVoidRet v) = printStatement xs (s ++ ["Ret Void"])
--   | (isRet v) = printStatement xs (s ++ [("Ret)" ++ " type: " ++ ty v ++ " value: " ++ value v)])
--   | (isBranch v) = printStatement xs (s ++ [("Branch)" ++ " label: " ++ label v)])
--   | (isCondBranch v) = printStatement xs (s ++ [("CondBranch)" ++ " cond: " ++ cond v ++ " trueLabel: " ++ trueLabel v ++ " falseLabel: " ++ falseLabel v)])
--   | (isIndirBranch v) = printStatement xs (s ++ [("IndirBranch)" ++ " type: " ++ ty v ++ " addr: " ++ addr v ++ " labels: [" ++ (head $ labels v) ++ ", " ++ (last $ labels v) ++ "]")])
--   | (isSwitch v) = printStatement xs (s ++ [("Switch)" ++ " type: " ++ ty v ++ " value: " ++ value v ++ " label: " ++ label v ++ " table: [" ++ (show $ length (table v)) ++ "]")])
--   | (isResume v) = printStatement xs (s ++ [("Resume)" ++ " type: " ++ ty v ++ " value: " ++ value v)])
--   | (isCatchSwitch v) = printStatement xs (s ++ [("CatchSwitch)" ++ " parent: " ++ parent v ++ " handlers: [" ++ (head $ handlers v) ++ ", " ++ (last $ handlers v) ++ "]" ++ " unwind: " ++ unwind v)])
--   | (isCatchRet v) = printStatement xs (s ++ [("CatchRet)" ++ " token: " ++ token v ++ " normal: " ++ normal v)])
--   | (isCleanUpRet v) = printStatement xs (s ++ [("CleanUpRet)" ++ " value: " ++ value v ++ " unwind: " ++ unwind v)])
--   -- | (isUnreachable v) = printStatement xs (s ++ ["Unreachable"]
--   | (isBinary v) = printStatement xs (s ++ [("Binary)" ++ " op: " ++ op v ++ " type: " ++ ty v ++ " values: [" ++ (head $ values v) ++ ", " ++ (last $ values v) ++ "]")])
--   | (isBitwise v) = printStatement xs (s ++ [("Bitwise)" ++ " op: " ++ op v ++ " type: " ++ ty v ++ " values: [" ++ (head $ values v) ++ ", " ++ (last $ values v) ++ "]")])
--   | (isRSP v) = printStatement xs (s ++ [("RSP)"  ++ " rsp: " ++ rsp v ++ " idx: " ++ idx v)])
--   | (isArray v) = printStatement xs (s ++ [("Array)" ++ " op: " ++ op v ++ " agg_type: " ++ agg_ty v ++ " value: " ++ value v ++ " e_type: " ++ (bool (fromJust $ e_ty v) "NOTHING" (isNothing $ e_ty v)) ++ " e: " ++ (bool (fromJust $ e v) "NOTHING" (isNothing $ e v)) ++ " e_idx: [" ++ (head $ e_idx v) ++ ", " ++ (last $ e_idx v) ++ "]")])
--   | (isStruct v) = printStatement xs (s ++ [("Struct)" ++ " op: " ++ op v ++ " agg_type: " ++ agg_ty v ++ " value: " ++ value v ++ " e_type: " ++ (bool (fromJust $ e_ty v) "NOTHING" (isNothing $ e_ty v)) ++ " e: " ++ (bool (fromJust $ e v) "NOTHING" (isNothing $ e v)) ++ " e_idx: [" ++ (head $ e_idx v) ++ ", " ++ (last $ e_idx v) ++ "]")])
--   | (isAlloca v) = printStatement xs (s ++ [("Alloca)"  ++ " inalloca: " ++ (printBool (inalloca v)) ++ " type: " ++ ty v ++ " alloc_numE: " ++ (bool (fromJust $ alloc_numE v) "NOTHING" (isNothing $ alloc_numE v)) ++ " alloc_align: " ++ (bool (fromJust $ alloc_align v) "NOTHING" (isNothing $ alloc_align v)) ++ " alloc_addrspace: " ++ (bool (fromJust $ alloc_addrspace v) "NOTHING" (isNothing $ alloc_addrspace v)))])
--   | (isLoad v) = printStatement xs (s ++ [("Load)"   ++ " volatile: " ++ (printBool (volatile v)) ++ " type: " ++ ty v ++ " ptr: " ++ ptr v)])
--   | (isFence v) = printStatement xs (s ++ [("Fence)"  ++ " syncscope: " ++ (bool (fromJust $ syncscope v) "NOTHING" (isNothing $ syncscope v)) ++ " ord: " ++ ordering v)])
--   | (isCmpxchg v) = printStatement xs (s ++ [("Cmpxchg)"  ++ " weak: " ++ (printBool (weak v)) ++ " volatile: " ++ (printBool (volatile v)) ++ " type: " ++ ty v ++ " ptr: " ++ ptr v ++ " cmp: " ++ cmp v ++ " new: " ++ new v ++ " syncscope: " ++ (bool (fromJust $ syncscope v) "NOTHING" (isNothing $ syncscope v)) ++ " success_ord: " ++ succ_ordering v ++ " fail_ord: " ++ fail_ordering v)])
--   | (isAtomicrmw v) = printStatement xs (s ++ [("Atomicrmw)"  ++ " volatile: " ++ (printBool (volatile v)) ++ " op: " ++ operation v ++ " type: " ++ ty v ++ " ptr: " ++ ptr v ++ " value: " ++ value v ++ " syncscope: " ++ (bool (fromJust $ syncscope v) "NOTHING" (isNothing $ syncscope v)) ++ " ord: " ++ ordering v)])
--   | (isGetElemPtr v) = printStatement xs (s ++ [("GetElemPtr)"  ++ " inbounds: " ++ (printBool (inbound v)) ++ " type: " ++ ty v ++ " ptr: " ++ ptr v ++ " & Elements: " ++ (bool (printElem (element v))  "NONE" (null $ element v)))])
--   | (isConv v) = printStatement xs (s ++ [("Conv)" ++ " op: " ++ op v ++ " type_from: " ++ ty1 v ++ " type_to: " ++ ty v ++ " value: " ++ value v)])
--   | (isCmpi v) = printStatement xs (s ++ [("Cmpi)" ++ " cond: " ++ cond v ++ " (" ++ sym v ++ ")" ++ " type: " ++ ty v ++ " values: [" ++ (head $ values v) ++ ", " ++ (last $ values v) ++ "]")])
--   | (isCmpf v) = printStatement xs (s ++ [("Cmpf)" ++ " cond: " ++ cond v ++ " flag: " ++ (bool (fromJust $ flag v) "NOTHING" (isNothing $ flag v)) ++ " (" ++ sym v ++ ")" ++ " type: " ++ ty v ++ " values: [" ++ (head $ values v) ++ ", " ++ (last $ values v) ++ "]")])
--   | (isPhi v) = printStatement xs (s ++ [("Phi)" ++ " type: " ++ ty v ++ " vLabels: [" ++ (head $ printTuple $ vlabels v) ++ ", " ++ (last $ printTuple $ vlabels v) ++ "]")])
--   | (isSelect v) = printStatement xs (s ++ [("Select)" ++ " selty: " ++ selty v ++ " cond: " ++ cond v ++ " type: " ++ ty v ++ " values: [" ++ (head $ values v) ++ ", " ++ (last $ values v) ++ "]")])
--   | (isVaArg v) = printStatement xs (s ++ [("VaArg)" ++ " va_list: " ++ va_list v ++ " arg_list: " ++ arglist v ++ " arg_type: " ++ argty v)])
--   | (isLandingPad v) = printStatement xs (s ++ [("LandingPad)" ++ " result_type: " ++ resultty v ++ " clauses: " ++ printClauses (clause v))])
--   | (isCatchPad v) = printStatement xs (s ++ [("CatchPad)" ++ " catchswitch: " ++ catchswitch v ++ " arg: " ++ arg v)])
--   | (isCleanUpPad v) = printStatement xs (s ++ [("CleanUpPad)" ++ " parent: " ++ parent v ++ " arg: " ++ arg v)])
--   | (isOther v) = printStatement xs (s ++ ["Other"])
--   | otherwise = printStatement xs (s ++ ["NONE STATEMENT"])
--     where (lhs, rhs) = x
--           (v, r) = rhs
--
-- doStatement [] = []
-- doStatement (x:xs) = (statement x):(doStatement xs)
--
-- main = do
--   let a = "%undef = undef"
--       b = "%const = 5"
--       c = "%declare = %high:%low"
--       tmp = [a, b, c]
--
--       t1 = "ret void"
--       t2 = "ret i32 %value"
--       t3 = "ret { i32, i8 } { i32 4, i8 2 }" -- <<<<<<<<<<<<<<<<
--       t4 = "br label %branch"
--       t5 = "br i1 %cond, label %trueBranch, label %falseBranch"
--       t6 = "indirectbr i8* %addr, [ label %label_1, label %label_2, label %label_3 ]"
--       t7 = "switch i32 0, label %label []"
--       t8 = "switch i32 %value, label %label [ i32 0, label %zero i32 1, label %one i32 2, label %two ]" -- <<<<<<<<<<<<<<<<
--       --Invoke
--       t9 = "resume i64 %value"
--       t10 = "resume { i8*, i32 } %value"
--       t11 = "catchswitch within none [label %handler_0, label %handler_1] unwind to caller"
--       t12 = "catchswitch within %parent [label %handler_0] unwind label %unwind_Default"
--       t13 = "catchret from %token to label %normal"
--       t14 = "cleanupret from %value unwind to caller"
--       t15 = "cleanupret from %value unwind label %unwind_continue"
--       t16 = "unreachable"
--       terminatorList = [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16]
--
--       b1 = "%b = add i32 %v1, %v2"
--       b2 = "%b = sub nuw i8 %v1, 4"
--       b3 = "%b = mul nsw i8 4, %v2"
--       b4 = "%b = mul nuw nsw i8 %v1, %v2"
--       b5 = "%b = udiv i32 4, %v"
--       b6 = "%b = sdiv exact i32 4, %v"
--       b7 = "%b = shl i32 4, %v"
--       b8 = "%b = shl nuw nsw i32 %v1, v2"
--       b9 = "%b = lshr <2 x i32> < i32 -2, i32 4>, < i32 1, i32 2>"
--       b10 = "%b = ashr i8 -4, 1"
--       b11 = "%b = and i32 15, 40"
--       b12 = "%b = or i32 4, %v"
--       b13 = "%b = xor i32 %v, -1"
--       binaryList = [b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13]
--
--       m1 = "%m = alloca i32"
--       m2 = "%m = alloca i32, i32 4"
--       m3 = "%m = alloca double, double 4, align 1024"
--       m4 = "%m = alloca i32, allign 1024" -- <<<<<<<<
--       m5 = "%m = load <16 x ty>, <16 x ty>* %ptr"
--       m6 = "%m = load i32, i32* %ptr"
--       m7 = "%m = load volatile i32, i32* %ptr"
--       m8 = "store i32 3, i32* %at"
--       m9 = "store <16 x ty> %20, <16 x ty>* %at"
--       m10 = "store atomic i32 value, i32* %at"
--       m11 = "fence ordering"
--       m12 = "fence syncscope(\"target_scope\") ordering"
--       m13 = "cmpxchg i32* %ptr, i32 %cmp, i32 %new success_ord fail_ord"
--       m14 = "cmpxchg i32* %ptr, i32 %cmp, i32 %new syncscope(\"target_scope\") success_ord fail_ord"
--       m15 = "atomicrmw operation i32* %ptr, i32 %value syncscope(\"target_scope\") ordering  "
--       m16 = "%m = getelementptr i32, i32* %ptr"
--       m17 = "%m = getelementptr inbounds %struct.ST, %struct.ST* %s, i64 idx_1, i32 idx_2, i32 idx_3, i64 idx_4, i64 idx_6"
--       m18 = "%m = getelementptr [10 x [20 x i32]], [10 x [20 x i32]]* %t3, i32 idx_1, i32 idx_2"
--       m19 = "%19 = getelementptr i8, <4 x i8*> %ptrs, <4 x i64> %offsets"
--       m20 = "%A = getelementptr i8, i8* %ptr, <4 x i64> %offsets"
--       m21 = "%m = getelementptr inbounds [1024 x i8], [1024 x i8]* %4, i32 0, i32 0"
--       m22 = "%m = getelementptr  %struct.ST, <4 x %struct.ST*> %s, <4 x i64> %ind1, <4 x i32> <i32 2, i32 2, i32 2, i32 2>, <4 x i32> <i32 1, i32 1, i32 1, i32 1>, <4 x i32> %ind4, <4 x i64> <i64 13, i64 13, i64 13, i64 13>, i32 0, i32 0"
--       memoryList = [m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21]
--
--       c1 = "%c = trunc i64 %value to i32"
--       c2 = "%c = trunc <2 x i16> <i16 8, i16 7> to <2 x i8>"
--       c3 = "%c = fptrunc double 16777217.0 to float"
--       c4 = "%c = bitcast <2 x int> %V to i64"
--       conversionList = [c1, c2, c3, c4]
--
--       o1 = "%o = icmp olt i32 4, 5"
--       o2 = "%o = fcmp olt double* %v1, %v2"
--       o3 = "%o = phi i32 [ 0, %label_1 ], [ %val, %label_2 ]"
--       o4 = "%o = select i1 true, i8 17, i8 42"
--       o5 = "%o = select {<n x i1>} true, i8 %value_1, i8 %value_2"
--       -- call
--       -- va_arg
--       o6 = "%o = landingpad { i8*, i32 } catch i8** %value"
--       o7 = "%o = landingpad { i8*, i32 } cleanup"
--       o8 = "%o = landingpad { i8*, i32 } catch i8** %value filter [1 x i8**] [%arr_const]"
--       o9 = "%o = catchpad within %catchswitch [i8** %arg]"
--       o10 = "%o = catchpad within %catchswitch []"
--       o11 = "%o = cleanuppad within %parent [i8** %arg]"
--       o12 = "%o = cleanuppad within %parent []"
--       otherList = [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12]
--
--       set = tmp ++ terminatorList ++ binaryList ++ memoryList ++ conversionList ++ otherList
--   -- putStrLn $ unwords $  printStatement (doStatement [t13]) []
--   mapM_ print (printStatement (doStatement [o6, o7, o8]) [])
--
-- set [] str n list = list
-- set (v:vs) str n list
--   | (hasOpening v && hasEnding v) = set vs (str ++ " " ++ v) (n) list
--   | (hasOpening v) = set vs (str ++ " " ++ v) (n+1) list
--   | (hasEnding v && n > 1) = set vs (str ++ " " ++ v) (n-1) list
--   | (hasEnding v && n == 1) = set vs "" 0 (list ++ [str ++ " " ++ v])
--   | otherwise = set vs "" 0 (list ++ [v])
--
-- main  = do
--   let v = "ok [ test x [ in x type ] ] and type { i8, [10 x [20 x i32]], i8 }"
--       u = "[20 x i32], [20 x i32]* %t4, i32 idx_1, i32 idx_2"
--   mapM_ print $ splitStartEndOneOf "{[<" ">]}" u
-- --   putStrLn ""
-- --   mapM_ print $ splitStartEndOneOf "{[<" ">]}" v
--
-- remove_uses :: String -> [String] -> [String] -> [String]
-- remove_uses str [] preLine = preLine
-- remove_uses str (line:nextLine) preLine
--   | (isUse str line) = do
--     if (isLHS line "caller_remove_uses")
--       then do
--         let v = fst (strSplit " = " line)
--         remove_uses str (remove_uses v nextLine []) preLine
--     else remove_uses str nextLine preLine
--   | otherwise = remove_uses str nextLine (preLine ++ [line])
--
-- remove_st :: String -> [String] -> [String] -> [String]
-- remove_st str [] preLines = preLines
-- remove_st str (line:nextLines) preLines
--   | (isInfixOf str line) = do
--     if (isLHS line "caller_remove_st")
--       then do
--         let v = fst (strSplit " = " line)
--         remove_st str (remove_uses v nextLines []) preLines
--       else
--         remove_st str nextLines preLines
--   | otherwise = remove_st str nextLines (preLines ++ [line])
--
-- remove_r :: [String] -> [String] -> [String]
-- remove_r [] content = content
-- remove_r (r:rs) content = do
--   remove_r rs (remove_st r content [])
--
-- splitFn :: [String] -> String -> [String] -> [LeftVar] -> [RP] -> [([String] , ([LeftVar],[RP]))] -> [([String] , ([LeftVar],[RP]))]
-- splitFn [] fn c v p set = set
-- splitFn (line: nextC) fn cList vList pList set
--   | (isFunction line) = splitFn nextC (getFunctionName line) [line] [] [] [] set
--   | (isFunctionEnd line) = splitFn nextC fn [] [] [] (set ++ [(cList ++ [line], vList, pList)])
--   -- | (isPrefixOf "entry_fn_" line || isPrefixOf "exit_fn_" line) =  splitFn (fnStartEndBlock (line:nextC)) fn cList vList pList set
--   | (isLHS line fn) = do
--     let (x, (des, reg)) = statement line
--         (v, state) = strSplit' "=" line
--
--     case (isPrefixOf "%R" (fromJust x) || isPrefixOf "%E" (fromJust x) ) of
--       True -> do
--         let pointer = fromJust x
--         if (isBinary des)
--           then do
--             let instr = op des
--                 rbase = head reg
--                 idx = read (last reg) :: Integer
--                 rdix = bool (0-idx) (idx) (instr == "add")
--                 newList = pList ++ [( RP pointer rbase rdix "")]
--             splitFn nextC (cList ++ [line]) vList newList set
--
--           else if (isLoad des)
--             then do
--               let rbase = head xreg
--                   newList = pList ++ [( RP pointer rbase 0 "")]
--               splitFn nextC (cList ++ [line]) vList newList set
--
--           else splitFn nextC (cList ++ [line]) vList pList set
--
--       _ -> do
--         let variable = fromJust x
--             vtype = variableType des
--             instr = head $ words state
--             newList = addVariable (variable) (vtype) (instr) (state) vList
--         splitFn nextC fn (cList ++ [line]) newList pList set
--   | otherwise = splitFn nextC fn (cList ++ [line]) vList pList set
--
-- fnSplit :: [String] -> String -> [String] -> [LeftVar] -> [([String] , [LeftVar])] -> [([String] , [LeftVar])]
-- fnSplit [] fn cont var set = set
-- fnSplit (line: nextC) fn cont var set
--   | (isFunction line) = fnSplit nextC (getFunctionName line) [line] [] set
--   | (isFunctionEnd line) = fnSplit nextC fn [] [] $ set ++ [(cont ++ [line], var)]
--   | (isLHS line fn) = do
--     let (x, (des, reg)) = statement line
--         (v, state) = strSplit' "=" line
--         newList = addVariable (fromJust x) (variableType des) (head $ words state) state var
--     fnSplit nextC fn (cont ++ [line]) newList set
--   | otherwise = fnSplit nextC fn (cont ++ [line]) var set

main = do
  let x = ["define void @fn_4004D0(%regset* noalias nocapture) {",
            "%157 = %RBP_2-8",
            "%EAX_3 = load i32, i32* %157, align 1",
            "%159 = sitofp i32 %EAX_3 to double",
            "%160 = bitcast double %159 to i64",
            "%XMM1_1 = %XMM1_0 : %160",
            "%170 = trunc i128 %XMM0_4 to i64",
            "%171 = bitcast i64 %170 to double",
            "%172 = trunc i128 %XMM1_1 to i64",
            "%173 = bitcast i64 %172 to double",
            "%174 = fdiv double %171, %173",
            "%175 = bitcast double %174 to i64",
            "%XMM0_5 = %XMM0_4 : %175",
            "}"]   -- x
      y = ["%160 = bitcast double %159 to i64",
            "%XMM1_1 = %XMM1_0 : %160"]

  -- let (f, (vlist, plist)) = head $ splitFn (map strip x) "" [] [] [] []
  --     (cont, list) = propagation f "" vlist []
  --
  -- let (g, ulist) = head $ fnSplit (map strip x) "" [] [] []
  --     (c, l) = propagateTypeVar y "double" "%159" "i32" "EAX_3" ulist []

  let n = ["0", "$03", "-3", "3A", "103", "ABC", "3.241", "-3.111"]
  mapM_ print (foldl (isNum) n)
  -- mapM_ print cont
-- printList [] = []
-- printList (x:xs) = (concat [variable x, " (", vtype x, ") <- (", instruction x, ") ", state x]) : printList xs
--
-- fnSplit :: [String] -> String -> [String] -> [LeftVar] -> [([String] , [LeftVar])] -> [([String] , [LeftVar])]
-- fnSplit [] fn cont var set = set
-- fnSplit (line: nextC) fn cont var set
--   | (isFunction line) = fnSplit nextC (getFunctionName line) [line] [] set
--   | (isFunctionEnd line) = fnSplit nextC fn [] [] $ set ++ [(cont ++ [line], var)]
--   | (isLHS line fn) = do
--     let (x, (des, reg)) = statement line
--         (v, state) = strSplit' "=" line
--         newList = addVariable (fromJust x) (variableType des) (head $ words state) state var
--     fnSplit nextC fn (cont ++ [line]) newList set
--   | otherwise = fnSplit nextC fn (cont ++ [line]) var set
--
-- fnElimination :: [([String], [LeftVar])] -> [([String], [LeftVar])]
-- fnElimination [] = []
-- -- fnElimination (f:fs) = (propagation f "" 0 []):(fnElimination fs)
-- fnElimination ((content, vList):fs) =  (simply content "" [] vList):(fnElimination fs)
--
-- main = do
--   let as = "define void @fn_A(%regset* noalias nocapture) {"
--       a1 = " %110 = add i64 %RBP_1, -24"
--       a2 = "%111 = inttoptr i64 %110 to i32*"
--
--       a3 = "%37 = add i64 %RSP_1, -4"
--       a4 = "%38 = inttoptr i64 %37 to i32*"
--       a5 = "store i32 0, i32* %38, align 1"
--       ae = "}"
--
--       aSet = [as, a1, a2, a3, a4, a5, ae]
--
--       bs = "define void @fn_B(%regset* noalias nocapture) {"
--       b1 = "%ZMM0_3 = load <16 x float>, <16 x float>* %ZMM0"
--       b2 = "%147 = bitcast <16 x float> %ZMM0_3 to i512"
--       b3 = "%XMM0_3 = trunc i512 %147 to i128"
--       b4 = "%148 = zext i64 %146 to i128"
--       b5 = "%149 = and i128 %XMM0_3, -18446744073709551616"
--       b6 = "%XMM0_4 = or i128 %148, %149"
--
--       b7 = "%150 = bitcast <16 x float> %ZMM0_3 to i512"
--       b8 = "%YMM0_3 = trunc i512 %150 to i256"
--       b9 = "%151 = zext i128 %XMM0_4 to i256"
--       b10 = "%152 = and i256 %YMM0_3, -340282366920938463463374607431768211456"
--       b11 = "%YMM0_4 = or i256 %151, %152"
--
--       b12 = "%153 = bitcast <16 x float> %ZMM0_3 to i512"
--       b13 = "%154 = zext i128 %XMM0_4 to i512"
--       b14 = "%155 = and i512 %153, -340282366920938463463374607431768211456"
--       b15 = "%ZMM0_4 = or i512 %154, %155"
--       b16 = "%RIP_19 = add i64 %RIP_18, 3"
--       b17 = "%EIP_16 = trunc i64 %RIP_19 to i32"
--       b18 = "%IP_16 = trunc i64 %RIP_19 to i16"
--       be = "}"
--
--       bSet = [bs, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, be]
--       cont = aSet ++ bSet
--
--       -- (tmp, vlist) = head $ fnSplit aSet "" [] [] []
--   mapM_ print (head $ map fst $ fnElimination $ fnSplit bSet "" [] [] [])
