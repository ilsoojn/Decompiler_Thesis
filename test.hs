import Block
import RegexFunction
import Data.Array
import Data.Graph
import Data.Tree
import Data.List
import Debug.Trace
import Data.Maybe
-- data Block = Block { label :: String, text :: [String], pre :: [String], suc :: [String]} deriving (Ord, Eq, Show, Read)



main = do

  let str1 = ["define void @fn_0(%regset* noalias nocapture) {",
            "%test = 0",
            "entry_fn_0:",
              "%a = 10",
              "br label %bb_0",
            "exit_fn_0: ; preds = %bb_7",
              "ret void",
            "bb_0: ; preds = %entry_fn_0",
              "%u = (%a % 2 == 0)",
              "br label %bb_1",
            "bb_1: ; preds = %bb_0",
              "%b = %a * 10",
              "br i1 %u, label %bb_2, label %bb_3",
            "bb_2: ; preds = %bb_1",
              "%a = %a * 10",
              "br label %bb_4",
            "bb_3: ; preds = %bb_1",
              "%b = %a + 100",
              "br label %bb_5",
            "bb_4: ; preds = %bb_2",
              "%b = %a - 1",
              "br label %bb_5",
            "bb_5: ; preds = %bb_3, %bb_4, %bb_6",
              "%c = %b * %b + 1",
              "br label %bb_51",
            "bb_51: ; preds = %bb_5",
              "%c = %c + 1",
              "br label %bb_6",
            "bb_6: ; preds = %bb_51",
              "%d = (%c % 2)", "%v = (%d == 0)",
              "br i1 %v, label %bb_5, label %bb_7",
            "bb_7: ; preds = %bb_6",
              "print %d",
              "br label %exit_fn_0",
            "}"
            ]
      str2 = ["define void @fn_400480(%regset* noalias nocapture) {",
            "%f = alloca double, align 8",
            "%e = alloca double, align 8",
            "%d = alloca double, align 8",
            "%c = alloca i32, align 4",
            "%b = alloca i32, align 4,",
            "entry_fn_400480:",
            "br label %bb_400480",
            "exit_fn_400480: ; preds = %bb_400480",
            "ret void",
            "bb_400480: ; preds = %entry_fn_400480",
            "store i32 0, i32* %b, align 1",
            "store i32 10, i32* %c, align 1",
            "store double 313.24, double* %d, align 1",
            "store double 10.0, double* %e, align 1",
            "%76 = load double, double* %e, align 1",
            "%77 = fmul double %c, %76",
            "%89 = load double, double* %d, align 1",
            "%90 = fadd double %77, %89",
            "store double %90, double* %f, align 1",
            "br label %exit_fn_400480",
            "}"
            ]
      str3 = ["define void @fn_0(%regset* noalias nocapture) {",
              "%test = 0",
              "entry_fn_0:",
                "%a = 10",
                "br label %bb_0",
              "exit_fn_0: ; preds = %bb_7",
                "ret void",
              "bb_0: ; preds = %entry_fn_0",
                "%u = (%a % 2 == 0)",
                "br label %bb_1",
              "bb_1: ; preds = %bb_0",
                "%b = %a * 10",
                "br i1 %u, label %bb_2, label %bb_3",
              "bb_2: ; preds = %bb_1",
                "%a = %a * 10",
                "br label %bb_4",
              "bb_3: ; preds = %bb_1",
                "%b = %a + 100",
                "br label %bb_5",
              "bb_4: ; preds = %bb_2",
                "%b = %a - 1",
                "br label %bb_5",
              "bb_5: ; preds = %bb_3, %bb_4, %bb_6",
                "%c = %b * %b + 1",
                "br label %bb_6",
              "bb_6: ; preds = %bb_5",
                "%d = (%c % 2)", "%v = (%d == 0)",
                "br i1 %v, label %bb_5, label %bb_7",
              "bb_7: ; preds = %bb_6, %bb_8",
                "print %d",
                "br label %bb_8",
              "bb_8: ; preds = %bb_7",
                "print (hello world!) ",
                "br i1 %v, label %exit_fn_0, label %bb_7",
              "}"
              ]
  -- let (g, t) = createCFG str3
  --     sc = scc g
  --     sf = map subForest sc
  --     c = map snd $ filter (\x-> (fst x) > 0) (zip (map length $ map subForest sc) sc)
  --     text = blockToText g t
  --     l = topSort g
  --     cl = getCyclic g
  --     x = minusList [[0, 2], [1, 5], [3, 0, 2], [3, 2]] [2, 3] []
  -- -- print (edges g)
  -- -- print (map fst $ filter (\e -> snd e > 1) (assocs $ outdegree g))
  -- print (x)
  -- print (l)
  -- mapM_ print text
  let f1 = head str1
      f2 = head str2
      f3 = head str3

  print $ fromJust (getFunctionInfo f1)

  -- let n0 = BasicBlock "0" ["entry"] ["a = 10"] ["1"]
  --     n1 = BasicBlock "1" ["0"] ["IF(a % 2 == 0)"] ["2", "3"]
  --     n2 = BasicBlock "2" ["1"] ["a = a * 10", "b = a - 1"] ["4"]
  --     n3 = BasicBlock "3" ["1"] ["b = a", "c = -100"] ["5"]
  --     n4 = BasicBlock "4" ["2"] ["c = 100"] ["5"]
  --     n5 = BasicBlock "5" ["2", "3", "5"] ["d = b + c"] ["6"]
  --     n6 = BasicBlock "6" ["5"] ["e = String d"] ["5", "7"]
  --     n7 = BasicBlock "7" ["6"] ["PRINT e"] []
  -- let h =  [n0, n1, n2,n3, n4,n5,n6,n7]
  -- print (printGraph h)
  -- print $ show (isCyclic h)
  -- let bb0 = BasicBlock "%bb_400480" ["%a = 1"] ["%entry_fn_400480"] ["%bb_40048A"]
  --     bb1 = BasicBlock "%bb_40048A" ["%b = %a + 1"] ["%bb_400480"] ["%bb_400490", "%bb_400498"]
  --     bb2 = BasicBlock "%bb_400490" ["%b = %a + 1"] ["%bb_40048A"] ["%bb_4004A0"]
  --     bb4 = BasicBlock "%bb_4004A0" ["%c = %a + %b"] ["%bb_400490"] ["%bb_4004AE"]
  --     bb3 = BasicBlock "%bb_400498" ["%c = %a - %b"] ["%bb_40048A"] ["%bb_4004AE"]
  --
  --     bb5 = BasicBlock "%bb_4004AE" ["%d = %c * 2"] ["%bb_400498", "bb_4004A0", "bb_4004B0"] ["%bb_4004B0"]
  --     bb7 = BasicBlock "%bb_4004BF" ["%f = %e + 1000", "ret void"] ["%bb_4004B0"] []
  --     bb6 = BasicBlock "%bb_4004B0" ["%e = %d + 100"] ["%bb_4004AE"] ["%bb_4004AE", "%bb_4004BF"]
  --     bb = [bb0, bb1, bb2, bb3, bb4, bb5, bb6, bb7]
  -- --
  -- let n7 = Node bb7 []
  --     n6 = Node bb6 [n7]
  --     n5 = Node bb5 [n6]
  --     n4 = Node bb4 [n5]
  --     n3 = Node bb3 [n5]
  --     n2 = Node bb2 [n4]
  --     n1 = Node bb1 [n2, n3]
  --     n0 = Node bb0 [n1]
  --     n6' = n6{next=[n5, n7]}

  -- let gm = CFG [m0, m1, m2, m3, m4, m5, m6, m7]
  --     gn = CFG [n0, n1, n2, n3, n4, n5, n6, n7]
  -- let h = CFG bb
  -- print (printGraph h)
  -- print $ show (isCyclic h)
  -- print $ show (isCyclic h)
