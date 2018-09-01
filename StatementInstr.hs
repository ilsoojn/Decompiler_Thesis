module StatementInstr where

import Data.Maybe
import Data.Tuple
import OtherFunction

import Debug.Trace

{-************** Format and Regex **************-}
data Function = Function {name::String, block::[(String, String)]}
data ListCount = ListCount {list :: [String], count :: Int} deriving (Show, Read)

data Clause = Catch {cty::String, cvalue::String} | Filter {cty::String, cvalue::String} | CleanUp deriving (Show)-- Catch <= single val, Filter <= Array
data Chain = Chain {v::String, def::(Integer, VAR), use::[(Integer, VAR)]} deriving (Show)

-- data LOAD = LOAD {ld_ty::String, rsp::String, index::Integer}
data VAR = Undef {value::String}
          | Const {value::String}
          | Declare {high::String, low::String}
          -- Terminator
          | RetVoid
          | Ret {ty::String, value::String} -- <<
          | Branch {label::String} -- <<
          | CondBranch {cond::String, trueLabel::String, falseLabel::String} -- <<
          | IndirBranch {ty::String, addr::String, labels::[String]} -- <<
          | Switch {ty::String, value::String, label::String, table::[(String, String)]} -- <<
          -- | Invoke {ccov::Maybe String, retAttrs::Maybe String, ty::String, f_ptr::String, f_arg::[String], f_attr::Maybe [String], bundle::Maybe [String], normLabel::String, excLabel::String}
          | Resume {ty::String, value::String} -- <<
          | CatchSwitch {parent::String, handlers::[String], unwind::String} -- <<
          | CatchRet {token::String, normal::String} -- <<
          | CleanUpRet {value::String, unwind::String} -- <<
          | Unreachable -- <<
          -- Bitwise & Binary
          | Binary {op::String, sym::String, ty::String, values::[String]}
          | Bit {op::String, sym::String, ty::String, values::[String]}
          | RSP {fname::String, rsp::String, idx::String}
          -- Aggregate
          | Array {op::String, agg_ty::String, value::String, e_ty::Maybe String, e::Maybe String, e_idx::[String]} -- agg_ty : [n x ty] <- [n x [n x ty]]
          | Struct {op::String, agg_ty::String, value::String, e_ty::Maybe String, e::Maybe String, e_idx::[String]} -- agg_ty : {ty} <- {ty, {ty}}
          -- Memory
          | Alloca {fname::String, inalloca::Bool, ty::String, alloc_numE::Maybe String, alloc_align::Maybe String, alloc_addrspace::Maybe String}
          | LOAD {fname::String, atomic::Bool, volatile::Bool, ty::String, ptr::String}
          | Fence {fname::String, syncscope::Maybe String, ordering::String} -- <<
          | Cmpxchg {fname::String, weak::Bool, volatile::Bool, ty::String, ptr::String, cmp::String, new::String, syncscope::Maybe String, succ_ordering::String, fail_ordering::String} -- <<
          | Atomicrmw {fname::String, volatile::Bool, operation::String, ty::String, ptr::String, value::String, syncscope::Maybe String, ordering::String} -- <<
          | GetElemPtr {fname::String, inbound::Bool, ty::String, ptr::String, element::[(Bool, String, String)]}
          -- Converison
          | Conv {op::String, ty1::String, ty::String, value::String}
          -- Other
          | Cmpi {cond::String, sym::String, ty::String, values::[String]}
          | Cmpf {cond::String, flag::Maybe String, sym::String, ty::String, values::[String]}
          | Phi {ty::String, vlabels::[(String, String)]}
          | Select {selty::String, cond::String, ty::String, values::[String]}
          -- | Call {tailing::Maybe String, flag::Maybe String, ccov::Maybe String, retAttrs::Maybe String, ty::String, f_ptr::String, f_arg::[String], f_attr::[String], bundle::[String], normLabel::String, unwindLabel::String}
          | VaArg {va_list::String, arglist::String, argty::String}
          | LandingPad {resultty::String, cleanup::Bool, clause::[Clause]}
          | CatchPad {catchswitch::String, arg::String}
          | CleanUpPad {parent::String, arg::String}
          -- undefined ones / temporary
          | Other deriving (Show)

data STORE = STORE {str_atomic::Bool, str_volatile::Bool, str_ty::String, str_v::[(String, VAR)], str_at::[(String, VAR)]} deriving (Show)

isStore (STORE _ _ _ _ _) = True

isUndef (Undef _) = True
isUndef _ = False
isConst (Const _) = True
isConst _ = False
isDeclare (Declare _ _) = True
isDeclare _ = False
isVoidRet (RetVoid) = True
isVoidRet _ = False
isRet (Ret _ _) = True
isRet _ = False
isBranch (Branch _) = True
isBranch _ = False
isCondBranch (CondBranch _ _ _) = True
isCondBranch _ = False
isIndirBranch (IndirBranch _ _ _) = True
isIndirBranch _ = False
isSwitch (Switch _ _ _ _) = True
isSwitch _ = False
-- isInvoke (Invoke _ _ _ _ _ _ _ _ _) = True
-- isInvoke _ = False
isResume (Resume _ _) = True
isResume _ = False
isCatchSwitch (CatchSwitch _ _ _) = True
isCatchSwitch _ = False
isCatchRet (CatchRet _ _) = True
isCatchRet _ = False
isCleanUpRet (CleanUpRet _ _) = True
isCleanUpRet _ = False
isUnreachable(Unreachable) = True
isUnreachable_ = False
isBinary (Binary _ _ _ _) = True
isBinary _ = False
isBitwise (Bit _ _ _  _) = True
isBitwise _ = False
isRSP (RSP _ _ _) = True
isRSP _ = False
isArray (Array _ _ _ _ _ _) = True
isArray _ = False
isStruct (Struct _ _ _ _ _ _) = True
isStruct _ = False
isAlloca (Alloca _ _ _ _ _ _) = True
isAlloca _ = False
isLoad (LOAD _ _ _ _ _) = True
isLoad _ = False
isFence (Fence _ _ _) = True
isFence _ = False
isCmpxchg (Cmpxchg _ _ _ _ _ _ _ _ _ _) = True
isCmpxchg _ = False
isAtomicrmw (Atomicrmw _ _ _ _ _ _ _ _) = True
isAtomicrmw _ = False
isGetElemPtr (GetElemPtr _ _ _ _ _) = True
isGetElemPtr _ = False
isConv (Conv _ _ _ _) = True
isConv _ = False
isCmpi (Cmpi _ _ _ _) = True
isCmpi _ = False
isCmpf (Cmpf _ _ _ _ _) = True
isCmpf _ = False
isPhi (Phi _ _) = True
isPhi _ = False
isSelect (Select _ _ _ _) = True
isSelect _ = False
-- isCall (Call _ _ _ _ _ _ _ _ _ _ _) = True
-- isCall _ = False
isVaArg (VaArg _ _ _) = True
isVaArg _ = False
isLandingPad (LandingPad _ _ _) = True
isLandingPad _ = False
isCatchPad (CatchPad _ _) = True
isCatchPad _ = False
isCleanUpPad (CleanUpPad _ _) = True
isCleanUpPad _ = False
isOther (Other) = True
isOther _ = False

{-************************************************************************
                            All Ones Byte
  *************************************************************************-}

all_ones :: (Num a, Integral a) => [(a, a)]
all_ones = pairOneToOne bits (map (2^) bits)
  where bits = map (2^) [0, 1, 2, 3, 4, 5, 6, 7]

isOnes, isZeros :: (Num a, Integral a, Eq a) => a -> Bool
isZeros x = not $ isNothing $ lookup (abs x) (map swap all_ones)
isOnes x = (isZeros (x+1))

hasOnes, hasZeros :: (Num a, Integral a, Eq a) => [a] -> Bool
hasOnes x = do
  case (length x) of
    1 -> isOnes (head x)
    2 -> isOnes (head x) || isOnes (last x)
    _ -> False

hasZeros x = do
  case (length x) of
    1 -> isZeros (head x)
    2 -> isZeros (head x) || isZeros (last x)
    _ -> False

{-************************************************************************
                    Archtecture Registers and Pointer
  *************************************************************************-}

reg_8, reg_16, reg_32, reg_64, reg_other, reg_ip :: [String]
reg_8 = ["%AH","%AL","%BH","%BL","%CH","%CL","%DH","%DL","%SIL","%DIL","%SPL","%BPL"]
reg_16 = ["%AX","%BX","%CX","%DX","%SI","%DI","%SP","%BP"]
reg_32 = ["%EAX","%EBX","%ECX","%EDX","%ESI","%EDI","%ESP","%EBP","EFLAGS"]
reg_64 = ["%RAX","%RBX","%RCX","%RDX","%RSI","%RDI","%RSP","%RBP","RFLAGS"]
reg_other = ["XMM0","XMM1","XMM2","YMM0","YMM1","YMM2","ZMM0","ZMM1","ZMM2"]
reg_ip = ["IP","EIP","RIP"]

{-************************************************************************
                            LLVM INSTRUCTIONS
  *************************************************************************-}

instructionList :: [(String, [String])] -> [(String, String)]
instructionList [] = []
instructionList (x:xs)= (pairOneToMany (fst x) (snd x))++(instructionList xs)

instructions :: [(String, String)]
instructions = instructionList [("terminator", ["ret", "br", "switch", "indirectbr", "invoke", "resume", "catchswitch", "catchret", "cleanupret", "unreachable"]),
        ("binary", ["add", "fadd", "sub", "fsub", "mul", "fmul", "udiv", "sdiv", "fdiv", "urem", "srem", "frem"]),
        ("bitwise", ["shl", "lshr", "ashr", "and", "or", "xor"]),
        ("vector", ["extractelement", "insertelement", "shufflevector"]),
        ("aggregate", ["extractvalue", "insertvalue"]),
        ("memory", ["alloca", "load", "store", "fence", "cmpxchg", "atomicrmw", "getelementptr"]),
        ("conversion", ["trunc", "zext", "sext", "fptrunc", "fpext", "fptoui", "fptosi", "uitofp", "sitofp", "ptrtoint", "inttoptr", "bitcast", "addrspacecast"]),
        ("other", ["icmp", "fcmp", "phi", "select", "call", "va_arg", "landingpad", "catchpad", "cleanuppad"])]

arithmetic :: [(String, String)]
arithmetic = [("add", "+"), ("fadd", "+"),
              ("sub", "-"), ("fsub", "-"),
              ("mul", "*"), ("fmul", "*"),
              ("udiv", "/"), ("sdiv", "/"), ("fdiv", "/"),
              ("urem", "%"), ("srem", "%"), ("frem", "%"),
              ("and", "&"), ("or", "|"), ("xor", "^"),
              ("shl","<<"), ("lshr",">>"), ("asha",">>")
              ]

condition :: [(String, String)]
condition= [("eq","=="), ("ne","!="),
            ("ugt",">"), ("uge",">="),
            ("ult","<"), ("ule","<="),
            ("sgt",">"), ("sge",">="),
            ("slt","<"), ("sle","<="),
            ("oeq","=="), ("one","!="),
            ("ogt",">"), ("oge",">="),
            ("olt","<"), ("ole","<="),
            ("ueq","=="), ("une","!="),
            ("ugt",">"), ("uge",">="),
            ("ult","<"), ("ule","<="),
            ("ord", ""), ("uno", ""),
            ("true", ""), ("false", "")
            ]

getInstructionType ::  String -> String
getInstructionType op
  | isNothing found_op = "none"
  | otherwise = fromJust found_op
  where found_op = lookup op (map swap instructions)

toSym ::  String -> String
toSym op
  | isNothing symOP = "call"
  | otherwise = fromJust symOP
  where symOP = lookup op (arithmetic ++ condition)
