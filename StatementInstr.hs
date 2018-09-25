module StatementInstr where

import Data.Maybe
import Data.Tuple
import OtherFunction

import Debug.Trace

{-************** Format and Regex **************-}

data RP = RP{rname::String, rbase::String, ridx::Integer, rstate:: String, permit::Bool} deriving (Ord, Eq, Show, Read)
data LeftVar = LeftVar{variable::String, vtype::String, instruction::String, state::String} deriving (Show, Read, Eq, Ord)
isLeftVar (LeftVar _ _ _ _) = True

data Clause = Catch {cty::String, cvalue::String} | Filter {cty::String, cvalue::String} | CleanUp deriving (Show)-- Catch <= single val, Filter <= Array
data Chain = Chain {v::String, def::(Integer, VAR), use::[(Integer, VAR)]} deriving (Show)

-- data LOAD = LOAD {ld_ty::String, rsp::String, index::Integer}
data VAR = Undef {value::String}
          | Const {value::String}
          | SemiColon {high::String, low::String}
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
          -- Aggregate
          | Array {op::String, agg_ty::String, value::String, e_ty::Maybe String, e::Maybe String, e_idx::[String]} -- agg_ty : [n x ty] <- [n x [n x ty]]
          | Struct {op::String, agg_ty::String, value::String, e_ty::Maybe String, e::Maybe String, e_idx::[String]} -- agg_ty : {ty} <- {ty, {ty}}
          -- Memory
          | Alloca {inalloca::Bool, ty::String, alloc_numE::Maybe String, alloc_align::Maybe String, alloc_addrspace::Maybe String}
          | LOAD {atomic::Bool, volatile::Bool, ty::String, ptr::String, alignment::Maybe String, syncscope::Maybe String, order::Maybe String}
          | Fence {syncscope::Maybe String, ordering::String} -- <<
          | Cmpxchg {weak::Bool, volatile::Bool, ty::String, ptr::String, cmp::String, new::String, syncscope::Maybe String, succ_ordering::String, fail_ordering::String} -- <<
          | Atomicrmw {volatile::Bool, operation::String, ty::String, ptr::String, value::String, syncscope::Maybe String, ordering::String} -- <<
          | GetElemPtr {inbound::Bool, ty::String, ptr::String, element::[(Bool, (String, String))]}
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
isSemicolon (SemiColon _ _) = True
isSemicolon _ = False
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
isArray (Array _ _ _ _ _ _) = True
isArray _ = False
isStruct (Struct _ _ _ _ _ _) = True
isStruct _ = False
isAlloca (Alloca _ _ _ _ _) = True
isAlloca _ = False
isLoad (LOAD _ _ _ _ _ _ _) = True
isLoad _ = False
isFence (Fence _ _) = True
isFence _ = False
isCmpxchg (Cmpxchg _ _ _ _ _ _ _ _ _) = True
isCmpxchg _ = False
isAtomicrmw (Atomicrmw _ _ _ _ _ _ _) = True
isAtomicrmw _ = False
isGetElemPtr (GetElemPtr _ _ _ _) = True
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
