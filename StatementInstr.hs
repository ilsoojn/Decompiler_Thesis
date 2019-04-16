module StatementInstr where

import Data.Maybe
import Data.Tuple

import Debug.Trace

data Function = Function{fname::String, retType::String, blocks::[BasicBlock], registers::[RP], variables::[LeftVar], code::[String]}
data BasicBlock = BasicBlock{blockName::String, preds::[String], txt::[String], succs::[String]} deriving (Ord, Eq, Show, Read)

{- DEF ( VARIABLE ) -}
data RP = RP{rname::String, rbase::String, ridx::Integer, rstate:: String, permit::Bool} deriving (Ord, Eq, Show, Read)
data LeftVar = LeftVar{variable::String, vtype::String, instruction::String, state::String} deriving (Show, Read, Eq, Ord)
isLeftVar (LeftVar _ _ _ _) = True
isRP (RP _ _ _ _ _) = True

{- STATEMENT -}

data Clause = Catch {cty::String, cvalue::String} | Filter {cty::String, cvalue::String} | CleanUp deriving (Show)-- Catch <= single val, Filter <= Array
data Chain = Chain {v::String, def::(Integer, VAR), use::[(Integer, VAR)]} deriving (Show)

data VAR = Undef {instrType::String, op::String, value::String}
          | Const {instrType::String, op::String, value::String}
          | Colon {instrType::String, op::String, high::String, low::String}
          -- Terminator
          | RetVoid {instrType::String, op::String}
          | Ret {instrType::String, op::String, ty::String, value::String} -- <<
          | Branch {instrType::String, op::String, label::String} -- <<
          | CondBranch {instrType::String, op::String, cond::String, trueLabel::String, falseLabel::String} -- <<
          | IndirBranch {instrType::String, op::String, ty::String, addr::String, labels::[String]} -- <<
          | Switch {instrType::String, op::String, ty::String, value::String, label::String, table::[(String, String)]} -- <<
          -- | Invoke {ccov::Maybe String, retAttrs::Maybe String, ty::String, f_ptr::String, f_arg::[String], f_attr::Maybe [String], bundle::Maybe [String], normLabel::String, excLabel::String}
          | Resume {instrType::String, op::String, ty::String, value::String} -- <<
          | CatchSwitch {instrType::String, op::String, parent::String, handlers::[String], unwind::String} -- <<
          | CatchRet {instrType::String, op::String, token::String, normal::String} -- <<
          | CleanUpRet {instrType::String, op::String, value::String, unwind::String} -- <<
          | Unreachable {instrType::String, op::String}
          -- Bitwise & Binary
          | Binary {instrType::String, op::String, sym::String, ty::String, values::[String]}  -- ***
          | Bit {instrType::String, op::String, sym::String, ty::String, values::[String]} -- ***
          -- Aggregate
          | Array {instrType::String, op::String, agg_ty::String, value::String, e_ty::Maybe String, e::Maybe String, e_idx::[String]} -- agg_ty : [n x ty] <- [n x [n x ty]]
          | Struct {instrType::String, op::String, agg_ty::String, value::String, e_ty::Maybe String, e::Maybe String, e_idx::[String]} -- agg_ty : {ty} <- {ty, {ty}}
          -- Memory
          | Alloca {instrType::String, op::String, inalloca::Bool, ty::String, alloc_numE::Maybe String, alloc_align::Maybe String, alloc_addrspace::Maybe String} -- ***
          | STORE {instrType::String, op::String, atomic::Bool, volatile::Bool, ty::String, value::String, at::String}
          | LOAD {instrType::String, op::String, atomic::Bool, volatile::Bool, ty::String, ptr::String, alignment::Maybe String, syncscope::Maybe String, order::Maybe String} -- ***
          | Fence {instrType::String, op::String, syncscope::Maybe String, ordering::String} -- <<
          | Cmpxchg {instrType::String, op::String, weak::Bool, volatile::Bool, ty::String, ptr::String, cmp::String, new::String, syncscope::Maybe String, succ_ordering::String, fail_ordering::String} -- <<
          | Atomicrmw {instrType::String, op::String, volatile::Bool, operation::String, ty::String, ptr::String, value::String, syncscope::Maybe String, ordering::String} -- <<
          | GetElemPtr {instrType::String, op::String, inbound::Bool, ty::String, ptr::String, element::[(Bool, (String, String))]}  -- ***
          -- Converison
          | Conv {instrType::String, op::String, ty1::String, ty::String, value::String} -- ***
          -- Other
          | Cmpi {instrType::String, op::String, cond::String, sym::String, ty::String, values::[String]}  -- ***
          | Cmpf {instrType::String, op::String, cond::String, flag::Maybe String, sym::String, ty::String, values::[String]}  -- ***
          | Phi {instrType::String, op::String, ty::String, vlabels::[(String, String)]} -- ***
          | Select {instrType::String, op::String, selty::String, cond::String, ty::String, values::[String]}
          -- | Call {tailing::Maybe String, flag::Maybe String, ccov::Maybe String, retAttrs::Maybe String, ty::String, f_ptr::String, f_arg::[String], f_attr::[String], bundle::[String], normLabel::String, unwindLabel::String}
          | VaArg {instrType::String, op::String, va_list::String, arglist::String, argty::String} -- ***
          | LandingPad {instrType::String, op::String, resultty::String, cleanup::Bool, clause::[Clause]}  -- ***
          | CatchPad {instrType::String, op::String, catchswitch::String, arg::String} -- ***
          | CleanUpPad {instrType::String, op::String, parent::String, arg::String}  -- ***
          -- undefined ones / temporary
          | Other{instrType::String, op::String} deriving (Show)

-- data STORE = STORE {instrType::String, op::String, atomic::Bool, volatile::Bool, ty::String, value::String, at::String} deriving (Show) -- str_v::[(String, VAR)], str_at::[(String, VAR)]} deriving (Show)

-- isUndef (Undef _ _ _) = True
-- isUndef _ = False
-- isConst (Const _ _ _) = True
-- isConst _ = False
-- isColon (Colon _ _ _ _) = True
-- isColon _ = False
isVoidRet (RetVoid _ _) = True
isVoidRet _ = False
isRet (Ret _ _ _ _) = True
isRet _ = False
isBranch (Branch _ _ _) = True
isBranch _ = False
isCondBranch (CondBranch _ _ _ _ _) = True
isCondBranch _ = False
isIndirBranch (IndirBranch _ _ _ _ _) = True
isIndirBranch _ = False
isSwitch (Switch _ _ _ _ _ _) = True
isSwitch _ = False
-- isInvoke (Invoke _ _ _ _ _ _ _ _ _) = True
-- isInvoke _ = False
-- isResume (Resume _ _ _ _) = True
-- isResume _ = False
-- isCatchSwitch (CatchSwitch _ _ _ _ _) = True
-- isCatchSwitch _ = False
-- isCatchRet (CatchRet _ _ _ _) = True
-- isCatchRet _ = False
-- isCleanUpRet (CleanUpRet _ _ _ _) = True
-- isCleanUpRet _ = False
-- isUnreachable(Unreachable _ _) = True
-- isUnreachable_ = False
isBinary (Binary _ _ _ _ _) = True
isBinary _ = False
isBitwise (Bit _ _ _ _  _) = True
isBitwise _ = False
-- isArray (Array _ _ _ _ _ _ _) = True
-- isArray _ = False
-- isStruct (Struct _ _ _ _ _ _ _) = True
-- isStruct _ = False
isAlloca (Alloca _ _ _ _ _ _ _) = True
isAlloca _ = False
isStore (STORE _ _ _ _ _ _ _) = True
isStore _ = False
isLoad (LOAD _ _ _ _ _ _ _ _ _) = True
isLoad _ = False
-- isFence (Fence _ _ _ _) = True
-- isFence _ = False
-- isCmpxchg (Cmpxchg _ _ _ _ _ _ _ _ _ _ _) = True
-- isCmpxchg _ = False
-- isAtomicrmw (Atomicrmw _ _ _ _ _ _ _ _ _) = True
-- isAtomicrmw _ = False
-- isGetElemPtr (GetElemPtr _ _ _ _ _ _) = True
-- isGetElemPtr _ = False
-- isConv (Conv _ _ _ _ _) = True
-- isConv _ = False
-- isCmpi (Cmpi _ _ _ _ _ _) = True
-- isCmpi _ = False
-- isCmpf (Cmpf _ _ _ _ _ _ _) = True
-- isCmpf _ = False
-- isPhi (Phi _ _ _ _) = True
-- isPhi _ = False
-- isSelect (Select _ _ _ _ _ _) = True
-- isSelect _ = False
-- isCall (Call _ _ _ _ _ _ _ _ _ _ _) = True
-- isCall _ = False
-- isVaArg (VaArg _ _ _ _ _) = True
-- isVaArg _ = False
-- isLandingPad (LandingPad _ _ _ _ _) = True
-- isLandingPad _ = False
-- isCatchPad (CatchPad _ _ _ _) = True
-- isCatchPad _ = False
-- isCleanUpPad (CleanUpPad _ _ _ _) = True
-- isCleanUpPad _ = False
isOther (Other _ _) = True
isOther _ = False
