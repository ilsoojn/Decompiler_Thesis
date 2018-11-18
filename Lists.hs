module Lists where

import Data.List
import Data.Ord

{-************************************************************************
                    Archtecture Registers and Pointer
  *************************************************************************-}

reg_8, reg_16, reg_32, reg_64, reg_other, reg_ip :: [String]
reg_8 = ["%AH","%AL","%BH","%BL","%CH","%CL","%DH","%DL","%SIL","%DIL","%SPL","%BPL", "%R8B", "%R9B", "%R10B", "%R11B", "%R12B", "%R13B", "%R14B", "%R15B"]
reg_16 = ["%AX","%BX","%CX","%DX","%SI","%DI","%SP","%BP", "%R8W", "%R9W", "%R10W", "%R11W", "%R12W", "%R13W", "%R14W", "%R15W"]
reg_32 = ["%EAX","%EBX","%ECX","%EDX","%ESI","%EDI","%ESP","%EBP","%EFLAGS", "%R8D", "%R9D", "%R10D", "%R11D", "%R12D", "%R13D", "%R14D", "%R15D"]
reg_64 = ["%RAX","%RBX","%RCX","%RDX","%RSI","%RDI","%RSP","%RBP","%RFLAGS", "%R8", "%R9", "%R10", "%R11", "%R12", "%R13", "%R14", "%R15"]
reg_other = ["%XMM0","%XMM1","%XMM2","%YMM0","%YMM1","%YMM2","%ZMM0","%ZMM1","%ZMM2"]
reg_ip = ["%IP","%EIP","%RIP"]

flags = ["%CtlSysEFLAGS", "%EFLAGS"]

reg_base = reg_32 ++ reg_64 ++ reg_ip ++ reg_other ++ flags

{-************************************************************************
                            LLVM INSTRUCTIONS
  *************************************************************************-}

instructionList :: [(String, [String])] -> [(String, String)]
instructionList [] = []
instructionList (x:xs)= ((zip.repeat) (fst x) (snd x))++(instructionList xs)

instructions :: [(String, String)]
instructions = instructionList [("terminator", ["ret", "br", "switch", "indirectbr", "invoke", "resume", "catchswitch", "catchret", "cleanupret", "unreachable"]),
        ("binary", ["add", "fadd", "sub", "fsub", "mul", "fmul", "udiv", "sdiv", "fdiv", "urem", "srem", "frem"]),
        ("bitwise", ["shl", "lshr", "ashr", "and", "or", "xor"]),
        ("vector", ["extractelement", "insertelement", "shufflevector"]),
        ("aggregate", ["extractvalue", "insertvalue"]),
        ("memory", ["alloca", "load", "store", "fence", "cmpxchg", "atomicrmw", "getelementptr"]),
        ("conversion", ["trunc", "zext", "sext", "fptrunc", "fpext", "fptoui", "fptosi", "uitofp", "sitofp", "ptrtoint", "inttoptr", "bitcast", "addrspacecast"]),
        ("other", ["icmp", "fcmp", "phi", "select", "call", "va_arg", "landingpad", "catchpad", "cleanuppad"])]

{-************************************************************************
                              SYMBOLS
  *************************************************************************-}

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


{-************************************************************************
                          Variable Naming
  *************************************************************************-}

names = sortBy (comparing length) $ sort (filter (not.null) (subsequences "abcdefghijk"))

{-************************************************************************
                            REGEX EXPRESSIONS
  *************************************************************************-}

-- regexInit_fn, regexInit_bb, regex_fn, regex_bb, regex_block, regexEnd_fn, start_fn, start_bb::String
-- regexInit_fn = "^define void @fn_(.*)\\(.*\\{"
-- regexEnd_fn = "^}"
-- regexInit_bb = "^bb_(.*):.*"
-- regex_block = "(.*): [^%](.*)"
-- regex_fn = ".*?@fn_(.*)\\(.*\\{"
-- regex_bb = ".*?%bb_(.*)\\ .*"
--
-- start_fn = "define void @fn_"
-- start_bb = "bb_"

regexLine_fn, regexEnd_fn, regexLine_bb, regex_fn, regex_bb, regex_bb_pred :: String
regexLine_fn = "^define void @fn_(.*)\\(.*\\{"
regexEnd_fn = "^}"
regexLine_bb = "^bb_(.*):.*"
regexLine_label = "^; <label>:(.*):.*"
regex_fn = ".*?@fn_(.*)\\(.*\\{"
regex_bb = ".*?%bb_(.*)\\ .*"
regex_bb_pred = ".*preds = (.*)"
regex_asm = "(.*) <(.*)>:"

str_main, str_fn, str_bb, line_bb, strStart_fn, strStart_bb:: String
strStart_fn = "define void @fn_"
strStart_bb = "bb_"
str_main = "@main"
str_fn = "@fn_"
str_bb = "%bb_"
str_var = "%"
line_bb = "; <label>:bb_"

regex_useRsp, regex_useEsp, regex_padding, regex_colon :: String
regex_generalVariable = "([0-9A-Z\\%\\+\\_\\-]*)"
regex_loadVariable = "\\[([0-9A-Z\\%\\+\\_\\-]*)\\]"

regex_useRsp = ".* = .*%R([0-9a-zA-Z\\_\\-\\+]*).*"
regex_useEsp = ".* = .*%E([0-9a-zA-Z\\_\\-\\+]*).*"
regex_padding = "(.*)%([0-9]*)(.*)"
regex_colon = "\\[?([0-9A-Z\\%\\+\\_\\-]*)\\]? : \\[?([0-9A-Z\\%\\+\\_\\-]*)\\]?"
regex_colon2 = "([0-9A-Z\\%\\+\\_\\-]*) : ([0-9A-Z\\%\\+\\_\\-]*)"

regex_array, regex_sync, regex_pad, regex_landpad :: String
regex_array = ".*?[(.*)] [(.*)](.*)"
regex_sync = "\\((\".*\")\\)" -- syncscope
regex_pad = "(.*)[(.*)]"
regex_landpad = ".*?\\{(.*)\\}(.*)"

regex_fbpadding, regex_fb, regex_var :: String
regex_fb = ".*%([0-9a-zA-Z\\_]*).*"
regex_var = ".*%([0-9a-zA-Z\\+\\_\\-]*).*"
regex_fbpadding= "(.*)%[0-9a-zA-Z\\+\\_\\-]*(.*)"
regex_after_padding = "[0-9a-zA-Z\\_\\-]*(.*)"

regex_equal, regex_equalLoad, regex_rhs, regex_rhsLoad :: String
regex_equal = ".* = %([0-9a-zA-Z\\+\\_\\-]*)"
regex_equalLoad = ".* = \\[([0-9a-zA-Z\\%\\+\\_\\-]*)\\]"
regex_equalNumber = ".* = ([0-9\\+\\-]*)"

regex_rhs = "%([a-zA-Z0-9\\+\\%\\_\\-]*)"
regex_rhsLoad = "\\[([0-9a-zA-Z\\%\\+\\_\\-]*)\\]"
regex_rhsNumber = "([0-9\\+\\-]*)"
