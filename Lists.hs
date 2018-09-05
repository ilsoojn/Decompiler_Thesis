module Lists where

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

regexLine_fn, regexEnd_fn, regexLine_bb, regex_semi1, regex_fn, regex_bb :: String
str_main, str_fn, str_bb, line_bb, strStart_fn, strStart_bb:: String
regex_rsp, regex_padding, regex_semi2, regex_array, regex_sync, regex_pad, regex_landpad, regex_fbpadding, regex_fb, regex_ab :: String
regexLine_fn = "^define void @fn_(.*)\\(.*\\{"
regexEnd_fn = "^}"
regexLine_bb = "^bb_(.*):.*"
regex_semi1 = "(.*): [^%](.*)"
regex_fn = ".*?@fn_(.*)\\(.*\\{"
regex_bb = ".*?%bb_(.*)\\ .*"

strStart_fn = "define void @fn_"
strStart_bb = "bb_"

str_main = "@main"
str_fn = "@fn_"
str_bb = "%bb_"
str_var = "%"
line_bb = "; <label>:bb_"

regex_rsp = ".* = .* %RSP_(.*),\\ (.*)"
regex_padding = "(.*)%([0-9]*)(.*)"

regex_semi2 = "%(.*) : %(.*)"
regex_array = ".*?[(.*)] [(.*)](.*)"
regex_sync = "\\((\".*\")\\)"
regex_pad = "(.*)[(.*)]"
regex_landpad = ".*?\\{(.*)\\}(.*)"
regex_fb = ".*%([0-9a-zA-Z\\_\\-]*).*"
regex_ab = ".* = %([0-9a-zA-Z\\_\\-\\+]*)"

-- regex expression for FRONT & BACK padding
regex_fbpadding= "(.*)%[0-9a-zA-Z\\_\\-]*(.*)"
regex_after_padding = "[0-9a-zA-Z\\_\\-]*(.*)"


-- regex_semi, regex_array, regex_sync, regex_pad, regex_landpad::String
-- regex_semi = "%(.*) : %(.*)"
-- regex_array = ".*?[(.*)] [(.*)](.*)"
-- regex_sync = "\\((\".*\")\\)"
-- regex_pad = "(.*)[(.*)]"
-- regex_landpad = ".*?\\{(.*)\\}(.*)"
-- regex_fbpadding= "(.*)%[0-9a-zA-Z\\_\\-]*(.*)"
-- regex_fb = ".*%([0-9a-zA-Z\\_\\-]*).*"
-- regex_ab = ".* = %([0-9a-zA-Z\\_\\-\\+]*)"
