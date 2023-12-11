module CodeGen2 where

import IR
import AST

printCode :: [FuncIR] -> IO ()
printCode function = let code = machineCode function
                     in putStrLn code

machineCode :: [FuncIR] -> String
machineCode [] = ""
machineCode (x:xs) = let code0 = genMachineCodeAux x
                         code1 = machineCode xs
                        in code0 ++ code1

genMachineCodeAux :: FuncIR -> String
genMachineCodeAux (FUNCIR name arg instr) = let l = length arg * (-4)
                                                pre = "\tsw $fp, -4($sp)\n" ++
                                                      "\tsw $ra, -8($sp)\n" ++
                                                      "\tla $fp, 0($sp)\n" ++
                                                      "\tla $sp, " ++ show l ++ "($sp)\n"
                                                args = genArg arg 0
                                                code = genFunctionCode instr
                                                post = "tla $sp, 0($fp)\n" ++
                                                       "\tlw $ra, -8($sp)\n" ++
                                                       "\tlw $fp, -4($sp)\n" ++
                                                       "\tjr $ra\n"
                                                in name ++ ":\n" ++ pre ++ args ++ "\n" ++ code ++ post

genArg :: [Temp] -> Int -> String
genArg [] _ = ""
genArg (x:xs) n = let code0 = "\tsw $" ++ x ++ ", " ++ show n ++ "($fp)\n"
                      code1 = genArg xs (n + 4)
                  in code0 ++ code1

genFunctionCode :: [Instr] -> String
genFunctionCode [] = ""
genFunctionCode (x:xs) = let code0 = genFuncCodeAux x
                             code1 = genFunctionCode xs
                         in code0 ++ code1

genFuncCodeAux :: Instr -> String
genFuncCodeAux (COND t1 op t2 l1 l2) = case op of
                    Lowert -> "\tblt $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1 ++ "\n" ++
                                "\tb " ++ l2 ++ "\n"
                    Greatert -> "\tbgt $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1 ++ "\n" ++
                                "\tb " ++ l2 ++ "\n"
                    Lowereq -> "\tble $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1 ++ "\n" ++
                                "\tb " ++ l2 ++ "\n"
                    Greatereq -> "\tbge $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1 ++ "\n" ++
                                    "\tb " ++ l2 ++ "\n"
                    Equal -> "\tbeq $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1 ++ "\n" ++
                                "\tb " ++ l2 ++ "\n"
                    Nequal -> "\tbne $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1 ++ "\n" ++
                                "\tb " ++ l2 ++ "\n"

genFuncCodeAux (OP op t1 t2 t3) = case op of
                                    Mult ->  "\tmul $" ++ t1 ++ ", $" ++ t2 ++ ", $" ++ t3 ++ "\n"
                                    Add ->   "\tadd $" ++ t1 ++ ", $" ++ t2 ++ ", $" ++ t3 ++ "\n"
                                    Minus -> "\tsub $" ++ t1 ++ ", $" ++ t2 ++ ", $" ++ t3 ++ "\n"
                                    Mod ->   "\trem $" ++ t2 ++ ", $" ++ t3 ++ "\n" ++
                                             "\tmfhi $" ++ t1 ++ "\n"
                                    Div ->   "\tdiv $" ++ t2 ++ ", $" ++ t3 ++ "\n" ++
                                             "\tmflo $" ++ t1 ++ "\n"

genFuncCodeAux (PRINTINT t1) = "\tmove $v0" ++ ", $" ++ t1 ++ "\n\tlw $a0, 0($sp)\n\tsyscall\n\tjr $ra\n"

genFuncCodeAux (PRINTSTR t1) = "\tla $a0, " ++ t1 ++ "\n\tsyscall\n\tjr $ra\n"

genFuncCodeAux (OPI Add t1 t2 n) = "\taddi $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ show n ++ "\n"

genFuncCodeAux (MOVE t1 t2) = "\tmove $" ++ t1 ++ ", $" ++ t2 ++ "\n"

genFuncCodeAux (MOVEI t1 n) = "\tli $" ++ t1 ++ ", " ++ show n ++ "\n"

genFuncCodeAux (JUMP l) = "\t j" ++ l ++ "\n"

genFuncCodeAux (RETURN t) = "\tmove $v0, $" ++ t ++ "\n"

genFuncCodeAux (LABEL l) = l ++ ":\n"
