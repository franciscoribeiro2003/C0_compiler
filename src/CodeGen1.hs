module CodeGen1 where

import           AST
import           IR
import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map

-- symbol table mapping variables to temporaries
type Table = Map String String

-- the "supply" for temporaries and labels
-- just two counters for names already used
type Supply = (Int, Int)

newTemp :: State Supply Temp
newTemp = do
  (temp, label) <- get
  put (temp + 1, label)
  return $ "t" ++ show temp

newLabel :: State Supply Label
newLabel = do
  (temp, label) <- get
  put (temp, label + 1)
  return $ "L" ++ show label

insertVar :: Table -> String -> Temp -> State Supply Table
insertVar table var temp = do
  return $ Map.insert var temp table

extendTable :: Table -> [(Temp, String)] -> Table
extendTable table [] = table
extendTable table ((temp, var):xs) = extendTable (Map.insert var temp table) xs

popTemp :: Int -> State Supply ()
popTemp n = do
  (temp, label) <- get
  put (temp - n, label)

getTemp :: State Supply Int
getTemp = do
  (temp, label) <- get
  return temp

transAst :: Table -> [Function] -> State Supply [FuncIR]
transAst table [] = return []
transAst table (first:xs) = do
    temp <- transFunc table first
    n <- getTemp
    popTemp n
    temp1 <- transAst table xs
    return $ temp : temp1

transFunc :: Table -> Function -> State Supply FuncIR
transFunc table (Function t name decls block) = do
    table1 <- getDeclFunction table decls
    table2 <- getDecl table1 (StatementsBlock block)
    dclList <- getDeclList table2 decls
    code1 <- transBlock table2 block
    return $ FUNCIR name dclList code1

getDeclFunction :: Table -> [Declaration] -> State Supply Table
getDeclFunction table [] = return table
getDeclFunction table (first:xs) = do
    table1 <- getDeclFuncAux table first
    table2 <- getDeclFunction table1 xs
    return table2

getDeclFuncAux :: Table -> Declaration -> State Supply Table
getDeclFuncAux table (t, name) = do
    temp <- newTemp
    let table1 = extendTable table [(temp, name)]
    return table1

getDecl :: Table -> Statement -> State Supply Table
getDecl table (StatementsBlock []) = return table
getDecl table (StatementsBlock (first:xs)) = do
    table1 <- getDeclAux table first
    table2 <- getDecl table1 (StatementsBlock xs)
    return table2

getDeclList :: Table -> [Declaration] -> State Supply [Temp]
getDeclList table [] = return []
getDeclList table ((t, name):xs) = case Map.lookup name table of
    Just temp -> do
        temp1 <- getDeclList table xs
        return $ temp : temp1
    Nothing -> error "Variable not found"

getDeclAux :: Table -> Statement -> State Supply Table
getDeclAux table stm = case stm of
    VarOp (Declaration t name exp) -> do
        temp <- newTemp
        let table1 = extendTable table [(temp, name)]
        return table1
    VarOp (Init t name) -> do
        temp <- newTemp
        let table1 = extendTable table [(temp, name)]
        return table1
    VarOp (AssignStm name exp) -> case Map.lookup name table of
        Just temp -> return table
        Nothing -> error "Variable not found"
    (StatementsBlock stm1) -> do
        table' <- getDecl table (StatementsBlock stm1)
        return table'
    stm1 -> return table

transStatement :: Table -> Statement -> State Supply [Instr]
transStatement table (VarOp (Init t name)) = case Map.lookup name table of
    Just temp -> return[]
    Nothing -> error "Variable not found"

transStatement table (VarOp (Declaration t name (Num n))) = case Map.lookup name table of
    Just temp -> do
        code1 <- transStatement table (VarOp (Init t name))
        return $ code1 ++ [MOVEI temp n]
    Nothing -> error "Variable not found"

transStatement table (VarOp (Declaration t name exp)) = case Map.lookup name table of
  Just temp -> do
    code1 <- transStatement table (VarOp (Init t name))
    tempexpr <- newTemp
    code2 <- transExp table exp tempexpr
    return $ code1 ++ code2 ++ [MOVE temp tempexpr]
  Nothing -> error "Variable not found"

transStatement table (VarOp (AssignStm name (Num n))) = case Map.lookup name table of
    Just temp -> do temp1 <- newTemp
                    return [MOVEI temp1 n]

transStatement table (VarOp (AssignStm name exp)) = case Map.lookup name table of
    Just temp -> do
        tempexpr <- newTemp
        code1 <- transExp table exp tempexpr
        return $ code1 ++ [MOVE temp tempexpr]
    Nothing -> error "Variable not found"

transStatement tabl (If cond stm)
  = do labtrue <- newLabel
       labfalse <- newLabel
       code0 <- transCond tabl cond labtrue labfalse
       table <- getDecl tabl (StatementsBlock [stm])
       code1 <- transStatement table stm
       return (code0 ++ [LABEL labtrue] ++ code1 ++ [LABEL labfalse])

transStatement tabl (IfElse cond stm1 stm2)
   = do labtrue <- newLabel
        labfalse <- newLabel
        labend <- newLabel
        code0 <- transCond tabl cond labtrue labfalse
        table1 <- getDecl tabl (StatementsBlock [stm1])
        code1 <- transStatement table1 stm1
        table2 <- getDecl tabl (StatementsBlock [stm2])
        code2 <- transStatement table2 stm2
        return (code0 ++ [LABEL labtrue] ++ code1 ++ [JUMP labend, LABEL labfalse] ++ code2 ++ [LABEL labend])

transStatement tabl (For decl cond op stm)
  = do labtrue <- newLabel
       labloop <- newLabel
       labend <- newLabel
       (code0, tabl') <- transOpFor tabl decl
       code1 <- transCond tabl' cond labtrue labend
       code2 <- transOp tabl' op
       tabl1 <- getDecl tabl' (StatementsBlock [stm])
       code3 <- transStatement tabl1 stm
       return (code0 ++ [LABEL labloop] ++
               code1 ++ [LABEL labtrue] ++ code3 ++ code2 ++
               [JUMP labloop, LABEL labend])

transStatement tabl (While cond stm)
  = do labtrue <- newLabel
       labloop <- newLabel
       labend <- newLabel
       code1 <- transCond tabl cond labtrue labend
       table <- getDecl tabl (StatementsBlock [stm])
       code2 <- transStatement table stm
       return ([LABEL labloop] ++ code1 ++
               [LABEL labtrue] ++ code2 ++
               [JUMP labloop, LABEL labend])

transStatement tabl (StatementsBlock stms)
  = do code1 <- transBlock tabl stms
       return code1

transStatement tabl (Return expr)
  = do temp <- newTemp
       popTemp(1)
       code <- transExp tabl expr temp
       return (code ++ [RETURN temp])

transStatement tabl (PrintInt expr)
  = do temp <- newTemp
       popTemp(1)
       code <- transExp tabl expr temp
       return (code ++ [PRINTINT temp])

transStatement tabl (PrintStr (Str str))
  = do temp <- newTemp
       popTemp(1)
       let code = [MOVE temp str]
       return (code ++ [PRINTSTR temp])

transStatement tabl (PrintStr expr)
  = do temp <- newTemp
       popTemp(1)
       code <- transExp tabl expr temp
       return (code ++ [PRINTSTR temp])

transStatement tabl (FunctionCallStm func expr)
  = do (code1, temps) <- transExps tabl expr
       return (code1 ++ [CALL func (temps)])

transBlock :: Table -> [Statement] -> State Supply [Instr]
transBlock table [] = return []
transBlock table (first:xs) = do
    code1 <- transStatement table first
    code2 <- transBlock table xs
    return $ code1 ++ code2

transExp :: Table -> Exp -> Temp -> State Supply [Instr]
transExp table (Num n) temp = return [MOVEI temp n]

transExp table (BooleanConst b) temp = return [MOVEI temp (if b then 1 else 0)]

transExp table (Str str) temp = case Map.lookup str table of
    Just temp -> return [MOVE temp temp]
    Nothing -> error "Variable not found"

transExp table (Var str) temp = case Map.lookup str table of
    Just temp -> return [MOVE temp temp]
    Nothing -> error "Variable not found"

transExp table (Op op exp1 exp2) temp = do
    temp1 <- newTemp
    temp2 <- newTemp
    code1 <- transExp table exp1 temp1
    code2 <- transExp table exp2 temp2
    popTemp(2)
    return $ code1 ++ code2 ++ [OP op temp temp1 temp2]

transExp table ScanIntExp temp = return [SCANINT]

transExp table (FunctionCallExp func expr) temp = do
    (code1, temps) <- transExps table expr
    return $ code1 ++ [CALL func (temps)]

transExps :: Table -> [Exp] -> State Supply ([Instr], [Temp])
transExps table [] = return ([], [])
transExps table (first:xs) = do
    temp1 <- newTemp
    code1 <- transExp table first temp1
    popTemp(1)
    (code2, temps) <- transExps table xs
    return (code1 ++ code2, temp1 : temps)

transCond :: Table -> CompareExpression -> Label -> Label -> State Supply [Instr]
transCond table (Comp op exp1 exp2) labtrue labfalse = do
    temp1 <- newTemp
    temp2 <- newTemp
    code1 <- transExp table exp1 temp1
    code2 <- transExp table exp2 temp2
    popTemp(2)
    return $ code1 ++ code2 ++ [COND temp1 op temp2 labtrue labfalse]

transCond table (And cond1 cond2) labtrue labfalse = do
    lab1 <- newLabel
    code1 <- transCond table cond1 lab1 labfalse
    code2 <- transCond table cond2 labtrue labfalse
    return $ code1 ++ [LABEL lab1] ++ code2

transCond table (Or cond1 cond2) labtrue labfalse = do
    lab1 <- newLabel
    code1 <- transCond table cond1 labtrue lab1
    code2 <- transCond table cond2 labtrue labfalse
    return $ code1 ++ [LABEL lab1] ++ code2

transCond table (Not cond) labtrue labfalse = do
    code1 <- transCond table cond labfalse labtrue
    return code1

transCond table (BooleanCond b) labtrue labfalse = return [JUMP (if b then labtrue else labfalse)]

transOp :: Table -> Operation -> State Supply [Instr]
transOp table (PosIncrement var) = case Map.lookup var table of
  Just temp -> return [OPI Add temp temp 1]
  Nothing -> error "Variable not found"

transOp table (PosDecrement var) = case Map.lookup var table of
  Just temp -> return [OPI Minus temp temp 1]
  Nothing -> error "Variable not found"

transOp table (PreIncrement var) = case Map.lookup var table of
  Just temp -> return [OPI Add temp temp 1]
  Nothing -> error "Variable not found"

transOp table (PreDecrement var) = case Map.lookup var table of
  Just temp -> return [OPI Minus temp temp 1]
  Nothing -> error "Variable not found"

transOpFor :: Table -> ForOperation -> State Supply ([Instr], Table)
transOpFor table (ForDeclaration t name exp) = do
  temp1 <- newTemp
  temp2 <- newTemp
  temp3 <- newTemp
  table1 <- insertVar table name temp1
  code0 <- transExp table1 (Var name) temp2
  code1 <- transExp table1 exp temp3
  return $ ((code0 ++ code1), table1)

transOpFor table (ForAssign name exp) = case Map.lookup name table of
  Just temp -> do
    temp1 <- newTemp
    code0 <- transExp table exp temp1
    return $ (code0, table)
  Nothing -> error "Variable not found"

transOpFor table EmptyFor = return ([], table)
