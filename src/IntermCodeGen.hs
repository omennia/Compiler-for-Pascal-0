{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module IntermCodeGen where

import AST
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Type.Coercion (trans)
import IR

-- type environment (i.e. symbol table)
type Table = Map Identifier TableEntry

data TableEntry
  = Temp String
  | Constant Int
  | ARR String Int 
  deriving (Show, Eq, Ord)

type Count = (Int, Int, [(String, Label)]) -- contadores para temporários e etiquetas

newTemp :: State Count Temp
newTemp = do (t, l, strings) <- get; put (t + 1, l, strings); return ("t" ++ show t)

popTemp :: Int -> State Count ()
popTemp k = modify (\(t, l, strings) -> (t - k, l, strings))

getTemps :: State Count Int
getTemps = do (t, l, strings) <- get; put (t, l, strings); return (t-1)

newLabel :: State Count Label
newLabel = do (t, l, strings) <- get; put (t, l + 1, strings); return ("L" ++ show l)

newString :: String -> State Count Label
newString s =
  do
    (t, l, strings) <- get
    case lookup s strings of
      Nothing ->
        do
          sLabel <- newLabel
          put (t, l + 1, strings ++ [(s, sLabel)])
          return sLabel
      Just l ->
        return l

getStrings :: State Count [(String, Label)]
getStrings = do (t, l, strings) <- get; return strings

{- Auxiliary functions to extend environment with consts -}
declareConstants :: Table -> [DeclarationsForConsts] -> Table
declareConstants table [] = table
declareConstants table ((DefineConst x v) : rest) =
  declareConstants (Map.insert x (Constant v) table) rest

declareVariables :: [Instr] -> Table -> [DeclarationsForVars] -> State Count ([Instr], Table)
declareVariables code table [] = return (code, table)
declareVariables code table ((Declare arrName (TA (TypeArray c1 c2 t))) : rest) =
  do
    let begin = maybeConstantToInt c1 table
    let end = maybeConstantToInt c2 table
    temp <- newTemp
    let code' = [ALLOC temp ((end - begin + 1) * 4)]
    declareVariables (code ++ code') (Map.insert arrName (ARR temp begin) table) rest
declareVariables code table ((Declare x _) : rest) =
  do
    temp <- newTemp
    declareVariables code (Map.insert x (Temp temp) table) rest

extendParams :: Table -> [Param] -> State Count Table
extendParams table [] = return table
extendParams table ((Parameter param (TB typebasic)) : xs) =
  do
    temp <- newTemp
    extendParams (Map.insert param (Temp temp) table) xs
extendParams table ((Parameter arrayName (TA (TypeArray c1 _ _))) : xs) =
  do
    let begin = maybeConstantToInt c1 table
    temp <- newTemp
    extendParams (Map.insert arrayName (ARR temp begin) table) xs

insertInTable :: Table -> Identifier -> State Count Table
insertInTable table ident =
  case Map.lookup ident table of
    Nothing ->
      do
        temp <- newTemp
        return (Map.insert ident (Temp temp) table)
    Just (Constant constant) ->
      do
        temp <- newTemp
        return (Map.insert ident (Temp temp) table)
    Just (Temp temp) ->
      do
        return (Map.insert ident (Temp temp) table)

declareProcedures :: Table -> [DeclarationsForProcedures] -> State Count [Instr]
declareProcedures table [] = return []
declareProcedures table (Procedure (FunctionName ident paramList _) (ProcedureBody (DeclareVariables varDecls) statements) : rest) =
  do
    table' <- insertInTable table ident -- function name
    table'' <- extendParams table' paramList -- recursion / scope
    (arrayDeclsCode, table''') <- declareVariables [] table'' varDecls -- variaveis
    code <- transStatements table''' Nothing statements
    popTemp (length paramList + length varDecls + 1)
    code' <- declareProcedures table rest
    let params = transParameters table''' paramList
    return (FUNCTION ident params : arrayDeclsCode ++ code ++ [RETURN "t0"] ++ code')

declareProcedures table (Procedure (ProcedureName procName paramList) (ProcedureBody (DeclareVariables varDecls) statements) : rest) =
  do
    table' <- insertInTable table procName -- function name
    table'' <- extendParams table' paramList -- recursion / scope
    (arrayDeclsCode, table''') <- declareVariables [] table'' varDecls
    code <- transStatements table''' Nothing statements
    popTemp (length paramList + length varDecls + 1)
    code' <- declareProcedures table rest
    let params = transParameters table''' paramList
    return (FUNCTION procName params : arrayDeclsCode ++ code ++ [RETURN "t0"] ++ code')

transProgram table (StartProgram header (ProgBody (DeclareConstants constDecls) (DeclareProcedures procDecls) (DeclareVariables varDecls) statements)) =
  do
    let table' = declareConstants table constDecls
    code <- declareProcedures table' procDecls
    (code', table'') <- declareVariables [] table' varDecls
    -- Translate the program
    code'' <- transStatements table'' Nothing statements
    strings <- getStrings
    return ([DATA] ++ stringsInstr strings ++ [TEXT] ++ code' ++ code'' ++ [EXIT] ++ code)

stringsInstr :: [(String, Label)] -> [Instr]
stringsInstr [] = []
stringsInstr (str_lbl:xs) = [MAKESTR (fst str_lbl) (snd str_lbl)] ++ stringsInstr xs

transExpr :: Table -> Expr -> Identifier -> State Count [Instr]
-- Variable access
transExpr table (Access (Variable ident)) dest =
  case Map.lookup ident table of
    Just (Temp v) -> return [MOVE dest v]
    Just (Constant i) -> return [MOVEI dest i]
    Just (ARR temp begin) -> return [MOVE dest temp]
    Nothing -> error ("Invalid identifier " ++ show ident ++ " on expression: " ++ show (Access (Variable ident)) ++ "\n\n")

-- Array access
transExpr table (Access (Array ident expr)) dest =
  do
    (code, address) <- transIndex table ident expr
    popTemp 1
    case Map.lookup ident table of
      Nothing -> error "Array has not been declared."
      Just (ARR temp _) -> return (code ++ [MACCESS dest address])

-- Constant expressions
transExpr table (Num n) dest =
  return [MOVEI dest n]

-- Booleans
transExpr table BooleanTrue dest = 
  return [MOVEI dest 1]
transExpr table BooleanFalse dest =
  return [MOVEI dest 0]

transExpr table (BinOp op e1 e2) dest =
  do
    temp1 <- newTemp
    temp2 <- newTemp
    code1 <- transExpr table e1 temp1
    code2 <- transExpr table e2 temp2
    popTemp 2
    return (code1 ++ code2 ++ [BinaryOp op dest temp1 temp2])

-- Functions
transExpr table (Call ident arguments) dest =
  do
    (code, temps) <- transArgs table arguments
    popTemp (length arguments)
    tempsUsed <- usedTemps
    return (code ++ [CALL dest ident temps tempsUsed])

-- Unary Operations
transExpr table (UnaryOp op (Num n)) dest =
  do
    return [UnOpI op dest n]
transExpr table (UnaryOp op expr) dest =
  do
    temp1 <- newTemp
    code1 <- transExpr table expr temp1
    popTemp 1
    return (code1 ++ [UnOp op dest temp1])

-- Strings
transExpr table (StringLiteral string) dest =
  do
    label <- newString string
    return [MOVESTR dest label]

transStm :: Table -> Maybe Label -> Statement -> State Count [Instr]
-- Assign Statements
transStm table breakLabel (AssignStm (Variable ident) expr) =
  case Map.lookup ident table of
    Nothing ->
      error ("Invalid identifier " ++ show ident ++ "\n\n")
    Just (Constant c) ->
      error "Cannot assign a value to a constant\n\n"
    Just (Temp ident_temp) -> do
      transExpr table expr ident_temp

transStm table label BreakStm =
  do
    case label of
      Nothing -> error "Break statement not in a breakable scope."
      Just breakLabel -> return [JUMP breakLabel]

transStm table breakLabel (AssignStm (Array arrName expr1) expr2) =
  do
    (code1, address) <- transIndex table arrName expr1
    t <- newTemp
    code2 <- transExpr table expr2 t
    popTemp 2
    case Map.lookup arrName table of
      Nothing -> error "Array is undefined."
      Just (ARR arrayTemp _) -> return (code1 ++ code2 ++ [MPUT address t])

-- Conditions
transStm table breakLabel (IfStm cond stm1) =
  do
    ltrue <- newLabel
    lfalse <- newLabel
    code0 <- transCond table cond ltrue lfalse
    code1 <- transStm table breakLabel stm1
    return
      ( code0 ++ [LABEL ltrue]
          ++ code1
          ++ [LABEL lfalse]
      )

transStm table breakLabel (IfThenElseStm cond stm1 stm2) =
  do
    ltrue <- newLabel
    lfalse <- newLabel
    lend <- newLabel
    code0 <- transCond table cond ltrue lfalse
    code1 <- transStm table breakLabel stm1
    code2 <- transStm table breakLabel stm2
    return
      ( code0 ++ [LABEL ltrue] ++ code1
          ++ [JUMP lend, LABEL lfalse]
          ++ code2
          ++ [LABEL lend]
      )

transStm table breakLabel (WhileStm cond stm) =
  do
    lbody <- newLabel
    lend <- newLabel
    lcond <- newLabel
    code1 <- transStm table (Just lend) stm
    code2 <- transCond table cond lbody lend
    return
      ( [JUMP lcond, LABEL lbody] ++ code1
          ++ [LABEL lcond]
          ++ code2
          ++ [LABEL lend]
      )

transStm table breakLabel (ForStm ident expr1 expr2 statement) =
  do
    lbody <- newLabel
    lend <- newLabel
    lcond <- newLabel
    code1 <- transStm table (Just lend) statement
    code2 <- transCond table (BinOp LessEqualThan (Access (Variable ident)) expr2) lbody lend
    identTemp <- case Map.lookup ident table of
      Just (Temp v) -> return v
      Just (Constant i) -> error "Cannot use a constant as the for loop initializer.\n\n"
      Nothing -> error ("Invalid identifier " ++ show ident ++ "\n\n")
    codeSet <- transExpr table expr1 identTemp
    codeIncrement <- transStm table (Just lend) (AssignStm (Variable ident) (BinOp Sum (Access (Variable ident)) (Num 1)))
    return
      ( codeSet
          ++ [ JUMP lcond,
               LABEL lbody
             ]
          ++ code1
          ++ codeIncrement
          ++ [LABEL lcond]
          ++ code2
          ++ [LABEL lend]
      )

transStm table breakLabel (ProcedureStm ident arguments) =
  do
    (code, temps) <- transArgs table arguments
    popTemp (length arguments)
    tempsUsed <- usedTemps
    return (code ++ [CALL "zero" ident temps tempsUsed])

-- Compound Statements
transStm table breakLabel (CompoundStm statements) =
  do
    transStatements table breakLabel statements

transStatements table breakLabel = worker
  where
    worker [] = return []
    worker (statement : stmts) =
      do
        code <- transStm table breakLabel statement
        code' <- worker stmts
        return (code ++ code')

-- Helper Functions
transArgs table arguments = worker arguments
  where
    worker [] = return ([], [])
    worker (exp : exps) =
      do
        temp <- newTemp
        code <- transExpr table exp temp
        (code', temps') <- worker exps
        return (code ++ code', temp : temps')

transCond :: Table -> Expr -> Label -> Label -> State Count [Instr]
transCond table (BinOp relOp e1 e2) ltrue lfalse
  | relOp == LessThan || relOp == LessEqualThan || relOp == Equals || relOp == NotEquals =
    do
      temp1 <- newTemp
      temp2 <- newTemp
      code1 <- transExpr table e1 temp1
      code2 <- transExpr table e2 temp2
      popTemp 2
      return
        (code1 ++ code2 ++ [COND temp1 relOp temp2 ltrue lfalse])

transCond table (BinOp And e1 e2) ltrue lfalse =
  do
    label2 <- newLabel
    code1 <- transCond table e1 label2 lfalse
    code2 <- transCond table e2 ltrue lfalse
    return (code1 ++ [LABEL label2] ++ code2)

transCond table (BinOp Or e1 e2) ltrue lfalse =
  do
    label2 <- newLabel
    code1 <- transCond table e1 ltrue label2
    code2 <- transCond table e2 ltrue lfalse
    return (code1 ++ [LABEL label2] ++ code2)

transCond table (UnaryOp Not e1) ltrue lfalse =
  transCond table e1 lfalse ltrue

transCond table BooleanTrue ltrue _ =
  return [JUMP ltrue]

transCond table BooleanFalse _ lfalse =
  return [JUMP lfalse]

transCond table expr ltrue lfalse =
  do
    temp <- newTemp
    code1 <- transExpr table expr temp
    popTemp 1
    return (code1 ++ [COND temp NotEquals "0" ltrue lfalse])

transParameters :: Table -> [Param] -> [Temp]
transParameters _ [] = []
transParameters table ((Parameter ident _) : parameters) =
  case Map.lookup ident table of
    Nothing -> error "Parameter does not exist."
    Just (Temp temp) -> temp : transParameters table parameters
    Just (ARR temp begin) -> temp : transParameters table parameters
    Just anythingelse -> error ("Error on " ++ show anythingelse)

transIndex table ident exp =
  do
    let (base, begin) = case Map.lookup ident table of
          Nothing -> error "Array identifier has not been declared"
          Just (ARR temp begin) -> (temp, begin)
    address <- newTemp
    code <- transExpr table exp address
    let expr0 = BinaryOpI Sum address address (-begin)
    let expr1 = BinaryOpI Multiply address address 4
    let expr2 = BinaryOp Sum address address base
    
    return (code ++ [expr0, expr1, expr2], address)

maybeConstantToInt :: Constant -> Table -> Int
maybeConstantToInt (Int n) _ = n
maybeConstantToInt (Const var) table = do
  case Map.lookup var table of
    Nothing -> error ("Variable " ++ var ++ "has not been declared.")
    Just (Constant n) -> n

usedTemps :: State Count [Temp]
usedTemps = 
  do
    temps <- getTemps
    return (usedTempsAux temps)

usedTempsAux :: Int -> [Temp]
usedTempsAux (-1) = []
usedTempsAux n = usedTempsAux (n-1) ++ ["t" ++ show n]



    