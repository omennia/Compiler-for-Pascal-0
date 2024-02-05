{-
 Adapted from Prof. Pedro Vasconcelos'
 Typechecker for simple C-like imperative language
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Typecheck where

import           AST

import           Data.Map(Map)
import qualified Data.Map as Map

-- type environment (i.e. symbol table)
type TypeEnv = Map EnvKey Type

-- To save the constant values
type ConstMap = Map Constant Int

data EnvKey = Var Identifier
            | Fun Identifier
            | Void Identifier
            deriving (Show, Ord, Eq)

-------------------------------------------------------------------------------------------------
initialMap :: TypeEnv -- We start with the IO functions already in the environment
initialMap = Map.fromList [(Fun "READINT", TB (TypeFunction [] TypeInteger)),
                           (Fun "WRITEINT", TB (TypeProcedure [TB TypeInteger])),
                           (Fun "WRITESTR", TB (TypeProcedure [TB TypeString]))]
-------------------------------------------------------------------------------------------------


{- Main call, checks if a program is correct -}
checkProg :: Program -> Bool
checkProg (StartProgram header (ProgBody (DeclareConstants constDecls) (DeclareProcedures procDecls) (DeclareVariables varDecls) stms))
  = let constMap = saveConsts Map.empty constDecls
        env0 = extendEnvConsts initialMap constMap constDecls
        env1 = extendEnvProcs env0 constMap procDecls
        env2 = extendEnvVars env1 constMap varDecls
    in aux(map (checkStatement env2) stms)





saveConsts :: ConstMap -> [DeclarationsForConsts] -> ConstMap
saveConsts constValues [] = constValues
saveConsts constValues ((DefineConst constName constVal):xs) = saveConsts (Map.insert (Const constName) constVal constValues) xs

transform :: ConstMap -> Constant -> Constant
transform constMap (Int k) = Int k
transform constMap (Const k) = case Map.lookup (Const k) constMap of
    Nothing -> error ("Variable " ++ k ++ "has not been declared.")
    Just t -> Int t

{- Auxiliary functions to extend environment with vars -}
extendEnvVars :: TypeEnv -> ConstMap -> [DeclarationsForVars] -> TypeEnv
extendEnvVars env constMap [] = env
extendEnvVars env constMap ((Declare x (TA (TypeArray start end t))):rest) = extendEnvVars (Map.insert (Var x) (TA (TypeArray (transform constMap start) (transform constMap end) t)) env) constMap rest
extendEnvVars env constMap ((Declare x (TB t)):rest) = extendEnvVars (Map.insert (Var x) (TB t) env) constMap rest

{- Auxiliary functions to extend environment with consts -}
extendEnvConsts :: TypeEnv -> ConstMap -> [DeclarationsForConsts] -> TypeEnv
extendEnvConsts env constMap [] = env
extendEnvConsts env constMap ((DefineConst x _):rest) = extendEnvConsts (Map.insert (Var x) (TB TypeInteger) env) constMap rest


{- Auxiliary function to extract the types for extendEnvProcs -}
extractTypes :: ConstMap -> [Param] -> [Type]
extractTypes constMap [] = []
extractTypes constMap ((Parameter pname (TB ptype)):xs) = TB ptype : extractTypes constMap xs
extractTypes constMap ((Parameter pname (TA (TypeArray start end ptype))):xs) = TA (TypeArray (transform constMap start) (transform constMap end) ptype) : extractTypes constMap xs

{- Auxiliary function to extract the types of Parameters -}
extendParams :: TypeEnv -> ConstMap -> [Param] -> TypeEnv
extendParams env constMap [] = env
extendParams env constMap ((Parameter param (TB typebasic)):xs) = extendParams (Map.insert (Var param) (TB typebasic) env) constMap xs
extendParams env constMap ((Parameter vector (TA (TypeArray lo hi typebasic))):xs) = extendParams (Map.insert (Var vector) (TA (TypeArray (transform constMap lo) (transform constMap hi) typebasic)) env) constMap xs

extendEnvProcs :: TypeEnv -> ConstMap -> [DeclarationsForProcedures] -> TypeEnv
extendEnvProcs env constMap [] = env
{- Extend a function but first have to verify everything inside it!! -}
extendEnvProcs env constMap ((Procedure (FunctionName x paramlist t) (ProcedureBody (DeclareVariables varDecls) stms)):rest) =
  extendEnvProcs (checkFunction env constMap (Procedure (FunctionName x paramlist t) (ProcedureBody (DeclareVariables varDecls) stms))) constMap rest
{- Extend a procedure but first have to verify everything inside it!! -}
extendEnvProcs env constMap ((Procedure (ProcedureName x paramlist) (ProcedureBody (DeclareVariables varDecls) stms)):rest) =
  extendEnvProcs (checkProcedure env constMap (Procedure (ProcedureName x paramlist) (ProcedureBody (DeclareVariables varDecls) stms))) constMap rest

{- Debugging for when it does not work -}
extendEnvProcs env constMap (s:rest) = error ("Error trying to extend procedures environment on: " ++ show s ++ "\n")


checkFunction :: TypeEnv -> ConstMap -> DeclarationsForProcedures -> TypeEnv
checkFunction env constMap (Procedure (FunctionName x paramlist t) (ProcedureBody (DeclareVariables varDecls) stms)) =
  let env' = extendParams env constMap paramlist
      env'' = extendEnvVars env' constMap varDecls
      env''' = Map.insert (Var x) (TB t) env'' -- Takes care of return
      env'''' = extendEnv env''' constMap (Fun x) (TB (TypeFunction (extractTypes constMap paramlist) t)) -- Takes care of recursion
        in
    if checkCmpStm env'''' constMap stms then extendEnv env constMap (Fun x) (TB (TypeFunction (extractTypes constMap paramlist) t))
    else error "error in checkFunction"

checkProcedure :: TypeEnv -> ConstMap -> DeclarationsForProcedures -> TypeEnv
checkProcedure env constMap (Procedure (ProcedureName x paramlist) (ProcedureBody (DeclareVariables varDecls) stms)) =
  let env' = extendParams env constMap paramlist
      env'' = extendEnvVars env' constMap varDecls
      env''' = extendEnv env'' constMap (Fun x) (TB (TypeProcedure (extractTypes constMap paramlist))) -- Takes care of recursion
        in
    if checkCmpStm env''' constMap stms then extendEnv env constMap (Fun x) (TB (TypeProcedure (extractTypes constMap paramlist)))
    else error "error in checkFunction"


extendEnv :: Ord k => Map k Type -> p -> k -> Type -> Map k Type
extendEnv env constMap x (TB (TypeFunction argsTypes t)) = Map.insert x (TB (TypeFunction argsTypes t)) env
extendEnv env constMap x (TB (TypeProcedure argsTypes)) = Map.insert x (TB (TypeProcedure argsTypes)) env


-- Checks a list of statements
checkCmpStm :: Foldable t => TypeEnv -> p -> t Statement -> Bool
checkCmpStm env constMap = foldr ((&&) . checkStatement env) True


{- Expressions -}
checkExpr :: TypeEnv -> Expr -> Type
checkExpr env (Num n) = TB TypeInteger
checkExpr env BooleanFalse = TB TypeBoolean
checkExpr env BooleanTrue = TB TypeBoolean
checkExpr env (StringLiteral s) = TB TypeString


checkExpr env (Access (Variable x)) = case Map.lookup (Var x) env of
    Nothing -> error ("Variable " ++ show x ++ " is not declared in this scope\n")
    Just t -> t


checkExpr env (Access (Array arrName index))
  = let typeIndex = checkExpr env index
      in if typeIndex == TB TypeInteger then
          case Map.lookup (Var arrName) env of
            Just (TB arrType) -> TB arrType
            Just (TA (TypeArray _ _ basicType)) -> TB basicType
            _ -> error ""
      else error ("\n\nCannot access the array " ++ show arrName ++ " on position " ++ show index ++ "\n\n")


{- Binary operators -}
checkExpr env (BinOp op e1 e2)
  | op == Sum || op == Subtract || op == Multiply || op == Divide || op == Mod
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1== TB TypeInteger && t2== TB TypeInteger then TB TypeInteger
         else error ("\n\ntype error in operation " ++ show op ++ "\n\n")


{- Relational operators -}
checkExpr env (BinOp op e1 e2)
  | op == Equals || op == NotEquals || op == LessThan || op == LessEqualThan
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1== TB TypeInteger && t2== TB TypeInteger then TB TypeBoolean
         else error ("\n\ntype error in operation " ++ show op ++ "\n\n")
 

checkExpr env (BinOp op e1 e2)
 | op == And || op == Or
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1== TB TypeBoolean && t2== TB TypeBoolean then TB TypeBoolean
         else error ("\n\ntype error in operation " ++ show op ++ "\n\n")


checkExpr env (UnaryOp Minus e1)
   = let t1 = checkExpr env e1
      in if t1== TB TypeInteger then TB TypeInteger
         else error "type error in operation NegNum"

checkExpr env (UnaryOp Not e1)
   = let t1 = checkExpr env e1
      in if t1== TB TypeBoolean then TB TypeBoolean
         else error "type error in operation Not"



{- Call Identifier ExpressionList -}
checkExpr env (Call funName funArgs)
  = let ts = map (checkExpr env) funArgs
    in case Map.lookup (Fun funName) env of
         Just (TB (TypeFunction ts' t)) ->
           if ts == ts' then TB t
           else error ("\n\nFunction " ++ show funName ++ " is expecting arguments of " ++ show ts ++ " but got " ++ show ts' ++ "\n\n")
         _ -> error ("\n\ninvalid function name " ++ show funName ++ " is not defined in \n" ++ show env ++ " VER: " ++ show ts ++ "\n")



{- Assign statements -}
checkStatement :: TypeEnv -> Statement -> Bool
checkStatement env (AssignStm (Variable var) expr)
  = case Map.lookup (Var var) env of
      Nothing -> error ("Variable " ++ show var ++ " is not declared in this scope")
      Just (TB (TypeFunction [things] funType)) -> let t2 = checkExpr env expr -- This is for the "return" in functions
        in ((TB funType == t2) || error ("type error in the assignment of " ++ show var
                                                                     ++ ".\n" ++ show var ++ " was declared as {" ++ show funType ++ "} but {" ++ show t2
                                                                     ++ "} was provided.\n"))

      Just t1 -> let t2 = checkExpr env expr
                 in ((t1 == t2) || error ("type error in the assignment of " ++ show var
                             ++ ".\n" ++ show var ++ " was declared as {" ++ show t1 ++ "} but {" ++ show t2
                             ++ "} was provided.\n"))


checkStatement env (AssignStm (Array arrName (Access arg)) expr)
 = case checkExpr env (Access arg) of
  TB TypeInteger ->
    case Map.lookup (Var arrName) env of
      Nothing -> error ("Variable " ++ show arrName ++ " is not declared in this scope")
      Just (TA (TypeArray _ _ t1)) -> let t2 = checkExpr env expr
                  in ((TB t1 == t2) || error ("type error in the assignment of " ++ show arrName
                              ++ ".\n" ++ show arrName  ++ " was declared as {" ++ show t1 ++ "} but {" ++ show t2
                              ++ "} was provided.\n"))
  _ -> error("\n\n" ++ show arg ++ " must be of Type Integer\n\n")

checkStatement env (AssignStm (Array arrName indiceExpr) expr)
 = case checkExpr env indiceExpr of
  TB TypeInteger ->
    case Map.lookup (Var arrName) env of
      Nothing -> error ("Variable " ++ show arrName ++ " is not declared in this scope")
      Just (TA (TypeArray _ _ t1)) -> let (TB t2) = checkExpr env expr
                  in ((t1 == t2) || error ("type error in the assignment of " ++ show arrName
                              ++ ".\n" ++ show arrName  ++ " was declared as {" ++ show t1 ++ "} but {" ++ show t2
                              ++ "} was provided.\n"))
  _ -> error("\n\n" ++ show indiceExpr ++ " must be of Type Integer\n\n")

{- If Statement -}
checkStatement env (IfStm cond stm1)
  = let t0 = checkExpr env cond
    in if t0 == TB TypeBoolean then
         checkStatement env stm1
       else error ("\n\ntype error: condition " ++ show cond ++ " should be bool in " ++ "\n\t" ++ show (IfStm cond stm1) ++ "\n\n")

{- If Then Else Statement -}
checkStatement env (IfThenElseStm cond stm1 stm2)
  = let t0 = checkExpr env cond
    in if t0 == TB TypeBoolean then
         checkStatement env stm1 &&
         checkStatement env stm2
       else error ("\n\ntype error: condition " ++ show cond ++ " should be bool in " ++ "\n\t" ++ show (IfThenElseStm cond stm1 stm2) ++ "\n\n")


{- While Statement -}
checkStatement env (WhileStm expr stm1)
  = let t0 = checkExpr env expr
    in if t0 == TB TypeBoolean then checkStatement env stm1
       else error "type error: condition should be bool --> WhileStm <--"

{- For Statement -}
checkStatement env (ForStm id expr1 expr2 stm1)
  = let t0 = checkExpr env expr1
        t1 = checkExpr env expr2
    in if t0 == t1 && t0 /= TB TypeString && t0 /= TB TypeBoolean then
        checkStatement env stm1
       else error "unexpected behavior on for statement"


{- Break Statement -}
checkStatement env BreakStm = True


{- Procedure Statement - Represents a function call - ProcedureStm FunctionName/ProcedureName [Exprs] -}
checkStatement env (ProcedureStm id exprs)
   = let ts = map (checkExpr env) exprs
    in case Map.lookup (Fun id) env of
         Just (TB (TypeFunction ts' t)) ->
           (ts == ts') || error ("\n\nError in function call "++ show id ++"\n\nExpects arguments of: " ++ show ts' ++ "\nGets arguments of:    " ++ show ts ++ "\n\n")
                     
         Just (TB (TypeProcedure ts')) ->
           (ts == ts') || error ("\n\nError in procedure call "++ show id ++"\n\nExpects arguments of: " ++ show ts' ++ "\nGets arguments of:    " ++ show ts ++ "\n\n")

         _ -> error ("invalid procedure statement\n\nExpecting " ++ show exprs ++
                     " types: " ++ show ts ++"\n Giving id: " ++ show id ++ "\n")


{- Compound Statement - A block of statements -}
checkStatement env (CompoundStm stmList)
  = aux (map (checkStatement env) stmList)





{- Auxiliary functions -}

{- Auxiliary function to check if all elements in a list are True -}
aux :: [Bool] -> Bool
aux [] = True
aux (x:xs)
  | x = aux xs
  | otherwise = False


{- Auxiliary function to peek the environment - Has no real use in the program -}
lookAtProgEnv :: Program -> TypeEnv
lookAtProgEnv (StartProgram header (ProgBody (DeclareConstants constDecls) (DeclareProcedures procDecls) (DeclareVariables varDecls) stms))
  = let constMap = saveConsts Map.empty constDecls
        env0 = extendEnvConsts Map.empty constMap constDecls
        env1 = extendEnvProcs env0 constMap procDecls
        env2 = extendEnvVars env1 constMap varDecls
    in env2