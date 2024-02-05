module AST where

type Identifier = String

{- Programs -}

data Program = StartProgram ProgramHeader ProgramBody
             deriving (Show, Eq)

data ProgramHeader = ProgHead Identifier 
                   deriving (Show, Eq)

data ProgramBody = ProgBody DeclarationsForConsts DeclarationsForProcedures DeclarationsForVars [Statement]
                  deriving (Show, Eq)




{- Procedures -}

data DeclarationsForProcedures = Procedure ProcHead ProcBody
                               | DeclareProcedures [DeclarationsForProcedures]
                               deriving (Show, Eq)

data ProcHead = ProcedureName Identifier [Param]
              | FunctionName Identifier [Param] BasicType
              deriving (Show, Eq)

data ProcBody = ProcedureBody DeclarationsForVars [Statement]
              deriving (Show, Eq)

data Param = Parameter Identifier Type
           deriving (Show, Eq)






{- Statements -}

data Statement = AssignStm VarAccess Expr --
               | IfStm Expr Statement --
               | IfThenElseStm Expr Statement Statement --
               | WhileStm Expr Statement --
               | ForStm Identifier Expr Expr Statement --
               | BreakStm 
               | ProcedureStm Identifier [Expr] --
               | CompoundStm [Statement] --
               deriving (Show, Eq)





{- Declarations -}

data DeclarationsForVars = Declare Identifier Type
                         | DeclareVariables [DeclarationsForVars]
                         deriving (Show, Eq)


{- {- TODO: Experimental!!!! -}
data FunDef = FunDef Identifier [(Identifier, BasicType)] BasicType Statement

data ProcDef = ProcDef Identifier [(Identifier, BasicType)] Statement
 -}

data Type = TB BasicType 
          | TA ArrayType
          deriving (Show, Eq)

data BasicType = TypeInteger | TypeBoolean | TypeString
                | TypeFunction [Type] BasicType
                | TypeProcedure [Type]
               deriving (Show, Eq)

data ArrayType = TypeArray Constant Constant BasicType
               deriving (Show, Eq)

data DeclarationsForConsts = DefineConst Identifier Int
                           | DeclareConstants [DeclarationsForConsts]
                           | NOCONSTS
                           deriving (Show, Eq, Ord)

data Constant = Int Int
              | Const Identifier
              deriving (Show, Eq, Ord)



{- TODO experimental! -}
--- variable declarations
type Decl = (Identifier, BasicType)           -- variable, type



{- Expressions -}

data Expr = Num Int --
          | StringLiteral String -- ?
          | BooleanTrue --
          | BooleanFalse --
          | Access VarAccess --
          | BinOp BinaryOperation Expr Expr --
          | UnaryOp UnaryOperation Expr -- 
          | Call Identifier [Expr] --
          deriving (Show, Eq)

data UnaryOperation = Minus | Not
                    deriving (Show, Eq)

{- 
  We will define that the following opperations are equivalent:
  x >= y       y <= x
  x > y        y < x
-}
data BinaryOperation = Sum | Subtract | Multiply | Divide | Mod | Equals |
                       NotEquals | LessThan | LessEqualThan | And | Or 
                       deriving (Show, Eq)
                       
data VarAccess = Variable Identifier
               | Array Identifier Expr
               deriving (Show, Eq)