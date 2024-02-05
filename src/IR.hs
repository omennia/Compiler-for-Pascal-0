{-
  3-Addresss Intermediate code
  Pedro Vasconcelos, 2022
-}
module IR (Instr (..), BinaryOperation (..), Temp, Label) where

import AST (BinaryOperation (..), UnaryOperation (..))

type Temp = String

type Label = String

data Instr
  = MOVE Temp Temp -- [DONE]
  | MOVEI Temp Int -- [DONE]
  | BinaryOp BinaryOperation Temp Temp Temp -- [DONE]
  | BinaryOpI BinaryOperation Temp Temp Int -- [DONE]
  | UnOp UnaryOperation Temp Temp -- [DONE]
  | UnOpI UnaryOperation Temp Int {- Falta fazer   nao esquecer -}
  | LABEL Label -- [DONE]
  | JUMP Label -- [DONE]
  | COND Temp BinaryOperation Temp Label Label
  | CALL Temp Label [Temp] [Temp] -- t:=CALL f(temps); save temps
  | FUNCTION Label [Temp]
  | RETURN Temp
  | BREAK
  | MACCESS Temp Temp -- temp1 := MEM[temp2]
  | MPUT  Temp Temp -- MEM[temp1] := temp2  
  | ALLOC Temp Int -- ALLOC array_base_temp array_size_bytes 
  | MAKESTR Label String
  | MOVESTR Temp Label
  | DATA
  | TEXT
  | EXIT
  deriving (Eq, Show)