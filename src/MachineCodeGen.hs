{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module MachineCodeGen where

import AST
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Type.Coercion (trans)
import IR

transProgram :: [Instr] -> [String]
transProgram [] = []
transProgram instructions = transInstructions instructions ++ writeInt ++ readInt ++ writeStr

transInstructions :: [Instr] -> [String]
transInstructions [] = []
transInstructions (COND rs Equals rt ltrue lfalse : LABEL label : xs)
  | label == lfalse = ["beq " ++ getReg rs ++ ", " ++ getReg rt ++ ", " ++ ltrue] ++ [lfalse ++ ":"] ++ transInstructions xs
  | label == ltrue = ["bne " ++ getReg rs ++ ", " ++ getReg rt ++ ", " ++ lfalse] ++ [ltrue ++ ":"] ++ transInstructions xs
transInstructions (COND rs Equals rt ltrue lfalse : xs) = ["beq " ++ getReg rs ++ ", " ++ getReg rt ++ ", " ++ ltrue] ++ ["j " ++ lfalse] ++ transInstructions xs

transInstructions (COND rs NotEquals rt ltrue lfalse : LABEL label : xs)
  | label == lfalse = ["bne " ++ getReg rs ++ ", " ++ getReg rt ++ ", " ++ ltrue] ++ [lfalse ++ ":"] ++ transInstructions xs
  | label == ltrue = ["beq " ++ getReg rs ++ ", " ++ getReg rt ++ ", " ++ lfalse] ++ [ltrue ++ ":"] ++ transInstructions xs
transInstructions (COND rs NotEquals rt ltrue lfalse : xs) = ["bne " ++ getReg rs ++ ", " ++ getReg rt ++ ", " ++ ltrue] ++ ["j " ++ lfalse] ++ transInstructions xs

transInstructions (COND rs LessThan rt ltrue lfalse : LABEL label : xs)
  | label == lfalse = ["blt " ++ getReg rs ++ ", " ++ getReg rt ++ ", " ++ ltrue] ++ [lfalse ++ ":"] ++ transInstructions xs
  | label == ltrue = ["bge " ++ getReg rs ++ ", " ++ getReg rt ++ ", " ++ lfalse] ++ [ltrue ++ ":"] ++ transInstructions xs
transInstructions (COND rs LessThan rt ltrue lfalse : xs) = ["blt " ++ getReg rs ++ ", " ++ getReg rt ++ ", " ++ ltrue] ++ ["j " ++ lfalse] ++ transInstructions xs

transInstructions (COND rs LessEqualThan rt ltrue lfalse : LABEL label : xs)
  | label == lfalse = ["ble " ++ getReg rs ++ ", " ++ getReg rt ++ ", " ++ ltrue] ++ [lfalse ++ ":"] ++ transInstructions xs
  | label == ltrue = ["bgt " ++ getReg rs ++ ", " ++ getReg rt ++ ", " ++ lfalse] ++ [ltrue ++ ":"] ++ transInstructions xs
transInstructions (COND rs LessEqualThan rt ltrue lfalse : xs) = ["ble " ++ getReg rs ++ ", " ++ getReg rt ++ ", " ++ ltrue] ++ ["j " ++ lfalse] ++ transInstructions xs

transInstructions (instr : xs) =
  case instr of
    MOVE dest start -> ["move " ++ getReg dest ++ ", " ++ getReg start]
    MOVEI dest constant -> ["li " ++ getReg dest ++ " " ++ show constant]
    BinaryOp op rd rs rt -> case op of
      Sum -> [translate3 "add" rd rs rt]
      Subtract -> [translate3 "sub" rd rs rt]
      Multiply -> [translate3 "mul" rd rs rt]
      Divide -> [translate3 "div" rd rs rt]
      Mod -> [translate3 "rem" rd rs rt]
      Equals -> [translate3 "seq" rd rs rt]
      NotEquals -> [translate3 "sne" rd rs rt]
      LessThan -> [translate3 "slt" rd rs rt]
      LessEqualThan -> [translate3 "sle" rd rs rt]
      And -> [translate3 "and" rd rs rt]
      Or -> [translate3 "or" rd rs rt]
    BinaryOpI Multiply rd rs n ->
      if n == 4 then
        [translate2 "sll" rd rs ++ " 2"]
      else
        error "Compiler error. Only BinaryOpI Multiplication by 4 is implemented."
    BinaryOpI Sum rd rs n ->
      [translate2 "addi" rd rs ++ " " ++ show n]
    UnOp op rd rt -> case op of
      Minus -> [translate2 "neg" rd rt]
      Not -> [translate2 "not" rd rt]
    UnOpI op rd rt -> case op of
      Minus -> ["sub " ++ getReg rd ++ ", " ++ (show (rt)) ++ ", " ++ (show (2 * rt))]
    CALL dest label args usedTemps ->
      do
        let loadArgumentsInstr = loadToStack args (-4)
        loadToStack usedTemps (-4) ++ loadArgumentsInstr ++ ["jal " ++ label, "addiu $sp, $sp, " ++ (show (4 * (length loadArgumentsInstr - 1)))] ++  restoreTemps usedTemps 0 ++["move " ++ getReg dest ++ ", $v0"]
    LABEL label -> [label ++ ":"]
    JUMP label -> ["j " ++ label]
    FUNCTION label paramList ->
      do
        [label ++ ": "] ++ ["sw $fp, -4($sp)"] ++ ["sw $ra, -8($sp)"] ++ ["move $fp, $sp"] ++ ["addiu $sp, $sp, -8"] ++ unloadArguments paramList 0
    RETURN t ->
      do
        ["move $v0, $t0"] ++ ["move $sp, $fp"] ++ ["lw $ra, -8($sp)"] ++ ["lw $fp, -4($sp)"] ++ ["jr $ra"]
    ALLOC dest size ->
      do
        let code = [translate2 "move" dest "gp"]
        let code' = ["addi " ++ "$gp" ++ ", $gp, " ++ (show size)]
        code ++ code'
    MPUT address source -> ["sw " ++ getReg source ++ ", 0(" ++ getReg address ++ ")"]
    MACCESS dest address -> ["lw " ++ getReg dest ++ ", 0(" ++ getReg address ++ ")"]
    EXIT -> endProgram
    MOVESTR temp label -> ["la " ++ getReg temp ++ " " ++ label]
    MAKESTR str lbl -> [lbl ++ ": .asciiz " ++ (show str)]
    DATA -> [".data"]
    TEXT -> [".text"]
    anything_else -> error ("\n\ncant do transInstructions on " ++ show anything_else ++ "\n\n")
    ++ transInstructions xs

{- Auxiliary functions -}
getReg temp = "$" ++ temp

translate3 instr rd rs rt = instr ++ " " ++ getReg rd ++ ", " ++ getReg rs ++ ", " ++ getReg rt

translate2 instr rd rt = instr ++ " " ++ getReg rd ++ ", " ++ getReg rt

loadToStack :: [Temp] -> Int -> [String]
loadToStack [] n = ["addiu $sp $sp " ++ show (n+4)]
loadToStack params n = ("sw " ++ getReg (last params) ++ ", " ++ show n ++ "($sp)") : loadToStack (init params) (n-4)

restoreTemps :: [Temp] -> Int -> [String]
restoreTemps [] n = ["addiu $sp, $sp " ++ show n]
restoreTemps params n = ("lw " ++ getReg (head params) ++ ", " ++ show n ++ "($sp)") : restoreTemps (tail params) (n+4)

unloadArguments :: [Temp] -> Int -> [String]
unloadArguments [] _ = []
unloadArguments params n = ("lw " ++ getReg (head params) ++ ", " ++ show n ++ "($fp)") : unloadArguments (tail params) (n+4)

allTemps :: [String]
allTemps = ["t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9"]

endProgram :: [String]
endProgram = ["li $v0, 10", "syscall"]

writeInt :: [String]
writeInt = ["WRITEINT: ", "li $v0, 1", "lw $a0, 0($sp)", "syscall", "jr $ra"]

readInt :: [String]
readInt = ["READINT: ", "li $v0, 5", "syscall", "jr $ra"]

writeStr :: [String]
writeStr = ["WRITESTR: ", "li $v0, 4", "lw $a0, 0($sp)", "syscall", "jr $ra"]