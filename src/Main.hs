module Main where

import AST
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Mapd
import qualified Data.Sequence as Map
import IntermCodeGen (TableEntry (Temp), transProgram)
import Lexer (alexScanTokens)
import MachineCodeGen
import Parser (parse)
import Typecheck (checkProg)

main :: IO ()
main = do
  txt <- getContents
  let ast = parse $ alexScanTokens txt
  -- print ast

  -- putStr "\n"
  -- putStr "\n"
  -- putStr "\n"
  -- putStr "\n"

  -- Typechecker
  if checkProg ast
    then putStrLn "typecheck ok"
    else error "typecheck not ok"

  putStr "\n"
  putStr "\n"
  putStr "\n"
  putStr "\n"

  let intermediaryCode = evalState (IntermCodeGen.transProgram Mapd.empty ast) (0, 0, [])

  mapM_ print $ intermediaryCode -- Prints intermediary code

  putStr "\n"
  putStr "\n"
  putStr "\n"
  putStr "\n"

  mapM_ putStrLn $ MachineCodeGen.transProgram intermediaryCode -- Get the machine code