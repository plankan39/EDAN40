module Statement (T, parse, toString, fromString, exec) where

import qualified Dictionary
import qualified Expr
import Parser hiding (T)
import Prelude hiding (fail, return)

type T = Statement

data Statement
  = Assignment String Expr.T
  | Skip
  | Block [Statement]
  | If Expr.T Statement Statement
  | While Expr.T Statement
  | Read String
  | Write Expr.T
  deriving (Show)

-- The >-> build* is for creating a Statement out
-- of the parsing

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip

buildSkip _ = Skip

-- Accept "begin", read statements until no longer possible,
-- and require that the last word is "end", then make this a block
block = accept "begin" -# iter parse #- require "end" >-> buildBlock

buildBlock = Block

ifelse = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIfelse

buildIfelse ((e, s1), s2) = If e s1 s2

whiledo = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhiledo

buildWhiledo (e, s) = While e s

read = accept "read" -# word #- require ";" >-> buildRead

buildRead = Read

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite

buildWrite = Write

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts : stmts) dict input =
  if Expr.value cond dict > 0
    then exec (thenStmts : stmts) dict input
    else exec (elseStmts : stmts) dict input
exec ((Assignment var expr) : stmts) dict input =
  exec stmts (Dictionary.insert (var, Expr.value expr dict) dict) input
exec (Skip : stmts) dict input = exec stmts dict input
exec ((Block innerStmts) : stmts) dict input = exec (innerStmts ++ stmts) dict input
exec ((While cond stmt) : stmts) dict input =
  if Expr.value cond dict > 0
    then exec (stmt : (While cond stmt : stmts)) dict input
    else exec stmts dict input
exec ((Read str) : stmts) _ [] = error ("Can not Read, input is empty")
exec ((Read str) : stmts) dict (i : input) =
  exec stmts (Dictionary.insert (str, i) dict) input
exec ((Write expr) : stmts) dict input =
  Expr.value expr dict : exec stmts dict input

instance Parse Statement where
  parse = assignment ! skip ! block ! ifelse ! whiledo ! Statement.read ! write
  toString = error "Statement.toString not implemented"
