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
  | Comment String
  deriving (Show)

-- The >-> build* is for creating a Statement out
-- of the parsing

assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

buildAss :: (String, Expr.T) -> Statement
buildAss (v, e) = Assignment v e

skip :: Parser Statement
skip = accept "skip" #- require ";" >-> buildSkip

buildSkip :: p -> Statement
buildSkip _ = Skip

-- Accept "begin", read statements until no longer possible,
-- and require that the last word is "end", then make this a block
block :: Parser Statement
block = accept "begin" -# iter parse #- require "end" >-> buildBlock

buildBlock :: [Statement] -> Statement
buildBlock = Block

ifelse :: Parser Statement
ifelse = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIfelse

buildIfelse :: ((Expr.T, Statement), Statement) -> Statement
buildIfelse ((e, s1), s2) = If e s1 s2

whiledo :: Parser Statement
whiledo = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhiledo

buildWhiledo :: (Expr.T, Statement) -> Statement
buildWhiledo (e, s) = While e s

read :: Parser Statement
read = accept "read" -# word #- require ";" >-> buildRead

buildRead :: String -> Statement
buildRead = Read

write :: Parser Statement
write = accept "write" -# Expr.parse #- require ";" >-> buildWrite

buildWrite :: Expr.T -> Statement
buildWrite = Write

comment :: Parser Statement
comment = Parser.comment >-> Comment

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
exec ((Read str) : stmts) _ [] = error "Can not Read, input is empty"
exec ((Read str) : stmts) dict (i : input) =
  exec stmts (Dictionary.insert (str, i) dict) input
exec ((Write expr) : stmts) dict input =
  Expr.value expr dict : exec stmts dict input
exec ((Comment c) : stmts) dict input = exec stmts dict input

instance Parse Statement where
  parse = assignment ! skip ! block ! ifelse ! whiledo ! Statement.read ! write
  toString (If cond thenStmts elseStmts) =
    "if "
      ++ toString cond
      ++ " then\n\t"
      ++ toString thenStmts
      ++ "else\n\t"
      ++ toString elseStmts
  toString (Assignment var val) =
    var ++ " := " ++ toString val ++ ";\n"
  toString (Skip) = "skip;\n"
  toString (Block stmts) = "begin\n\t" ++ foldl tabAppend "" stmts ++  "end\n"
    where tabAppend acc stmt = acc ++ "\t" ++ toString stmt
  toString (While cond stmt) = "while " ++ toString cond ++ "do\n\t" ++ toString stmt
  toString (Read str) = "read " ++ str ++ ";\n"
  toString (Write expr) = "write " ++ toString expr ++ ";\n"
  toString (Comment c) = "--" ++ c ++ "\n"