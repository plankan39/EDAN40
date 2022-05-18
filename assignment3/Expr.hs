{-
   An expression of type Expr is a representation of an arithmetic expression
   with integer constants and variables. A variable is a string of upper-
   and lower case letters. The following functions are exported

   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int

   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.

   fromString expects its argument to contain an expression and returns the
   corresponding Expr.

   toString converts an expression to a string without unneccessary
   parentheses and such that fromString (toString e) = e.

   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.
-}
module Expr (Expr, T, parse, fromString, value, toString) where

import qualified Dictionary
import Parser hiding (T)
import Prelude hiding (fail, return, exponent)

data Expr
  = Num Integer
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  deriving (Show)

type T = Expr

var, num, factor, term, expr :: Parser Expr
term', expr' :: Expr -> Parser Expr
var = word >-> Var
num = number >-> Num

mulOp =
  lit '*' >-> (\_ -> Mul)
    ! lit '/' >-> (\_ -> Div)

addOp =
  lit '+' >-> (\_ -> Add)
    ! lit '-' >-> (\_ -> Sub)

expOp = lit '^' >-> (\_ -> Exp)

bldOp e (oper, e') = oper e e'

factor =
  num
    ! var
    ! lit '(' -# expr #- lit ')'
    ! err "illegal factor"

exponent' e = expOp # factor >-> bldOp e

exponent = factor #> exponent'

term' e = mulOp # exponent >-> bldOp e #> term' ! return e

term = exponent #> term'

expr' e = addOp # term >-> bldOp e #> expr' ! return e

expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec > 5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec > 5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec > 6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec > 6) (shw 6 t ++ "/" ++ shw 7 u)
shw prec (Exp t u) = parens (prec > 7) (shw 7 t ++ "^" ++ shw 8 u)

-- |
-- The expression value e dictionary should return the value of e
-- if all the variables occur in dictionary and there is no division by zero.
--
-- Otherwise an error should be reported using error.
--
-- The dictionary contains variable bindings, for example
-- a := 1;
-- b := 2;
-- etc.
value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) _ = n
value (Var a) vars = case Dictionary.lookup a vars of
  Just val -> val
  Nothing -> error $ "Variable " ++ a ++ " does not exist"
value (Add a b) vars = value a vars + value b vars
value (Sub a b) vars = value a vars - value b vars
value (Mul a b) vars = value a vars * value b vars
value (Div a b) vars =
  if y == 0
    then error "Division by zero!"
    else x `div` y
  where
    x = value a vars
    y = value b vars
value (Exp a b) vars = value a vars ^ value b vars

instance Parse Expr where
  parse = expr
  toString = shw 0
