-- EDAN40 A3 Parser by Emil Eriksson (em5184er-s) && Lukas Elmlund (lu0804el-s)
module Program (K, parse, fromString, toString, exec) where

import qualified Dictionary
import Parser hiding (T)
import qualified Statement
import Prelude hiding (fail, return)

newtype K = Program [Statement.T]

instance Parse K where
  parse = iter Statement.parse >-> Program
  toString (Program p) = concatMap Statement.toString p

exec :: K -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Program k) = Statement.exec k