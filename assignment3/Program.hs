module Program (K, parse, fromString, toString, exec) where

import qualified Dictionary
import Parser hiding (T)
import qualified Statement
import Prelude hiding (fail, return)

newtype K = Program [Statement.T]

instance Parse K where
  parse = iter Statement.parse >-> Program
  toString (Program p) = concatMap Statement.toString p

exec (Program k) = Statement.exec k