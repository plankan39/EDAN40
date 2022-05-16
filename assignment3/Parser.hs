module Parser
  ( module CoreParser,
    T,
    digit,
    digitVal,
    chars,
    letter,
    err,
    lit,
    number,
    iter,
    accept,
    require,
    token,
    spaces,
    word,
    (-#),
    (#-),
  )
where

import CoreParser
import Data.Char
import Prelude hiding (fail, return)


infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message ++ " near " ++ cs ++ "\n")

-- |Iterates on a parser until no longer possible
iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons (a, b) = a : b

-- |Applies two parsers in sequence, but throws away
-- the result of the **first** one
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

-- |Applies two parsers in sequence, but throws away
-- the result of the **second** one
(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

spaces :: Parser String
spaces = iter $ char ? isSpace

-- |Removes trailing spaces after a string
token :: Parser a -> Parser a
token m = m #- spaces -- Defined as `m #- iter space` in PDF

letter :: Parser Char
letter = char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n - 1) >-> cons

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (== w)

require :: String -> Parser String
require w = error "require not implemented"

lit :: Char -> Parser Char
lit c = token char ? (== c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n =
  digitVal #> (\d -> number' (10 * n + d))
    ! return n

number :: Parser Integer
number = token (digitVal #> number')
