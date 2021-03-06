{- Test for Program -}
module TestProgram where

import qualified Dictionary
import Program

p0, p1, p2, p3, p4, p5 :: Program.K
p0 =
  fromString
    "\
    \read k;\
    \read n;\
    \m := 1;\
    \while n-m do\
    \  begin\
    \    if m - m/k*k then\
    \      skip;\
    \    else\
    \      write m;\
    \    m := m + 1;\
    \  end"
p1 =
  fromString
    "\
    \read n;\
    \ -- hej\n\
    \read b;\
    \m := 1;\
    \s := 0;\
    \p := 1;\
    \while n do\
    \  begin\
    \    q := n/b;\
    \    r := n - q*b;\
    \    write r;\
    \    s := p*r+s;\
    \    p := p*10;\
    \    n :=q;\
    \  end\
    \write s;"

s1 =
  "\
  \read n;\
  \--hejhej \n\
  \read b;\
  \m := 1;\
  \s := 0;\
  \p := 1;\
  \while n do\
  \  begin\
  \    q := n/b;\
  \    r := n - q*b;\
  \    write r;\
  \    s := p*r+s;\
  \    p := p*10;\
  \    n :=q;\
  \  end\
  \write s;"

sp = putStr (toString p0)

sp1 = putStr (toString p1)

p2 = fromString (toString p0)

p3 = fromString (toString p1)

rp0 = Program.exec p0 Dictionary.empty [3, 16]

rp1 = Program.exec p1 Dictionary.empty [1024, 2]

s4 =
  "\
  \read a;\
  \read b;\
  \s := 3;\
  \while a do\
  \  begin\
  \    c := a^s;\
  \    d := 2^a;\
  \    write c;\
  \    write d;\
  \    a := a-1;\
  \  end\
  \write a;"

p4 = fromString s4

rp4 = Program.exec p4 Dictionary.empty [4, 4]

s5 =
  "\
  \a := 2;\
  \b := 3;\
  \c := a^b^b;\
  \write c;"

p5 = fromString s5

rp5 = Program.exec p5 Dictionary.empty []
