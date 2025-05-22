{- Test for Program -}
-- Seems to be working now!
module TestProgram where

import           Program

p0, p1, p2, p3, p4, p5 :: Program.T
-- p0: write m if m is a multiple of k
p0 =
  fromString
    ("\
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
    \  end")

-- p1 does some thing with bases and numbers
-- print the base b digits of n in reverse order (I think)
p1 =
  fromString
    ("\
    \read n;\
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
    \write s;")

s1 =
  "\
 \read n;\
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

-- testing toString for programs
sp = putStr (toString p0)

sp1 = putStr (toString p1)

-- does it give us correct programs back?
p2 = fromString (toString p0)

p3 = fromString (toString p1)

-- does execution work as expected?
-- p4 does some other unneccessary things
s4 =
  "\
 \read a;\
 \read b;\
 \-- a comment\n\
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

-- more tricky one
-- p5 calculates the factorial of n
p5 =
  fromString
    ("\
    \begin\
    \  read n;   -- just reading n...\n\
    \  -- this line should just be ignored\n\
    \  fac := -- initialize fac\n\
    \         -- to 1:\n\
    \         1;\
    \  while n do\
    \    begin\
    \      fac := fac*n;\
    \      n := n-1;\
    \    end\
    \  write -- woops\n\
    \  fac;\
    \end")

sp5 = putStr (toString p5)

p6 = fromString (toString p5)

sp6 = putStr (toString p6)

-- All tests:
-- Expected [3, 6, 9, 12, 15]
rp0 = Program.exec p0 [3, 16]

-- Expecteed [0,0,0,0,0,0,0,0,0,0,1,10000000000]
-- (10 zeros, then a 1, then the value of s)
-- (which is 1024 in base 2)
-- this time some comments and exponents
rp1 = Program.exec p1 [1024, 2]

-- Expected [64, 16, 27, 8, 8, 4, 1, 2, 0]
rp4 = Program.exec p4 [4, 4]

-- Expected [24]
rp6 = Program.exec p6 [4]
