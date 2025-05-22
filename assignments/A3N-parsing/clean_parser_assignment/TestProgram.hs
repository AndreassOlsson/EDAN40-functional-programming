{- Test for Program -}
module TestProgram where

import Program

p0, p1, p2, p3, p4, p5 :: Program.T

-- basic program, no comments

p0 = fromString  ("\
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

-- another simple program. What does it do?

p1 = fromString  ("\
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

-- this time just string correspnding to p1

s1 = "\
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

rp0 = Program.exec p0 [3,16]

rp1 = Program.exec p1 [1024, 2]

-- this time some comments and exponents

s4 = "\
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

rp4 = Program.exec p4 [4,4]

-- more tricky one

p5 = fromString ("\
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

sp6 =  putStr (toString p6)