{- Test for Expr-}
module TestExpr where

import qualified Dictionary
import Expr

dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) $
       Dictionary.empty 

testValue string = value (fromString string) dict

n1 = testValue "1"
n2 = testValue "x"
n3 = testValue "x+y"
n4 = testValue "x-y-y"
n21 = testValue "1/(2-y)" {-  Expr.value: division by 0 -}
n31 = testValue "2+z"     {-  Expr.value: undefined variable z -}

-- testing exponentiation

p1 = testValue "y^3"
p2 = testValue "y^3^4"
p3 = testValue "8^4"
p4 = testValue "(y+3)*2^(x+y)"
p5 = testValue "2^3^4+2^5*6+7^8+9"

-- p1 = 8
-- p2 should be much larger than p3!
-- p2 = 2^81 = 2417851639229258349412352
-- p3 = 8^4 = 2^12 = 4096
-- p4 = 40
-- p5 = 2417851639229258355177354

