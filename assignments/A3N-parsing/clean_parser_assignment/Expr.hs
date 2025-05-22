module Expr
  ( Expr
  , T
  , parse
  , fromString
  , value
  , toString
  ) where

import qualified Dictionary
import           Parser     hiding (T, fromString)
import           Prelude    hiding (fail, return)

-- | Abstract syntax for expressions, with exponentiation
data Expr
  = Num Integer
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show)

type T = Expr

-- Parsers for the usual arithmetic operators, with correct precedences
var, num, factor, power, term, expr :: Parser Expr
term', expr' :: Expr -> Parser Expr
-- variable and numeric literal
var = word >-> Var

num = number >-> Num

-- constructors for multiplication/division and addition/subtraction
mulOp = lit '*' >-> const Mul ! lit '/' >-> const Div

addOp = lit '+' >-> const Add ! lit '-' >-> const Sub

powOp = lit '^' >-> const Pow

-- build an operation node
bldOp e (oper, e') = oper e e'

-- highest precedence: factors in parentheses, numbers, vars
factor = num ! var ! (lit '(' -# expr #- lit ')') ! err "illegal factor"

-- exponentiation: right-associative, higher precedence than * /
power' e = powOp # factor >-> bldOp e #> power' ! return e

power = factor #> power'

-- multiplication/division
term' e = mulOp # power >-> bldOp e #> term' ! return e

term = power #> term'

-- addition/subtraction
expr' e = addOp # term >-> bldOp e #> expr' ! return e

expr = term #> expr'

-- | Show with minimal parentheses
parens :: Bool -> String -> String
parens cond str =
  if cond
    then "(" ++ str ++ ")"
    else str

shw :: Int -> Expr -> String
shw prec (Num n)   = show n
shw prec (Var v)   = v
shw prec (Add a b) = parens (prec > 5) $ shw 5 a ++ "+" ++ shw 5 b
shw prec (Sub a b) = parens (prec > 5) $ shw 5 a ++ "-" ++ shw 6 b
shw prec (Mul a b) = parens (prec > 6) $ shw 6 a ++ "*" ++ shw 6 b
shw prec (Div a b) = parens (prec > 6) $ shw 6 a ++ "/" ++ shw 7 b
shw prec (Pow a b) = parens (prec > 8) $ shw 8 a ++ "^" ++ shw 9 b

-- | Evaluate, with errors on undefined vars or bad ops
value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) _ = n
value (Var x) dict =
  case Dictionary.lookup x dict of
    Just v  -> v
    Nothing -> error ("variable " ++ x ++ " not in dictionary")
value (Add a b) d = value a d + value b d
value (Sub a b) d = value a d - value b d
value (Mul a b) d = value a d * value b d
value (Div a b) d =
  let vb = value b d
   in if vb == 0
        then error "division by zero"
        else value a d `div` vb
value (Pow a b) d =
  let eb = value b d
   in if eb < 0
        then error "negative exponent"
        else value a d ^ eb

-- Assuming Parse is a type class that needs to be implemented
instance Parse Expr where
  parse = expr
  toString = shw 0

-- Adding fromString function since it's exported
fromString :: String -> Expr
fromString s =
  case parse s of
    Just (expr, "")   -> expr
    Just (expr, rest) -> error ("Unexpected input: " ++ rest)
    Nothing           -> error "Parse error"
