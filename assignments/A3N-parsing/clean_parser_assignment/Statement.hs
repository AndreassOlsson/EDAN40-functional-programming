-- Andreas Olsson (an5175ol-s) and Nikolai Vesic Ellman (ni8034ve-s)
module Statement
  ( T
  , parse
  , toString
  , fromString
  , exec
  ) where

import qualified Dictionary
import qualified Expr
import           Parser     hiding (T, fromString, parse, toString)
import           Prelude    hiding (fail, return)

-- | Abstract syntax for statements
-- seven kinds: assignment, skip, begin, if, while, read, write
data Statement
  = Assign String Expr.T
  | Skip
  | Begin [Statement]
  | If Expr.T Statement Statement
  | While Expr.T Statement
  | Read String
  | Write Expr.T
  deriving (Show)

type T = Statement

-- Parsers for each statement form
assignment :: Parser Statement
-- uncurry :: (a -> b -> c) -> (a, b) -> c can replace original "(\(v, e) -> Assign v e)""
-- converts a curried function (two arguments one at a time) into a function that takes a pair.
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> uncurry Assign

skipStmt :: Parser Statement
skipStmt = accept "skip" -# require ";" >-> const Skip

beginStmt :: Parser Statement
beginStmt = accept "begin" -# stmts #- require "end" >-> Begin
  where
    stmts = iter statement

ifStmt :: Parser Statement
ifStmt =
  accept "if"
    -# Expr.parse
    #- require "then"
    # statement
    #- require "else"
    # statement
    >-> (\((cond, t), e) -> If cond t e)

whileStmt :: Parser Statement
whileStmt =
  accept "while" -# Expr.parse #- require "do" # statement >-> uncurry While

readStmt :: Parser Statement
readStmt = accept "read" -# word #- require ";" >-> Read

writeStmt :: Parser Statement
writeStmt = accept "write" -# Expr.parse #- require ";" >-> Write

-- Forward declaration helper
statement :: Parser Statement
statement =
  assignment ! skipStmt ! ifStmt ! whileStmt ! readStmt ! writeStmt ! beginStmt

-- Top-level parse: try each in order
parse :: Parser Statement
parse = statement

-- | Convert a single statement to formatted string with indentation
toString :: T -> String
toString = toStr 0
  where
    indent n = replicate (2 * n) ' '
    toStr i (Assign v e) = indent i ++ v ++ " := " ++ Expr.toString e ++ ";\n"
    toStr i Skip = indent i ++ "skip;\n"
    toStr i (Read v) = indent i ++ "read " ++ v ++ ";\n"
    toStr i (Write e) = indent i ++ "write " ++ Expr.toString e ++ ";\n"
    toStr i (If c t e) =
      indent i
        ++ "if "
        ++ Expr.toString c
        ++ " then\n"
        ++ toStr (i + 1) t
        ++ indent i
        ++ "else\n"
        ++ toStr (i + 1) e
    toStr i (While c b) =
      indent i ++ "while " ++ Expr.toString c ++ " do\n" ++ toStr (i + 1) b
    toStr i (Begin ss) =
      indent i
        ++ "begin\n"
        ++ concatMap (toStr (i + 1)) ss
        ++ indent i
        ++ "end\n"

-- | Parse from String
fromString :: String -> Statement
fromString s =
  case parse s of
    Just (stmt, "")   -> stmt
    Just (stmt, rest) -> error ("Unexpected input: " ++ rest)
    Nothing           -> error "Parse error"

-- | Execute a single statement.
execOne ::
     T
  -> Dictionary.T String Integer
  -> [Integer]
  -> (Dictionary.T String Integer, [Integer], [Integer])
execOne stmt dict input =
  case stmt of
    Assign v e -> (Dictionary.insert (v, Expr.value e dict) dict, input, []) -- Assigns a value to a variable.
    Skip -> (dict, input, []) -- Does nothing.
    Read v ->
      case input of
        (x:xs) -> (Dictionary.insert (v, x) dict, xs, []) -- Reads a value from input.
        []     -> error "read: no more input"
    Write e -> (dict, input, [Expr.value e dict]) -- Writes an expression's value.
    If c t e ->
      let cond = Expr.value c dict
       in if cond > 0
            then execOne t dict input -- Executes the "then" branch.
            else execOne e dict input -- Executes the "else" branch.
    While c b ->
      let cond = Expr.value c dict
       in if cond > 0
            then let (dict', input', output') = execOne b dict input
                     (dict'', input'', output'') = execOne stmt dict' input'
                  in (dict'', input'', output' ++ output'') -- Loops while condition is true.
            else (dict, input, [])
    Begin ss -> execList ss dict input -- Executes a block of statements.

-- | Execute a list of statements.
execList ::
     [T]
  -> Dictionary.T String Integer
  -> [Integer]
  -> (Dictionary.T String Integer, [Integer], [Integer])
execList [] dict input = (dict, input, []) -- Base case: no statements.
execList (s:ss) dict input =
  let (dict', input', output') = execOne s dict input
      (dict'', input'', output'') = execList ss dict' input'
   in (dict'', input'', output' ++ output'') -- Combines outputs from all statements.

-- | Execute a list of statements, returning only the output
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec stmts dict input =
  let (_, _, output) = execList stmts dict input
   in output
