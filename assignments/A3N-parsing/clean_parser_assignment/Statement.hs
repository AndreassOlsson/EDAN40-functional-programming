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
assignment =
  word #- accept ":=" # Expr.parse #- require ";" >-> (\(v, e) -> Assign v e)

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
  accept "while"
    -# Expr.parse
    #- require "do"
    # statement
    >-> (\(cond, body) -> While cond body)

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
toString stmt = toStr 0 stmt
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

-- | Execute a statement with dictionary and input, returning new input
execOne ::
     T
  -> Dictionary.T String Integer
  -> [Integer]
  -> (Dictionary.T String Integer, [Integer], [Integer])
execOne stmt dict input =
  case stmt of
    Assign v e -> (Dictionary.insert (v, Expr.value e dict) dict, input, [])
    Skip -> (dict, input, [])
    Read v ->
      case input of
        (x:xs) -> (Dictionary.insert (v, x) dict, xs, [])
        []     -> error "read: no more input"
    Write e -> (dict, input, [Expr.value e dict])
    If c t e ->
      let cond = Expr.value c dict
       in if cond > 0
            then execOne t dict input
            else execOne e dict input
    While c b ->
      let cond = Expr.value c dict
       in if cond > 0
            then let (dict', input', output') = execOne b dict input
                     (dict'', input'', output'') = execOne stmt dict' input'
                  in (dict'', input'', output' ++ output'')
            else (dict, input, [])
    Begin ss -> execList ss dict input

-- | Execute a list of statements
execList ::
     [T]
  -> Dictionary.T String Integer
  -> [Integer]
  -> (Dictionary.T String Integer, [Integer], [Integer])
execList [] dict input = (dict, input, [])
execList (s:ss) dict input =
  let (dict', input', output') = execOne s dict input
      (dict'', input'', output'') = execList ss dict' input'
   in (dict'', input'', output' ++ output'')

-- | Execute a list of statements, returning only the output
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec stmts dict input =
  let (_, _, output) = execList stmts dict input
   in output
