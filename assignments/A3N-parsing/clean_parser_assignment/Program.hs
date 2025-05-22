module Program (T, parse, fromString, toString, exec) where

import Dictionary qualified
import Parser hiding (T, fromString, parse, toString)
import Statement qualified
import Prelude hiding (fail, return)

-- A Program is just a wrapper for a list of statements
newtype T = Program [Statement.T] deriving (Show)

-- Parse a list of statements as a program
parse :: Parser T
parse = iter Statement.parse >-> Program

-- Convert source string to a parsed program
fromString :: String -> T
fromString s = case parse s of
  Just (prog, "") -> prog
  Just (prog, rest) -> error ("Unexpected input: " ++ rest)
  Nothing -> error "Parse error"

-- Convert a program back into source code
toString :: T -> String
toString (Program stmts) = concatMap Statement.toString stmts

-- Execute the program with input values
exec :: T -> [Integer] -> [Integer]
exec (Program stmts) input = Statement.exec stmts Dictionary.empty input