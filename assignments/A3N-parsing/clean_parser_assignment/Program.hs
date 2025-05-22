-- Andreas Olsson (an5175ol-s) and Nikolai Vesic Ellman (ni8034ve-s)
module Program
  ( T
  , parse
  , fromString
  , toString
  , exec
  ) where

import qualified Dictionary
import           Parser     hiding (T, fromString, parse, toString)
import           Prelude    hiding (fail, return)
import qualified Statement

-- A Program is just a wrapper for a list of statements
newtype T =
  Program [Statement.T]
  deriving (Show)

-- Parse a list of statements as a program
parse :: Parser T
parse = iter Statement.parse >-> Program

-- Convert source string to a parsed program
fromString :: String -> T
fromString s =
  case parse s of
    Just (prog, "")   -> prog
    Just (prog, rest) -> error ("Unexpected input: " ++ rest)
    Nothing           -> error "Parse error"

-- Convert a program back into source code
toString :: T -> String
toString (Program stmts) = concatMap Statement.toString stmts

-- Execute the program with input values
exec :: T -> [Integer] -> [Integer]
exec (Program stmts) = Statement.exec stmts Dictionary.empty
