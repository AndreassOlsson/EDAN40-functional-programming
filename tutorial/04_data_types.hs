-- Custom Data Types in Haskell

-- Defining a simple data type (enum)
data Color = Red | Green | Blue | Yellow

-- Function that works with our Color type
showColor :: Color -> String
showColor Red = "Red"
showColor Green = "Green"
showColor Blue = "Blue"
showColor Yellow = "Yellow"

-- Data type with parameters (like a struct or class)
data Person = Person String Int Bool
-- Person's name, age and whether they are a student

-- Constructor with named fields (record syntax)
data Book = Book {
    title :: String,
    author :: String,
    year :: Int,
    pageCount :: Int
}

-- Using the record syntax to create a book
myBook = Book {
    title = "Learn You a Haskell",
    author = "Miran Lipovača",
    year = 2011,
    pageCount = 400
}

-- Parameterized types (generics)
data Box a = Empty | MkBox a

-- Example of using the Box type
intBox :: Box Int
intBox = MkBox 42

stringBox :: Box String
stringBox = MkBox "Hello"

-- Pattern matching with our custom types
unbox :: Box a -> Maybe a
unbox Empty = Nothing
unbox (MkBox x) = Just x

-- Recursive data structures (like a linked list)
data List a = Nil | Cons a (List a)

-- Convert our custom List to a standard list
toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

-- Binary tree data structure
data Tree a = EmptyTree | Node a (Tree a) (Tree a)

-- Creating a simple tree
myTree = Node 5 
            (Node 3 EmptyTree EmptyTree) 
            (Node 7 EmptyTree EmptyTree)

-- Type synonyms - giving new names to existing types
type Name = String
type Age = Int
type Student = (Name, Age)

-- newtype - a more efficient way when wrapping a single type
newtype EmailAddress = Email String

-- Type classes - similar to interfaces in OOP
-- Here's how to make our types part of existing type classes
instance Show Color where
    show = showColor

-- Or define our own type class
class Describable a where
    describe :: a -> String

instance Describable Color where
    describe = showColor

instance Describable Person where
    describe (Person name age isStudent) = 
        name ++ ", " ++ show age ++ 
        (if isStudent then " (student)" else "")

main :: IO ()
main = do
    let alice = Person "Alice" 25 True
        bob = Person "Bob" 30 False
    
    putStrLn "Custom data type examples:"
    print Red  -- Uses our Show instance
    putStrLn (describe Green)  -- Uses our Describable instance
    putStrLn (describe alice)
    putStrLn (title myBook)
    putStrLn (author myBook)
    print (unbox intBox)
    print (unbox (Empty :: Box Int))
