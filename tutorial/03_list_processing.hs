-- List Processing in Haskell

-- List comprehensions (similar to Python's list comprehensions)
-- Generate even numbers from 1 to 10
evens = [x | x <- [1..10], x `mod` 2 == 0]  -- [2,4,6,8,10]

-- Generate pairs
pairs = [(x, y) | x <- [1..3], y <- [1..3]]  -- [(1,1),(1,2),(1,3),(2,1),...]

-- map - applies a function to every element in a list
doubles = map (*2) [1..5]  -- [2,4,6,8,10]

-- filter - keeps only elements that satisfy a predicate
positives = filter (>0) [-2, -1, 0, 1, 2]  -- [1,2]

-- foldl - reduces a list to a single value (from left)
-- foldl function accumulator list
sumList = foldl (+) 0 [1, 2, 3, 4, 5]  -- 15

-- foldr - reduces a list from right
-- Different order can matter for non-associative operations
subtractAll = foldr (-) 0 [1, 2, 3]  -- 2 (1-(2-(3-0)))

-- List patterns and recursion
sumRecursive :: [Int] -> Int
sumRecursive [] = 0                           -- Base case
sumRecursive (x:xs) = x + sumRecursive xs     -- Recursive case

-- Take elements from a list
firstThree = take 3 [1..10]  -- [1,2,3]

-- Drop elements from a list
withoutFirstThree = drop 3 [1..10]  -- [4,5,6,7,8,9,10]

-- List functions: length, reverse, elem
listLength = length [1, 2, 3, 4]  -- 4
reversedList = reverse [1, 2, 3, 4]  -- [4,3,2,1]
containsFive = 5 `elem` [1, 2, 3, 4]  -- False

-- zip combines corresponding elements from two lists into pairs
zipped = zip [1, 2, 3] ["a", "b", "c"]  -- [(1,"a"),(2,"b"),(3,"c")]

-- Creating an infinite list (Haskell's lazy evaluation handles this)
infiniteList = [1..]  -- [1,2,3,...]
first10 = take 10 infiniteList  -- [1,2,3,4,5,6,7,8,9,10]

-- Finding elements
findFirstEven = head (filter even [1..10])  -- 2

-- List composition with pipe operator
processNumbers = sum . filter even . map (*3) $ [1..10]
-- Equivalent to: sum (filter even (map (*3) [1..10]))

main :: IO ()
main = do
    putStrLn "List processing examples:"
    print evens
    print (take 5 pairs)
    print doubles
    print positives
    print sumList
    print subtractAll
    print (sumRecursive [1..5])
    print firstThree
    print withoutFirstThree
    print zipped
    print first10
    print processNumbers
