module Lists where

import Data.List (intercalate)

-- Problem 1 (words): Split a string into a list of words using Prelude's words.
-- words :: String -> [String]
-- Task: Write splitWords :: String -> [String] that splits on whitespace.
-- Use words from Prelude instead of manual recursion.
-- Solution:
splitWords :: String -> [String]
splitWords = words

-- Example:
-- splitWords "hello world haskell" = ["hello","world","haskell"]
-- splitWords "foo  bar" = ["foo","bar"]

-- Problem 2 (intercalate): Join a list of strings with a delimiter using
-- Data.List functions.
-- intercalate :: [a] -> [[a]] -> [a]
-- Task: Write joinWithComma :: [String] -> String that joins with commas.
-- Use intercalate from Data.List instead of manual folding.
-- Solution:
joinWithComma :: [String] -> String
joinWithComma = intercalate ","

-- Example:
-- joinWithComma ["apple","banana","cherry"] = "apple,banana,cherry"
-- joinWithComma [] = ""

-- Problem 3 (unlines, zip): Combine two lists into pairs and format as lines
-- using library functions.
-- unlines :: [String] -> String
-- zip :: [a] -> [b] -> [(a, b)]
-- Task: Write formatPairs :: [String] -> [Int] -> String that pairs names with
-- ages and formats as "name:age" lines. Use zip, map, and unlines.
-- Solution:
formatPairs :: [String] -> [Int] -> String
formatPairs names ages = unlines . map (\(n, a) -> n ++ ":" ++ show a) $ 
  zip names ages

-- Example:
-- formatPairs ["Alice","Bob","Charlie"] [25,30,22]
--   = "Alice:25\nBob:30\nCharlie:22"
-- formatPairs [] [] = ""

-- Problem 4 (filter, map): Filter and transform using Prelude combinators.
-- filter :: (a -> Bool) -> [a] -> [a]
-- map :: (a -> b) -> [a] -> [b]
-- Task: Write squarePositives :: [Int] -> [Int] using filter and map.
-- Avoid manual recursion or folding.
-- Solution:
squarePositives :: [Int] -> [Int]
squarePositives = map (^ 2) . filter (> 0)

-- Example:
-- squarePositives [1, -2, 3, -4, 5] = [1, 9, 25]
-- squarePositives [-1, -2, -3] = []

-- Problem 5 (unwords, map, show): Convert a list of numbers to a space-
-- separated string using library functions.
-- unwords :: [String] -> String
-- map :: (a -> b) -> [a] -> [b]
-- show :: Show a => a -> String
-- Task: Write numbersToString :: [Int] -> String that converts to
-- space-separated string. Use map, show, and unwords from Prelude.
-- Solution:
numbersToString :: [Int] -> String
numbersToString = unwords . map show

-- Example:
-- numbersToString [1, 2, 3] = "1 2 3"
-- numbersToString [] = ""

