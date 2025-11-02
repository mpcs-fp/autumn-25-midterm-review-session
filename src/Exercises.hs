module Exercises where

import Prelude hiding
  ( dropWhile,
    filter,
    takeWhile,
  )

-- Exercise 2.1.1
alphabet :: String
alphabet = undefined
-- alphabet = 'a' : 'b' : 'c' : 'd'

-- Exercise 2.1.2
yayHaskell :: String
yayHaskell = undefined
-- yayHaskell = ['h', 'a', 's', 'k', 'e', 'l', 'l', []]

-- Exercise 2.1.3
swapFirstTwo :: [a] -> [a]
swapFirstTwo = undefined
-- swapFirstTwo [] = []
-- swapFirstTwo [x] = [x]
-- swapFirstTwo (x1 : x2 : xs) = x2 : x1 ++ xs

-- Exercise 2.1.4
prependOne :: [Int] -> [Int]
prependOne = undefined
-- prependOne ints = [1] ++ [ints]

-- Exercise 2.2.1
filter' :: (a -> Bool) -> [a] -> [a]
filter' = undefined

-- Exercise 2.2.2
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' = undefined

-- Exercise 2.2.3
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' = undefined

-- Exercise 2.2.4
takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' = undefined

-- Exercise 2.2.5
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' = undefined

-- Exercise 2.2.6
dropWhile'' :: (a -> Bool) -> [a] -> [a]
dropWhile'' = undefined

-- Exercise 2.2.7
diff :: Eq a => [a] -> [a] -> [a]
diff = undefined

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = diff

-- Exercise 2.2.8
diff' :: Eq a => [a] -> [a] -> [a]
diff' = undefined

-- Exercise 2.2.9
nub :: Eq a => [a] -> [a]
nub = undefined

-- Exercise 2.2.10
nub' :: Eq a => [a] -> [a]
nub' = undefined

-- Exercise 2.3.2
partition :: [Int] ->
             Int ->
             ([Int], Int, [Int])
partition = undefined

-- Exercise 2.3.3
quicksort :: [Int] -> [Int]
quicksort = undefined
