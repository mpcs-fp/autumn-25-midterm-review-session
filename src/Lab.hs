module Lab where

--- FOR INSPIRATION!!

append :: [a] -> [a] -> [a]
append lst1 [] = lst1
append [] lst2 = lst2
append (x : xs) lst2 = x : (append xs lst2)

reverse' :: [a] -> [a]
reverse' lst =
  let reverse'' [] acc = acc
      reverse'' (x : xs) acc = reverse'' xs (x : acc)
   in reverse'' lst []

maxmin :: [Float] -> (Float, Float)
maxmin lst =
  let infinity = 1 / 0
      smaller n m = if n < m then n else m
      bigger n m = if n >= m then n else m
   in foldr
        (\n (mini, maxi) -> (smaller n mini, bigger n maxi))
        (infinity, 0)
        lst

--- FOR INSPIRATION!!

-- Exercise 1.2.2
length' :: [a] -> Int
length' = undefined

-- Exercise 1.2.3
length'' :: [a] -> Int
length'' = undefined

-- Exercise 1.2.4
elem' :: Eq a => a -> [a] -> Bool
elem' = undefined

-- Exercise 1.2.5
elem'' :: Eq a => a -> [a] -> Bool
elem'' = undefined

-- Exercise 1.2.6
map' :: (a -> b) -> [a] -> [b]
map' = undefined

-- Exercise 1.2.7
map'' :: (a -> b) -> [a] -> [b]
map'' = undefined

-- Exercise 1.2.8
append' :: [a] -> [a] -> [a]
append' = undefined

-- Exercise 1.2.9
append'' :: [a] -> [a] -> [a]
append'' = undefined
