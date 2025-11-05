module Maps where

import Data.Map qualified as M

-- Problem 1 (Map.map): Curve all student scores by a fixed amount.
-- Map.map :: (a -> b) -> M.Map k a -> M.Map k b
-- Task: Write addCurve :: Int -> M.Map String Int -> M.Map String Int that adds c
-- to every score.
-- Solution:
addCurve :: Int -> M.Map String Int -> M.Map String Int
addCurve c = M.map (+ c)

-- Example:
--   addCurve 5 (M.fromList [("alice",80),("bob",90)])
--   = M.fromList [("alice",85),("bob",95)]

-- Problem 2 (Map.union, left-biased): Resolve layered settings so that command-line
-- overrides environment, which overrides base.
-- Map.union :: M.Map k a -> M.Map k a -> M.Map k a
-- Task: Write resolveSettings :: M.Map String String -> M.Map String String ->
-- M.Map String String -> M.Map String String where resolveSettings base env cli
-- applies precedence cli > env > base.
-- Solution: Map.union is left-biased, so earlier maps take precedence.
resolveSettings
  :: M.Map String String
  -> M.Map String String
  -> M.Map String String
  -> M.Map String String
resolveSettings base env cli = M.union cli (M.union env base)

-- Example:
-- base = M.fromList [("timeout","30"),("host","prod")]
-- env = M.fromList [("timeout","60")]
-- cli = M.fromList [("host","localhost")]
-- resolveSettings base env cli
--   = M.fromList [("host","localhost"),("timeout","60")]

-- Problem 3 (Map.unionWith): Combine two inventories by summing quantities per SKU.
-- Map.unionWith :: (a -> a -> a) -> M.Map k a -> M.Map k a -> M.Map k a
-- Task: Write combineInventory :: M.Map String Int -> M.Map String Int ->
-- M.Map String Int that sums on key collisions.
-- Solution:
combineInventory :: M.Map String Int -> M.Map String Int -> M.Map String Int
combineInventory = M.unionWith (+)

-- Example:
-- invA = M.fromList [("A",10),("B",3)]
-- invB = M.fromList [("B",5),("C",2)]
-- combineInventory invA invB = M.fromList [("A",10),("B",8),("C",2)]

-- Problem 4 (Map.intersection): Find keys common to both maps, keeping values from
-- the first map.
-- Map.intersection :: M.Map k a -> M.Map k b -> M.Map k a
-- Task: Write commonStudents :: M.Map String Int -> M.Map String Int ->
-- M.Map String Int that returns students present in both classes, keeping scores
-- from the first class.
-- Solution:
commonStudents :: M.Map String Int -> M.Map String Int -> M.Map String Int
commonStudents = M.intersection

-- Example:
-- class1 = M.fromList [("alice",90),("bob",85),("charlie",92)]
-- class2 = M.fromList [("alice",88),("charlie",95),("david",87)]
-- commonStudents class1 class2 = M.fromList [("alice",90),("charlie",92)]

-- Problem 5 (Map.intersectionWith): Compute revenue per SKU only where both price
-- and quantity exist.
-- Map.intersectionWith :: (a -> b -> c) -> M.Map k a -> M.Map k b ->
-- M.Map k c
-- Task: Write revenue :: M.Map String Int -> M.Map String Int -> M.Map String Int
-- where revenue prices qty multiplies values on shared keys.
-- Solution:
revenue :: M.Map String Int -> M.Map String Int -> M.Map String Int
revenue = M.intersectionWith (*)

-- Example:
-- prices = M.fromList [("A",100),("B",250),("D",400)]
-- qty = M.fromList [("A",2),("C",1),("B",3)]
-- revenue prices qty = M.fromList [("A",200),("B",750)]

-- Problem 6 (Map.mapMaybe): Filter a map to keep only positive values, dropping
-- zeros and negatives.
-- Map.mapMaybe :: (a -> Maybe b) -> M.Map k a -> M.Map k b
-- Task: Write keepPositives :: M.Map String Int -> M.Map String Int that keeps only
-- keys with positive values, dropping zero and negative values.
-- Solution:
keepPositives :: M.Map String Int -> M.Map String Int
keepPositives = M.mapMaybe (\x -> if x > 0 then Just x else Nothing)

-- Example:
-- scores = M.fromList [("alice",90),("bob",0),("charlie",-5),("david",85)]
-- keepPositives scores = M.fromList [("alice",90),("david",85)]

-- Problem 7 (Map.lookup): Safely look up a value in a Map, returning Maybe.
-- Map.lookup :: k -> M.Map k a -> Maybe a
-- Task: Write getScore :: String -> M.Map String Int -> Maybe Int that looks up
-- a student's score, returning Nothing if the student isn't in the map.
-- Solution:
getScore :: String -> M.Map String Int -> Maybe Int
getScore = M.lookup

-- Example:
-- scores = M.fromList [("alice",90),("bob",85)]
-- getScore "alice" scores = Just 90
-- getScore "charlie" scores = Nothing

-- Problem 8 (Map.findWithDefault): Look up a value with a default fallback.
-- Map.findWithDefault :: a -> k -> M.Map k a -> a
-- Task: Write getScoreOrDefault :: Int -> String -> M.Map String Int -> Int that
-- looks up a score, returning a default value if the key doesn't exist.
-- Solution:
getScoreOrDefault :: Int -> String -> M.Map String Int -> Int
getScoreOrDefault = M.findWithDefault

-- Example:
-- scores = M.fromList [("alice",90),("bob",85)]
-- getScoreOrDefault 0 "alice" scores = 90
-- getScoreOrDefault 0 "charlie" scores = 0

-- Problem 9 (Map lookups with map): Use a Map as a lookup table to transform
-- a list.
-- Task: Write lookupGrades :: [String] -> M.Map String Char -> [Maybe Char]
-- that looks up each name in the list and returns their grade, or Nothing if not
-- found. Use map and M.lookup.
-- Solution:
lookupGrades :: [String] -> M.Map String Char -> [Maybe Char]
lookupGrades names gradeMap = map (\name -> M.lookup name gradeMap) names

-- Example:
-- grades = M.fromList [("alice",'A'),("bob",'B')]
-- lookupGrades ["alice","charlie","bob"] grades = [Just 'A',Nothing,Just 'B']

-- Problem 10 (Map lookups to modify structures): Use Map lookups to compute
-- total cost from a shopping list.
-- Task: Write totalCost :: [String] -> M.Map String Int -> Int that looks up
-- each item's price and sums them, using 0 for missing items. Use map,
-- M.findWithDefault, and sum.
-- Solution:
totalCost :: [String] -> M.Map String Int -> Int
totalCost items prices = sum . map (\item -> M.findWithDefault 0 item prices) $ items

-- Example:
-- prices = M.fromList [("apple",2),("banana",1),("cherry",3)]
-- totalCost ["apple","banana","apple","unknown"] prices = 5

