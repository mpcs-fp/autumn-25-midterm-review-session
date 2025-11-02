module AssignmentSpec
  ( spec,
  )
where

import Data.Char qualified as Char
import Data.List qualified as List
import Lab qualified
import Exercises qualified
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Fun, applyFun)

isValidLength :: (forall a. [a] -> Int) -> SpecWith ()
isValidLength f = do
  it "works for example 1" $ do
    f [1 :: Int, 2] `shouldBe` 2

  it "works for example 2" $ do
    f ['e' .. 'z'] `shouldBe` 22

  it "works for example 3" $ do
    f [1 :: Int .. 29] `shouldBe` 29

  it "works for example 4" $ do
    f [] `shouldBe` 0

  prop "behaves like Prelude's length" $
    \(l :: [Int]) ->
      f l `shouldBe` length l

isValidElem :: (forall a. Eq a => a -> [a] -> Bool) -> SpecWith ()
isValidElem f = do
  it "works for example 1" $ do
    f 1 [1, 2, 3 :: Int] `shouldBe` True

  it "works for example 2" $ do
    f 4 [1, 2, 3 :: Int] `shouldBe` False

  it "works for example 3" $ do
    f [] [[1, 2], [2, 3], [] :: [Int]] `shouldBe` True

  it "works for example 4" $ do
    f [] ([] :: [[Int]]) `shouldBe` False

  prop "behaves like Prelude's elem" $
    \(a :: Int, l :: [Int]) ->
      f a l `shouldBe` elem a l

isValidMap :: (forall a b. (a -> b) -> [a] -> [b]) -> SpecWith ()
isValidMap f = do
  it "works for example 1" $ do
    f (+ 1) [1, 2, 3, 4 :: Int] `shouldBe` [2, 3, 4, 5]

  it "works for example 2" $ do
    f Char.toUpper "kickin it functional style" `shouldBe` "KICKIN IT FUNCTIONAL STYLE"

  it "works for example 3" $ do
    f (\x -> [x, x]) [1, 2, 3 :: Int] `shouldBe` [[1, 1], [2, 2], [3, 3]]

  it "works for example 4" $ do
    f (\x -> x + 2) ([] :: [Int]) `shouldBe` []

  prop "behaves like Prelude's map" $
    \(m' :: Fun Int Int, l :: [Int]) -> do
      let m = applyFun m'

      f m l `shouldBe` map m l

isValidAppend :: (forall a. [a] -> [a] -> [a]) -> SpecWith ()
isValidAppend f = do
  prop "behaves like append" $
    \(a :: [Int], b :: [Int]) -> do
      f a b `shouldBe` Lab.append a b

isValidFilter :: (forall a. (a -> Bool) -> [a] -> [a]) -> SpecWith ()
isValidFilter f = do
  it "works for example 1" $ do
    f Char.isAlpha "abcdefg12345hijklmno67890" `shouldBe` "abcdefghijklmno"

  it "works for example 2" $ do
    f (\x -> length x == 1) [[1 :: Int, 2, 3], [4], [5], [6, 7, 8]] `shouldBe` [[4], [5]]

  it "works for example 3" $ do
    f (\x -> x `mod` 2 == 0) [1 :: Int, 2, 3, 4] `shouldBe` [2, 4]

  it "works for example 4" $ do
    f (\x -> x `mod` 2 == 0) [] `shouldBe` ([] :: [Int])

  prop "behaves like Prelude's filter" $
    \(p' :: Fun Int Bool, as :: [Int]) -> do
      let p = applyFun p'

      f p as `shouldBe` filter p as

isValidTakeWhile :: (forall a. (a -> Bool) -> [a] -> [a]) -> SpecWith ()
isValidTakeWhile f = do
  it "works for example 1" $ do
    f Char.isAlpha "Amber Tamblyn" `shouldBe` "Amber"

  it "works for example 2" $ do
    f Char.isUpper "Amber Tamblyn" `shouldBe` "A"

  it "works for example 3" $ do
    f Char.isSpace "Amber Tamblyn" `shouldBe` ""

  it "works for example 4" $ do
    f Char.isSpace "       Amber Tamblyn" `shouldBe` "       "

  it "works for example 5" $ do
    f (\_ -> False) "       Amber Tamblyn" `shouldBe` ""

  it "works for example 6" $ do
    f (\_ -> True) "" `shouldBe` ""

  prop "behaves like Prelude's takeWhile" $
    \(p' :: Fun Int Bool, as :: [Int]) -> do
      let p = applyFun p'

      f p as `shouldBe` takeWhile p as

isValidDropWhile :: (forall a. (a -> Bool) -> [a] -> [a]) -> SpecWith ()
isValidDropWhile f = do
  it "works for example 1" $ do
    f Char.isDigit "555-1234" `shouldBe` "-1234"

  it "works for example 2" $ do
    f Char.isAlpha "555-1234" `shouldBe` "555-1234"

  it "works for example 3" $ do
    f Char.isAlpha "" `shouldBe` ""

  it "works for example 4" $ do
    f Char.isSpace "      President Biden" `shouldBe` "President Biden"

  it "works for example 5" $ do
    f (\_ -> False) "President Biden" `shouldBe` "President Biden"

  it "works for example 6" $ do
    f (\_ -> True) "President Biden" `shouldBe` ""

  prop "behaves like Prelude's dropWhile" $
    \(p' :: Fun Int Bool, as :: [Int]) -> do
      let p = applyFun p'

      f p as `shouldBe` dropWhile p as

isValidDiff :: (forall a. Eq a => [a] -> [a] -> [a]) -> SpecWith ()
isValidDiff f = do
  it "works for example 1" $ do
    f [1 :: Int, 2, 3, 4, 5] [2, 3] `shouldBe` [1, 4, 5]

  it "works for example 2" $ do
    f [1 :: Int, 2, 3, 4, 5] [1, 5] `shouldBe` [2, 3, 4]

  it "works for example 3" $ do
    f [1 :: Int, 2, 3, 4, 5] [1, 3, 5] `shouldBe` [2, 4]

  it "works for example 4" $ do
    f [1 :: Int, 2, 3, 4, 5] [] `shouldBe` [1, 2, 3, 4, 5]

  it "works for example 5" $ do
    f [] [1 :: Int, 2, 3, 4, 5] `shouldBe` []

  prop "for lists with no duplicates, behaves like List's diff" $
    \(as' :: [Int], bs' :: [Int]) -> do
      let as = List.nub as'
          bs = List.nub bs'
      f as bs `shouldBe` (List.\\) as bs

isValidNub :: (forall a. Eq a => [a] -> [a]) -> SpecWith ()
isValidNub f = do
  it "works for example 1" $ do
    f [1 :: Int, 2, 3, 1, 2, 3] `shouldBe` [1, 2, 3]

  it "works for example 2" $ do
    f [1 :: Int, 1, 1, 2, 2, 2, 3, 3, 3] `shouldBe` [1, 2, 3]

  it "works for example 3" $ do
    f [4 :: Int, 5, 2, 6, 2, 3, 5, 6, 4, 2, 4, 1, 7, 2, 7, 8, 7, 3, 5, 4] `shouldBe` [4, 5, 2, 6, 3, 1, 7, 8]

  it "works for example 4" $ do
    f [] `shouldBe` ([] :: [Int])

  prop "behaves like List's nub" $
    \(as :: [Int]) -> do
      f as `shouldBe` List.nub as

spec :: Spec
spec = do
  describe "length'" $ do
    isValidLength Lab.length'

  describe "length''" $ do
    isValidLength Lab.length''

  describe "elem'" $ do
    isValidElem Lab.elem'

  describe "elem''" $ do
    isValidElem Lab.elem''

  describe "map'" $ do
    isValidMap Lab.map'

  describe "map''" $ do
    isValidMap Lab.map''

  describe "append'" $ do
    isValidAppend Lab.append'

  describe "append''" $ do
    isValidAppend Lab.append''

  describe "alphabet" $ do
    it "alphabet is \"abcd\"" $ do
      Exercises.alphabet `shouldBe` "abcd"

  describe "yayHaskell" $ do
    it "yayHaskell is \"haskell\"" $ do
      Exercises.yayHaskell `shouldBe` "haskell"

  describe "swapFirstTwo" $ do
    it "swaps letters in \"computers\"" $ do
      Exercises.swapFirstTwo "computers" `shouldBe` "ocmputers"

    it "swaps booleans" $ do
      Exercises.swapFirstTwo [False, True, False, False]
        `shouldBe`
        [True, False, False, False]

    it "swaps string tuples" $ do
      Exercises.swapFirstTwo [("let", "us"), ("go", "then"), ("you", "and")]
        `shouldBe`
        [("go","then"),("let","us"),("you","and")]

    it "does nothing to an empty string" $ do
      Exercises.swapFirstTwo "" `shouldBe` ""

  describe "prependOne" $ do
    it "works with short integer lists" $ do
      Exercises.prependOne [3,4,5] `shouldBe` [1,3,4,5]

    it "works with lists defined by ranges" $ do
      Exercises.prependOne [10..20]
        `shouldBe`
        [1,10,11,12,13,14,15,16,17,18,19,20]

    it "works with empty lists" $ do
      Exercises.prependOne [] `shouldBe` [1]

    it "works with singleton lists" $ do
      Exercises.prependOne [2] `shouldBe` [1,2]

  describe "filter'" $ do
    isValidFilter Exercises.filter'

  describe "filter''" $ do
    isValidFilter Exercises.filter''

  describe "takeWhile'" $ do
    isValidTakeWhile Exercises.takeWhile'

  describe "takeWhile''" $ do
    isValidTakeWhile Exercises.takeWhile''

  describe "dropWhile'" $ do
    isValidDropWhile Exercises.dropWhile'

  describe "dropWhile''" $ do
    isValidDropWhile Exercises.dropWhile''

  describe "diff" $ do
    isValidDiff Exercises.diff

  describe "diff'" $ do
    isValidDiff Exercises.diff'

  describe "nub" $ do
    isValidNub Exercises.nub

  describe "nub'" $ do
    isValidNub Exercises.nub'

  describe "partition" $ do
    it "works for example 1" $ do
      Exercises.partition [6, 3, 4, 1, 5, 2] 4 `shouldBe` ([3, 1, 2], 4, [6, 5])

    it "works for example 2" $ do
      Exercises.partition [1, 2, 3, 4, 5, 6] 4 `shouldBe` ([1, 2, 3], 4, [5, 6])

    it "works for example 3" $ do
      Exercises.partition [1] 1 `shouldBe` ([], 1, [])

    it "works for example 4" $ do
      Exercises.partition [4, 3, 2, 1] 4 `shouldBe` ([3, 2, 1], 4, [])

    it "works for example 5" $ do
      Exercises.partition [4, 3, 2, 1] 1 `shouldBe` ([], 1, [4, 3, 2])

  describe "quicksort" $ do
    it "works for example 1" $ do
      Exercises.quicksort [6, 3, 4, 1, 5, 2] `shouldBe` [1, 2, 3, 4, 5, 6]

    it "works for example 2" $ do
      Exercises.quicksort [4, 3, 2, 1] `shouldBe` [1, 2, 3, 4]

    it "works for example 3" $ do
      Exercises.quicksort [1] `shouldBe` [1]

    it "works for example 4" $ do
      Exercises.quicksort [] `shouldBe` []
