{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module                  : Lecture1
-- Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
-- SPDX-License-Identifier : MPL-2.0
-- Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
-- Stability               : Stable
-- Portability             : Portable
--
-- Exercises for the Lecture 1 of the Haskell Beginners course.
--
-- To complete exercises, you need to complete implementation and add
-- missing top-level type signatures. You can implement any additional
-- helper functions. But you can't change the names of the given
-- functions.
--
-- Comments before each function contain explanations and example of
-- arguments and expected returned values.
--
-- It's absolutely okay if you feel that your implementations are not
-- perfect. You can return to these exercises after future lectures and
-- improve your solutions if you see any possible improvements.
module Lecture1
  ( makeSnippet,
    sumOfSquares,
    lastDigit,
    minmax,
    subString,
    strSum,
    lowerAndGreater,
  )
where

-- VVV If you need to import libraries, do it after this line ... VVV

-- ^ ^^ and before this line. Otherwise the test suite might fail  ^^^

-- | Specify the type signature of the following function. Think about
-- its behaviour, possible types for the function arguments and write the
-- type signature explicitly.
makeSnippet :: Int -> [Char] -> [Char]
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

-- | Implement a function that takes two numbers and finds sum of
-- their squares.
--
-- >>> sumOfSquares 3 4
-- 25
--
-- >>> sumOfSquares (-2) 7
-- 53
--
-- Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
-- is 25.

-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
sumOfSquares :: Integer -> Integer -> Integer
sumOfSquares x y = (x * x) + (y * y)

-- | Implement a function that returns the last digit of a given number.
--
-- >>> lastDigit 42
-- 2
-- >>> lastDigit (-17)
-- 7
--
-- 🕯 HINT: use the @mod@ functionn

-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
lastDigit :: Int -> Int
lastDigit n = mod (abs n) 10


headOrDefault :: Int -> [Int] -> Int
headOrDefault def list =
  if null list
    then def
    else head list


headOrDefault2 :: a -> [a] -> a
headOrDefault2 def [] = def
headOrDefault2 _ list = head list


sign :: Int -> String
sign n
  | n == 0 = "Zero"
  | n < 0 = "Negative"
  | otherwise = "Positive"


-- Are the first 3 elements equal to the last 3 elements?
-- let .. in .. -> IN must be present, and it must return something. Variables must be used.
sameThreeAround :: [Int] -> Bool
sameThreeAround list =
  let firstThree = take 3 list
      lastThree = reverse (take 3 (reverse list))
   in firstThree == lastThree


-- where allows local assignment of variables / funcs
appendLastTwos :: [Int] -> [Int] -> [Int]
appendLastTwos list1 list2 = lastTwo list1 ++ lastTwo list2
  where
    lastTwo :: [Int] -> [Int]
    lastTwo l = reverse (take 2 (reverse l))


-- count num of eles equal to given number
-- count n list = go 0 list - This works because "list" is essentially crossed out in the below. both sides have it. The sub-func "go" knows it accepts a "l" list, and so we can apply it just the same
count :: Int -> [Int] -> Int
count n = go 0
  where
    go :: Int -> [Int] -> Int
    go result l
      | null l = result
      | head l == n = go (result + 1) (tail l)
      | otherwise = go result (tail l)

count2 :: Int -> [Int] -> Int
count2 n = go 0
  where
    go :: Int -> [Int] -> Int
    go result [] = result
    go result (x : xs)
      | x == n    = go (result + 1) xs
      | otherwise = go result       xs

------- HOF's -------

-- First-class
applyToSame :: (Int -> Int -> Int) -> Int -> Int
applyToSame f x = f x x

-- Lambda
satisfies :: (Int -> Bool) -> Int -> String
satisfies check n
  | check n    = "Pass"
  | otherwise = "Fail"

-- let a = satisfies (\x -> x > 0) 5


-- Partials
applyTwice :: (Integer -> Integer) -> Integer -> Integer
applyTwice f x = f (f x)


-- | Write a function that takes three numbers and returns the
-- difference between the biggest number and the smallest one.
--
-- >>> minmax 7 1 4
-- 6
--
-- Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
-- and 1 is the smallest, and 7 - 1 = 6.
--
-- Try to use local variables (either let-in or where) to implement this
-- function.

minmax :: Int -> Int -> Int -> Int
minmax x y z =
  let maxVal = max x (max y z)
      minVal = min x (min y z)
  in maxVal - minVal


-- | Implement a function that takes a string, start and end positions
-- and returns a substring of a given string from the start position to
-- the end (including).
--
-- >>> subString 3 7 "Hello, world!"
-- "lo, w"
--
-- >>> subString 10 5 "Some very long String"
-- ""
--
-- This function can accept negative start and end position. Negative
-- start position can be considered as zero (e.g. substring from the
-- first character) and negative end position should result in an empty
-- string.

subString :: Int -> Int -> [Char] -> [Char]
subString start end str = drop start (take (end + 1) str)


-- | Write a function that takes a String — space separated numbers,
-- and finds a sum of the numbers inside this string.
--
-- >>> strSum "100 -42  15"
-- 73
--
-- The string contains only spaces and/or numbers.
strSum :: [Char] -> Int
strSum str = sum (map read (words str))


parse :: [String] -> [Int]
parse = map read



-- | Write a function that takes a number and a list of numbers and
-- returns a string, saying how many elements of the list are strictly
-- greater than the given number and strictly lower.
--
-- >>> lowerAndGreater 3 [1 .. 9]
-- "3 is greater than 2 elements and lower than 6 elements"
--
-- Explanation: the list [1 .. 9] contains 9 elements: [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- The given number 3 is greater than 2 elements (1 and 2)
-- and lower than 6 elements (4, 5, 6, 7, 8 and 9).
--
-- 🕯 HINT: Use recursion to implement this function.
lowerAndGreater :: Int -> [Int] -> [Char]
lowerAndGreater n list = show n 
  ++ " is greater than " 
  ++ show (countCompare (< n) 0 list) 
  ++ " elements and lower than " 
  ++ show (countCompare (> n) 0 list) 
  ++ " elements"
  where
    countCompare :: (Int -> Bool) -> Int -> [Int] -> Int
    countCompare check result l
      | null l = result
      | check (head l) = countCompare check (result + 1) (tail l)
      | otherwise = countCompare check result (tail l)
