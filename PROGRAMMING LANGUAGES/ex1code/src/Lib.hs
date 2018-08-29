{-# LANGUAGE ParallelListComp #-}

module Lib
    ( add
    , isVowel
    , Status(..)
    , amountOf
    , fib
    , ending
    , takeInt
    , fizzbuzz
    , printFizz
    , listOfEven
    , zipped
    , cartesian
    ) where

import Control.Monad (mapM_)

-- TASK 1
-- Simple functions

-- finish the function "add" that takes two integers
-- and returns the sum of them
add :: Int -> Int -> Int
add n m = n+m

-- complete the function "isVowel" which
-- takes a character and returns True
-- if it's a vowel (English language), False otherwise
-- hint: a string is a list
-- hint2: use `elem` from Prelude
isVowel :: Char -> Bool
isVowel chr =   elem  chr ['a','e','i','o','u','A','E','I','O','U']

data Status = One | Two | Three | None deriving (Show, Eq)

-- complete the function "amountOf" which takes
-- a name and a list of subjects
-- it then pattern matches on the amount of subjects in the list
-- and returns a tuple of a Status (look above, describes the amount of subjects in the list)
-- and the name
amountOf :: String -> [a] -> (Status, String)
amountOf  name  point= (getStatus(length point),name)

getStatus 1=One
getStatus 2=Two
getStatus 3=Three
getStatus n=None


-- TASK 2
-- Recursion

-- finish the function "fib" that calculates the
-- nth fibonacci number 
-- assuming that 0th = 0 and 1st = 1
-- do not optimize it
fib :: Int -> Int
fib 0=0
fib 1=1
fib n=fib(n-1)+ fib(n-2)

-- TASK 3
-- Working with lists

-- complete the function "ing" that takes a list of
-- strings and returns them with the ending -ing
-- if the string is empty, remove it from the list
ending :: [String] -> [String]
ending  collection=[k++"ing"|k<-collection,k/=[]]

-- complete the function "takeInt" that
-- an integer n and a list of integers and
-- returns the first n elements of 
-- the list
takeInt :: Int -> [Int] -> [Int]
takeInt  n list = take n list

-- implement "fizzbuzz" as described in exercise 1
fizzbuzz :: [String]
fizzbuzz=[getFizzOrBuzz n |n<-[1..100]]

--fizzbuzz = fb [1..100]

--fb [] = []
--fb (x:xs) = getFizzOrBuzz x:fb xs
            --where
getFizzOrBuzz x
    | x `mod` 15 == 0  = "FizzBuzz"
    | x `mod` 3  == 0  = "Fizz"
    | x `mod` 5  == 0  = "Buzz"
    | otherwise  = show x 


printFizz :: IO ()
printFizz = mapM_ putStrLn fizzbuzz

-- TASK 4
-- List comprehensions

-- create a list "listOfEven" which contains all 
-- even numbers that are equal or greater than 0
-- use a list comprehension
listOfEven :: [Integer]
listOfEven = [n|n<-[0..],mod n 2 ==0]

-- create a list of tuples, "zipped"
-- where each tuple contains the nth entry
-- in the lists [1..26] and ['a'..'z']
-- hint: parallel list comprehension
zipped :: [(Int, Char)]
--[(i, j) | i <- [1..3] | j <- ["One", "Two", "Three"] ]
--zipped=zip [1..26] ['a'..'z']
zipped=[(i,j)|i<-[1..26]|j<-['a'..'z']]
-- create a list that contains the cartesian
-- product of the two vectors [4, 6, 8]
-- and [3, 7, 9]
-- use a list comprehension
cartesian :: [(Int, Int)]
cartesian = [(a,b) | a <- as, b <-bs]  
as = [4,6,8]
bs = [3,7,9]

