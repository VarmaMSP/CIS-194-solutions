{-# OPTIONS_GHC -Wall #-}
module Main where

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x | x <= 0    = []
           | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x <= 0    = []
              | otherwise = [x `mod` 10] ++ toDigitsRev (x `div` 10)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []  = []
doubleEveryOther (x : y : xs)
  | even $ length xs = 2*x : y : doubleEveryOther xs
  | otherwise        = x : doubleEveryOther (y : xs)

-- Exericse 3
sumDigits :: [Integer] -> Integer
sumDigits []      = 0
sumDigits (x : l) = (x `div` 10) + (x `mod` 10) + sumDigits l

-- Exercise 4
validate :: Integer -> Bool
validate x | x <= 0    = False
           | otherwise = (sumDigits $ doubleEveryOther $ toDigits x) `mod` 10 == 0


-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 src _ des     = [(src, des)]
hanoi n src inter des = hanoi (n-1) src des inter
                     ++ [(src, des)]
                     ++ hanoi (n-1) inter src des

-- Exercise 6
hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' 1 src _ _ des          = [(src, des)]
hanoi' 2 src _ inter des      = hanoi 2 src inter des
hanoi' n src inter inter1 des = hanoi' (n-2) src des inter1 inter
                             ++ hanoi 2 src inter1 des
                             ++ hanoi' (n-2) inter src inter1 des

{-
--Recurrence Pattern in hanoi problem

-- 2 Pegs
hanoi :: Integer -> Peg -> Peg -> [Move]
hanoi 1 src des = [(src, des)]

-- 3 Pegs
hanoi' :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi' 1 src _ des     = hanoi 1 src des
hanoi' n src inter des = hanoi' (n-1) src des inter
                      ++ hanoi 1 src des
                      ++ hanoi' (n-1) inter src des
-- 4 Pegs
hanoi'' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi'' 1 src _ _ des          = hanoi 1 src des
hanoi'' 2 src _ inter des      = hanoi' 2 src inter des
hanoi'' n src inter inter1 des = hanoi'' (n-2) src des inter1 inter
                              ++ hanoi' 2 src inter1 des
                              ++ hanoi'' (n-2) inter src inter1 des

-- 5 Pegs
hanoi''' :: Integer -> Peg -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi''' 1 src _ _ _ des               = hanoi 1 src des
hanoi''' 2 src _ _ inter des           = hanoi' 2 src inter des
hanoi''' 3 src _ inter inter1 des      = hanoi'' 3 src inter inter1 des
hanoi''' n src inter inter1 inter2 des = hanoi''' (n-3) src des inter1 inter2 inter
                                      ++ hanoi'' 3 src inter1 inter2 des
                                      ++ hanoi''' (n-3) inter src inter1 inter2 des

-- 6 Pegs
hanoi'''' :: Integer -> Peg -> Peg -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi'''' 1 src _ _ _ _ des                    = hanoi 1 src des
hanoi'''' 2 src _ _ _ inter des                = hanoi' 2 src inter des
hanoi'''' 3 src _ _ inter inter1 des           = hanoi'' 3 src inter inter1 des
hanoi'''' 4 src _ inter inter1 inter2 des      = hanoi''' 4 src inter inter1 inter2 des
hanoi'''' n src inter inter1 inter2 inter3 des = hanoi'''' (n-4) src des inter1 inter2 inter3 inter
                                              ++ hanoi''' 4 src inter1 inter2 inter3 des
                                              ++ hanoi'''' (n-4) inter src inter1 inter2 inter3 des
-}
