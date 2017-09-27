module Fibonacci where

----------- Exercise 1 ------------
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fin n = fib (n-1) + fib (n-2)

fib1 :: [Integer]
fib1 = map fib [0..]

----------- Exercise 2 ------------
fib2 :: [Integer]
fib2 = gen 0 1
  where
    gen :: Integer -> Integer -> [Integer]
    gen x y = x : gen y (x + y)

----------- Exercise 3-------------
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

----------- Exercise 4 ------------
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons (f x) (streamFromSeed f (f $ f x))

----------- Exercise 5 ------------
nats :: Stream Integer
nats = gen 0
  where
    gen :: Integer -> Stream Integer
    gen x = Cons x (gen (x+1))

ruler :: Stream Integer
ruler = gen 1
  where
    maxPower :: Integer -> Integer
    maxPower x | even x    = 1 + maxPower (x `div` 2)
               | otherwise = 0

    gen :: Integer -> Stream Integer
    gen x = Cons (maxPower x) (gen (x+1))
