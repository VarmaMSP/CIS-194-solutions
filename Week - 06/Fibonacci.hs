module Fibonacci where

----------- Exercise 1 ------------
fib :: Integer -> Interger
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
streamMap f (Cons x xs) = cons (f x) (streamMap xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = const (f x) (stream f $ f x)

----------- Exercise 5 ------------
nats :: Stream Integer
nats = gen 0
  where
    gen :: Integer -> Stream
    gen x = Cons x (gen (x+1))
