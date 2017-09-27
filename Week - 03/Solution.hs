{-# OPTIONS_GHC -Wall #-}
module Solution where

-- Exercise 1
skips :: [a] -> [[a]]
skips x = map (nthElementList x) [1..(length x)]
  where
    nthElementList :: [a] -> Int -> [a]
    nthElementList xs n =
      case drop (n-1) xs of
        (y : ys) -> y : nthElementList ys n
        []       -> []

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : xs)
  | x < y && y > z = y : localMaxima (y : z : xs)
  | otherwise      = localMaxima (y : z : xs)
localMaxima _      = []

-- Exercise 3
