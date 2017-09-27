{-# OPTIONS_GHC -Wall #-}
module Solution where

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum
     . filter even
     . takewhile (>1)
     . iterate (\x -> if even x then (x `div` 2) else (3 * x + 1))

-- Exercise 2
data Tree a
    = Leaf
    | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

insert :: a -> (Tree a) -> (Tree a)
insert x Leaf                                        = Node 1 Leaf x Leaf
insert x (Node h left@(Leaf) y right@(Leaf))         = Node (h+1) (insert x left) y right
insert x (Node h left@(Node _ _ _ _) y right@(Leaf)) = Node (h+1) left y (insert x right)
insert x (Node h left@(Leaf) y right@(Node _ _ _ _)) = Node (h+1) (insert x left) y right
insert x (Node h left@(Node lh _ _ _) y right@(Node rh _ _ _))
  | lh > rh   = Node (h+1) left y (insert x right)
  | otherwise = Node (h+1) (insert x left) y right

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

{- utility function to check if a tree is balanced -}
isBalanced :: Tree a -> Bool
isBalanced Leaf           = True
isBalanced (Node 1 _ _ _) = True
isBalanced (Node 2 _ _ _) = True
isBalanced (Node _ left@(Node lh _ _ _) _ right@(Node rh _ _ _))
                          = isBalanced left && abs (rh - lh) <= 1 && isBalanced right
isBalanced (Node _ _ _ _) = False

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (/=) True
    . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (g f) []
  where
    g :: (a -> b) -> a -> [b] -> [b]
    g f' x y = (f' x) : y

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f = foldr (g f)
  where
    g :: (a -> b -> a) -> b -> a -> a
    g f' x y = f' y x

-- Exercise 4
