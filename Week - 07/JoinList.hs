{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Data.Monoid

import Buffer
import Editor
import Sized
import Scrabble

-- Ex: Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'b')
data JoinList m a
    = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-------------- Exercise 1 ---------------
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

-------------- Exercise 2 ---------------
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty   = Nothing
indexJ n (Single _ a)
  | n == 0       = Just a
  | otherwise    = Nothing
indexJ n (Append m left right)
  | n > sizeList = Nothing
  | n < sizeLeft = indexJ n left
  | otherwise    = indexJ (n-sizeLeft) right
  where
    sizeList  = getSize $ size m
    sizeLeft  = getSize $ size (tag left)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty    = Empty
dropJ n list@(Single _ _)
  | n > 0        = Empty
  | otherwise    = list
dropJ n (Append m left right)
  | n > sizeList = Empty
  | n < sizeLeft = dropJ n left +++ right
  | otherwise    = dropJ (n-sizeLeft) right
  where
    sizeList = getSize $ size m
    sizeLeft = getSize $ size (tag left)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty    = Empty
takeJ n list@(Single _ _)
  | n > 0        = list
  | otherwise    = Empty
takeJ n list@(Append m left right)
  | n > sizeList = list
  | n < sizeLeft = takeJ n left
  | otherwise    = left +++ takeJ (n - sizeLeft) right
  where
    sizeList = getSize $ size m
    sizeLeft = getSize $ size (tag left)

-------------- Exercise 3 ---------------
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

-------------- Exercise 4 ---------------
jlToList :: Monoid m => JoinList m a -> [a]
jlToList Empty                 = []
jlToList (Single _ x)          = [x]
jlToList (Append _ left right) = jlToList left ++ jlToList right

instance Buffer (JoinList (Score, Size) String) where
  toString   = unlines
             . jlToList

  fromString = foldr (+++) Empty
             . map (\str -> Single (scoreString str, Size 1) str)
             . lines

  line       = indexJ

  numLines   = getSize . snd . tag

  value      = getScore . fst . tag

  replaceLine n str jl = (takeJ (n-1) jl
                     +++ fromString str
                     +++ (dropJ n jl)

main = runEditor editor (fromString "My name is Siva Pavan Satya Narayana Varma" :: (JoinList (Score, Size) String))
