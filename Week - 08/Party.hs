{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Data.Tree
import Data.List (sort)

import Employee

-------------- Exercise 1 ---------------
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp : emps) ((empFun emp) + fun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL emps fun) (GL emps' fun') = GL (emps ++ emps') (fun + fun')

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl gl' | gl <= gl' = gl'
               | otherwise = gl

-------------- Exercise 2 ---------------
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f node = f (rootLabel node) (map (treeFold f) (subForest node))

-------------- Exercise 3 ---------------
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (onlyBoss, mempty)
  where
    onlyBoss    = GL [boss] (empFun boss)
nextLevel boss gl = (withBoss, withoutBoss)
  where
    withBoss    = glCons boss $ mconcat $ map snd gl
    withoutBoss = mconcat $ map (uncurry moreFun) gl

-------------- Exercise 4 ----------------
maxFun :: Tree Employee -> GuestList
maxFun = (uncurry moreFun) . treeFold nextLevel

-------------- Exercise 5 ----------------
instance Show GuestList where
  show (GL emps fun) = unlines ["Total Fun: " ++ show fun
                               , unlines $ sort $ map empName emps
                               ]

main :: IO ()
main = readFile "./company.txt" >>= print . maxFun . read
