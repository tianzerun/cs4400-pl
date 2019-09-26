{- |
Module      :  Assignment1
Description :  Assignment 1 submission for CS 5400
Copyright   :  (c) Zerun Tian

Maintainer  :  tian.ze@husky.neu.edu
-}

module Assignment1 where


-- Task 1
fibonacci :: Integer -> Integer
fibonacci n = fibt n 0 1

fibt :: Integer -> Integer -> Integer -> Integer
fibt 0 a b = a
fibt n a b = fibt (n - 1) b (a + b)


-- Task 2
isIntegerElem :: Integer -> [Integer] -> Bool
isIntegerElem n [] = False
isIntegerElem n (e : l) | e == n = True
                        | otherwise = isIntegerElem n l


-- Task 3
isElem :: Eq a => a -> [a] -> Bool
isElem n [] = False
isElem n (e : l) | e == n = True
                 | otherwise = isElem n l


-- Task 4
-- count :: {- constraint -} => {- element type -} -> {- list type -} -> Integer
count :: Eq a => a -> [a] -> Integer
count n [] = 0
count n (e : l) | e == n = 1 + count n l
                | otherwise = count n l


-- Task 5
data Tree a = Node (Tree a) a (Tree a)
            | Empty
            deriving (Show, Eq)

tree1 :: Tree Integer
tree1 = Node (Node (Node (Node Empty 30 Empty)
                         40 
                         (Node Empty 50 Empty))
                   15 
                   (Node Empty 50 Empty))
             18 
             (Node (Node Empty 8 (Node Empty 13 Empty)) 
                    20 
                    Empty)


-- Task 6
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node l e r) = inOrder l ++ [e] ++ inOrder r    

