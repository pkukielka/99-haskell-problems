module H99.Ex01To10 where

import           Control.Arrow

-------------------------------------------------------------------------------
-- Problem 1
-- Find the last element of a list.
-------------------------------------------------------------------------------
myLast :: [a] -> a
myLast []  = error "List is empty"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' = head . reverse

-------------------------------------------------------------------------------
-- Problem 2
-- Find the last but one element of a list.
-------------------------------------------------------------------------------
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs
myButLast []     = error "List is too short"

myButLast' :: [a] -> a
myButLast' (x:_:[]) = x
myButLast' (_:xs)   = myButLast' xs
myButLast' []       = error "List is too short"

myButLast'' :: [a] -> a
myButLast'' = head . tail . reverse

-------------------------------------------------------------------------------
-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
-------------------------------------------------------------------------------
elementAt :: [a] -> Int -> a
elementAt (x:_) 1           = x
elementAt (_:xs) k | k  > 0 = elementAt xs (k - 1)
elementAt _ _               = error "Index out of bounds"

-------------------------------------------------------------------------------
-- Problem 4
-- Find the number of elements of a list.
-------------------------------------------------------------------------------
myLength :: [a] -> Int
myLength []   = 0
myLength (_:xs) = myLength xs + 1

myLength' :: [a] -> Int
myLength' = foldl (\n _ -> n + 1) 0

myLength'' :: [a] -> Int
myLength'' = foldl (const . (+1)) 0

myLength''' :: [a] -> Int
myLength''' = foldr (\_ n -> n + 1) 0

myLength'''' :: [a] -> Int
myLength'''' = foldr (\_ -> (+1)) 0

myLength''''' :: [a] -> Int
myLength''''' = foldr (const (+1)) 0

-------------------------------------------------------------------------------
-- Problem 5
-- Reverse a list.
-------------------------------------------------------------------------------
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' (x:xs) = reverse xs ++ [x]

myReverse'' :: [a] -> [a]
myReverse'' = myReverse''' []
  where
    myReverse''' acc []     = acc
    myReverse''' acc (x:xs) = myReverse''' (x:acc) xs

-------------------------------------------------------------------------------
-- Problem 6
-- Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. (x a m a x).
-------------------------------------------------------------------------------
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' []  = True
isPalindrome' [_] = True
isPalindrome' xs  = (head xs == last xs) && isPalindrome' (init $ tail xs)

-------------------------------------------------------------------------------
-- Problem 7
-- Flatten a nested list structure.
-------------------------------------------------------------------------------
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x)  = [x]
myFlatten (List xs) = foldl flt [] xs
  where
    flt acc (Elem x)  = acc ++ [x]
    flt acc (List ys) = foldl flt acc ys

myFlatten' :: NestedList a -> [a]
myFlatten' (Elem x)      = [x]
myFlatten' (List [])     = []
myFlatten' (List (x:xs)) =  myFlatten x ++ myFlatten' (List xs)

myFlatten'' :: NestedList a -> [a]
myFlatten'' (Elem x)  = [x]
myFlatten'' (List xs) = concatMap myFlatten'' xs

-------------------------------------------------------------------------------
-- Problem 8
-- Eliminate consecutive duplicates of list elements.
-------------------------------------------------------------------------------
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress (filter (/=x) xs)

-------------------------------------------------------------------------------
-- Problem 9
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
-------------------------------------------------------------------------------
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = let (hd, rest) = span (==head xs) xs
           in hd : pack rest

-------------------------------------------------------------------------------
-- Problem 10
-- Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length
-- encoding data compression method.  Consecutive duplicates of elements are
-- encoded as lists (N E) where N is the number of duplicates of the element E.
-------------------------------------------------------------------------------
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (length &&& head) . pack
