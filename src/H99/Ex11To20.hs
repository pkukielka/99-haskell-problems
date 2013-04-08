module H99.Ex11To20 where

import           Control.Arrow
import           Data.Tuple
import           H99.Ex01To10

-------------------------------------------------------------------------------
-- Problem 11
-- Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element
-- has no duplicates it is simply copied into the result list.
-- Only elementswith duplicates are transferred as (N E) lists.
-------------------------------------------------------------------------------
data ModifiedLength a = Multiple Int a | Single a
  deriving (Show, Eq)

encodeModified :: (Eq a) => [a] -> [ModifiedLength a]
encodeModified = map encodeModified' . encode
  where
    encodeModified' (1, x) = Single x
    encodeModified' (k, x) = Multiple k x

-------------------------------------------------------------------------------
-- Problem 12
-- Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.
-------------------------------------------------------------------------------
decodeModified :: (Eq a) => [ModifiedLength a] -> [a]
decodeModified = foldr decodeModified' []
  where
    decodeModified' (Multiple k x) acc = replicate k x ++ acc
    decodeModified' (Single x)     acc = x:acc

-------------------------------------------------------------------------------
-- Problem 13
-- Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
-------------------------------------------------------------------------------
encodeDirect :: (Eq a) => [a] -> [ModifiedLength a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' 1 x xs
  where
    encodeDirect' 1 y []                   = [Single y]
    encodeDirect' k y []                   = [Multiple k y]
    encodeDirect' k y (y':ys') | y == y'   = encodeDirect' (k + 1) y' ys'
                               | otherwise = encodeDirect' k y [] ++ encodeDirect' 1 y' ys'

-------------------------------------------------------------------------------
 --Problem 14
 --Duplicate the elements of a list.
-------------------------------------------------------------------------------
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

dupli' :: [a] -> [a]
dupli' xs = concat [[x, x] | x <- xs]

dupli'' :: [a] -> [a]
dupli'' = concatMap (replicate 2)

-------------------------------------------------------------------------------
-- Problem 15
-- Replicate the elements of a list a given number of times.
-------------------------------------------------------------------------------
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-------------------------------------------------------------------------------
-- Problem 16
-- Drop every N'th element from a list.
-------------------------------------------------------------------------------
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n -1) xs ++ dropEvery (drop n xs) n

-------------------------------------------------------------------------------
-- Problem 17
-- Split a list into two parts; the length of the first part is given.
-------------------------------------------------------------------------------
split :: [a] -> Int -> ([a], [a])
split = moveN []
  where
    moveN acc (y:ys) n | n > 0  = moveN (acc ++ [y]) ys (n - 1)
    moveN acc ys _              = (acc, ys)

split' :: [a] -> Int -> ([a], [a])
split' (x:xs) n | n > 0 = (:) x . fst &&& snd $ split xs (n - 1)
split' xs _             = ([], xs)

-------------------------------------------------------------------------------
-- Problem 18
-- Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits included).
-- Start counting the elements with 1.
-------------------------------------------------------------------------------
slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k - i + 1) $ drop (i - 1) xs

-------------------------------------------------------------------------------
-- Problem 19
-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
-------------------------------------------------------------------------------
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n = let l = length xs in uncurry (++) $ swap $ splitAt (n `mod` l + l `mod` l) xs

-------------------------------------------------------------------------------
-- Problem 20
-- Remove the K'th element from a list.
-------------------------------------------------------------------------------
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)
