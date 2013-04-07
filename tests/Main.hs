module Main where

import           H99.Ex01To10

import           Data.List
import           Test.Framework                       as TF (Test, defaultMain,
                                                             testGroup)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
import           Test.QuickCheck

prop_myLast :: ([Int] -> Int) -> [Int] -> Property
prop_myLast f xs = not (null xs) ==> f xs == last xs

prop_myButLast :: ([Int] -> Int) -> [Int] -> Property
prop_myButLast f xs = length xs > 1 ==> f xs == xs !! (length xs - 2)

prop_elementAt :: ([Int] -> Int -> Int) -> [Int] -> Int -> Property
prop_elementAt f xs k = k <= length xs && k >= 1 ==> f xs k == xs !! (k - 1)

prop_myLength :: ([Int] -> Int) -> [Int] -> Bool
prop_myLength f xs = f xs == length xs

prop_myReverse :: ([Int] -> [Int]) -> [Int] -> Bool
prop_myReverse  f xs = f xs == reverse xs

prop_isPalindrome :: ([Int] -> Bool) -> [Int] -> Bool
prop_isPalindrome f xs = f xs == (xs == reverse xs)

prop_compress :: ([Int] -> [Int]) -> [Int] -> Bool
prop_compress f xs = f xs == nub xs

prop_pack :: ([Int] -> [[Int]]) -> [Int]  -> Bool
prop_pack f xs = f xs == group xs

tests :: [TF.Test]
tests =
  [
    testGroup "myLast" [
      testProperty "myLast"          (prop_myLast myLast),
      testProperty "myLast'"         (prop_myLast myLast')
    ],
    testGroup "myButLast" [
      testProperty "myButLast"       (prop_myButLast myButLast),
      testProperty "myButLast'"      (prop_myButLast myButLast'),
      testProperty "myButLast''"     (prop_myButLast myButLast'')
    ],
    testGroup "elementAt" [
      testProperty "elementAt"       (prop_elementAt elementAt)
    ],
    testGroup "myLength" [
      testProperty "myLength"        (prop_myLength myLength),
      testProperty "myLength'"       (prop_myLength myLength'),
      testProperty "myLength''"      (prop_myLength myLength''),
      testProperty "myLength'''"     (prop_myLength myLength'''),
      testProperty "myLength''''"    (prop_myLength myLength''''),
      testProperty "myLength'''''"   (prop_myLength myLength''''')
    ],
    testGroup "myReverse" [
      testProperty "myReverse"       (prop_myReverse myReverse),
      testProperty "myReverse'"      (prop_myReverse myReverse'),
      testProperty "myReverse''"     (prop_myReverse myReverse'')
    ],
    testGroup "isPalindrome" [
      testProperty "isPalindrome"    (prop_isPalindrome isPalindrome),
      testProperty "isPalindrome'"   (prop_isPalindrome isPalindrome')
    ],
    testGroup "myFlatten" [
      testCase "empty list"          ([]            @=? myFlatten (List ([] :: [NestedList Int]))),
      testCase "empty list of lists" ([]            @=? myFlatten (List ([List [], List [List []]] :: [NestedList Int]))),
      testCase "multiple values"     ([1, 2, 3, 4]  @=? myFlatten (List ([List [Elem 1, Elem 2], List [List [Elem 3]], Elem 4] :: [NestedList Int]))),
      testCase "single elem"         (["abcdef"]    @=? myFlatten (Elem "abcdef" :: NestedList String)),
      testCase "char array"          ("xyz"         @=? myFlatten (List ([List [Elem 'x', List []], List [List [Elem 'y']], Elem 'z'] :: [NestedList Char])))
    ],
    testGroup "compress" [
      testProperty "compress"        (prop_compress compress)
    ],
    testGroup "pack" [
      testProperty "pack"            (prop_pack pack)
    ],
    testGroup "encode" [
      testCase "empty list"          ([]            @=? encode ([] :: [Int])),
      testCase "single element"      ([(1, 1)]      @=? encode ([1] :: [Int])),
      testCase "multiple values"     ([(1, 'a'), (2, 'b'), (1, 'c'), (2, 'b'), (1, 'd'), (2, 'a')]
                                                    @=? encode ("abbcbbdaa" :: String))
    ]
  ]

main :: IO ()
main = defaultMain tests
