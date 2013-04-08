module Main where

import           H99.Ex01To10
import           H99.Ex11To20

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

prop_split :: ([Int] -> Int -> ([Int], [Int])) -> [Int] -> Int -> Bool
prop_split f xs n = f xs n == splitAt n xs

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
      testCase "multiple values"     ([1, 2, 3, 4]  @=? myFlatten (List ([List [Elem 1, Elem 2], List [List [Elem 3]], Elem 4]))),
      testCase "single elem"         (["abcdef"]    @=? myFlatten (Elem "abcdef")),
      testCase "char array"          ("xyz"         @=? myFlatten (List ([List [Elem 'x', List []], List [List [Elem 'y']], Elem 'z'])))
    ],
    testGroup "compress" [
      testProperty "compress"        (prop_compress compress)
    ],
    testGroup "pack" [
      testProperty "pack"            (prop_pack pack)
    ],
    testGroup "encode" [
      testCase "empty list"          ([]            @=? encode ([] :: [Int])),
      testCase "single element"      ([(1, 1)]      @=? encode [1]),
      testCase "multiple values"     ([(1, 'a'), (2, 'b'), (1, 'c'), (2, 'b'), (1, 'd'), (2, 'a')]
                                                    @=? encode ("abbcbbdaa"))
    ],
    testGroup "encodeModified" [
      testCase "empty list"          ([]            @=? encodeModified ([] :: [Int])),
      testCase "single element"      ([Single 1]    @=? encodeModified [1]),
      testCase "multiple values"     ([Single 'a', Multiple 2 'b', Single 'c', Multiple 2 'b', Single 'd', Multiple 2 'a']
                                                    @=? encodeModified ("abbcbbdaa"))
    ],
    testGroup "decodeModified" [
      testCase "empty list"          ([]            @=? decodeModified ([] :: [ModifiedLength Int])),
      testCase "single element"      ([1]           @=? decodeModified [Single 1]),
      testCase "multiple values"     ("abbcbbdaa"   @=? decodeModified [Single 'a', Multiple 2 'b', Single 'c', Multiple 2 'b', Single 'd', Multiple 2 'a'])
    ],
    testGroup "encodeDirect" [
      testCase "empty list"          ([]            @=? encodeDirect ([] :: [Int])),
      testCase "single element"      ([Single 1]    @=? encodeDirect [1]),
      testCase "multiple values"     ([Single 'a', Multiple 2 'b', Single 'c', Multiple 2 'b', Single 'd', Multiple 2 'a']
                                                    @=? encodeDirect ("abbcbbdaa"))
    ],
    testGroup "dupli" [
      testCase "empty list"          ([]            @=? dupli ([] :: [Int])),
      testCase "single element"      ([1, 1]        @=? dupli [1]),
      testCase "multiple values"     ("aabbccdddd"  @=? dupli "abcdd")
    ],
    testGroup "dropEvery" [
      testCase "empty list"          ([]            @=? dropEvery ([] :: [Int]) 3),
      testCase "single element"      ([1, 4, 7]     @=? dropEvery [1, 2, 4, 6, 7, 11] 2),
      testCase "multiple values"     ("atdfhlja"    @=? dropEvery "atsdf5hlsja" 3)
    ],
    testGroup "split" [
      testProperty "split"           (prop_split split),
      testProperty "split'"          (prop_split split')
    ],
    testGroup "slice" [
      testCase "empty list 1"        (""            @=? slice "" 1 3),
      testCase "empty list 2"        (""            @=? slice "" 3 3),
      testCase "single elem"         ("c"           @=? slice "abcdefghijk" 3 3),
      testCase "subset"              ("cdefg"       @=? slice "abcdefghijk" 3 7),
      testCase "left border"         ("abcdefg"     @=? slice "abcdefghijk" 1 7),
      testCase "right border"        ("cdefghijk"   @=? slice "abcdefghijk" 3 11),
      testCase "full match"          ("abcdefghijk" @=? slice "abcdefghijk" 1 11)
    ],
    testGroup "rotate" [
      testCase "empty list"          (""            @=? rotate "" 4),
      testCase "rotate by 0"         ("abcdefghijk" @=? rotate "abcdefghijk" 0),
      testCase "rotate forward"      ("defghijkabc" @=? rotate "abcdefghijk" 3),
      testCase "full forward"        ("bcdefghijka" @=? rotate "abcdefghijk" 12),
      testCase "rotate backwards"    ("jkabcdefghi" @=? rotate "abcdefghijk" (-2)),
      testCase "full backward"       ("hijkabcdefg" @=? rotate "abcdefghijk" (-15))
    ],
    testGroup "removeAt" [
      testCase "removeAt beggining"  (('a', "bcde") @=? removeAt 1 "abcde"),
      testCase "removeAt end"        (('c', "abde") @=? removeAt 3 "abcde"),
      testCase "removeAt middle"     (('e', "abcd") @=? removeAt 5 "abcde")
    ]
  ]

main :: IO ()
main = defaultMain tests
