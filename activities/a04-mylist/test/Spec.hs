import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Lib

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Composition of functions is identity" [
                testProperty "mylist2list (list2mylist xx) == xx" prop1
           ]
      ]

prop1 :: [Integer] -> Bool
prop1 x = mylist2list (list2mylist x) == x
