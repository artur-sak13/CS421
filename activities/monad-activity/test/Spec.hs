import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Lib

main :: IO ()
main = defaultMain tests

tests = [
  testGroup "Counter Monad" [
      testProperty "Counts bindings" countsBindings,
      testProperty "Preserves data" preservesData
      ]
  ]

inc x = return $ x + 1
plus a b = return $ a + b

countsBindings (x,y) =
  let Counter p i = addCounters x y
   in i == 4

preservesData (x,y) =
  let Counter p i = addCounters x y
      result = x + y + 2
   in result == p

addCounters :: Integer -> Integer -> Counter Integer
addCounters x y =
  do a <- return x
     b <- return y
     aa <- inc a
     bb <- inc b
     plus aa bb
