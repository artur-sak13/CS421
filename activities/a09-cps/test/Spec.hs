import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Lib hiding (dropMax,dropMaxk,multifoo,evenk,foo,fix)

main :: IO ()
main = defaultMain tests

tests = [
  testGroup "Fixk Function" [
      testProperty "Works like dropMax" worksLikeDropMax
      ]
  , testGroup "fook Function" [
      testProperty "Works like foo" worksLikeFoo
      ]
  , testGroup "multifook Function" [
      testProperty "Works like multifoo" worksLikeMultifoo
      ]
  ]

fix f x = 
  if x == result
    then x
    else fix f result
  where result = f x

multifoo p xx = fix (foo p) xx

foo p [] = []
foo p [x] | p x       = [x]
          | otherwise = []
foo p (x:y:xs) | p x = 2 * x + y : foo p xs
               | otherwise = x : foo p (y:xs)

worksLikeDropMax :: [Int] -> Bool
worksLikeDropMax xx =
  fixk dropMaxk xx (\res -> res == fix dropMax xx)

worksLikeFoo :: [Int] -> Bool
worksLikeFoo xx =
  fook evenk xx (\res -> res == foo even xx)

worksLikeMultifoo :: [Int] -> Bool
worksLikeMultifoo xx =
  multifook evenk xx (\res -> res == multifoo even xx)

evenk x k = k $ even x

dropMax [] = []
dropMax [x] = [x]
dropMax (x:y:xs) =
  if x < y then x : dropMax xs
   else y : dropMax xs

dropMaxk []       k = k []
dropMaxk [x]      k = k [x]
dropMaxk (x:y:xs) k =
  if x < y then dropMaxk xs (\z -> k $ x:z)
   else dropMaxk xs (\z -> k $ y:z)
