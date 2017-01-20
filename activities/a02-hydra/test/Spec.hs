import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Lib
import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Chop Function" [
                testProperty "Length is preserved" (forAll randHydra sameLength)
           ,    testProperty "Difference is correct" (forAll randHydra difference)
           ]
      ]

lottaZeros = frequency [ (2, arbitrary `suchThat` (\x -> x > 0 && x < 10))
                       , (1, return 0) ]

randHydra = listOf1 lottaZeros


sameLength :: [Int] -> Bool
sameLength x = length x == (length $ chop x)

nonzero :: Int -> [Int] -> [Int]
nonzero _ [] = []
nonzero n (0:xs) = nonzero n xs
nonzero 1 xs = xs
nonzero n (_:xs) = nonzero (n-1) xs

allzerobutlast [0] = False
allzerobutlast [n] = True
allzerobutlast (0:xs) = allzerobutlast xs
allzerobutlast (n:xs) = False

difference :: [Int] -> Bool
difference xx | all (== 0) xx = chop xx == xx
difference x | allzerobutlast x =
           length (filter (/= 0) diff) == 1
        && head nz1 == 1
        && allzerobutlast diff
    where chopped = chop x
          diff = zipWith (-) x chopped
          nz1 = nonzero 1 diff
difference x = length (filter (/= 0) diff) == 2
            && head nz1 == 1
            && head nz2 == - length nz2
    where chopped = chop x
          diff = zipWith (-) x chopped
          nz1 = nonzero 1 diff
          nz2 = nonzero 2 diff
