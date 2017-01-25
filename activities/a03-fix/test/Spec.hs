import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Lib
import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Fix Function" [
             testProperty "fix bubble is sorted" fixBubble
           , testProperty "fix pmod is gcd" (forAll posPair fixPmod)
   --      , testProperty "fox cos x = cos (fix cos x)" fixCos
   --      Commented it out because it doesn't converge on some platforms.
           ]
      ]

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) | x <= y = isSorted (y:xs)
                  | otherwise = False

fixBubble :: [Integer] -> Bool
fixBubble x = isSorted (fix bubble x)

posPair :: Gen (Integer, Integer)
posPair  = do a <- arbitrary `suchThat` (>0)
              b <- arbitrary `suchThat` (>0)
              return (a,b)

fixPmod :: (Integer, Integer) -> Bool
fixPmod x = let (a,b) = fix pmod x
                        in a == gcd a b && b == 0

fixCos :: Double -> Bool
fixCos x = let result = fix cos x
            in result == cos result
