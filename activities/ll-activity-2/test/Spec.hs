import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Lib
import Test.QuickCheck

main :: IO ()
main = defaultMain tests

-- data Exp = PlusExp Exp Exp
--          | IntExp Integer
--          | VarExp String
--          | LetExp String Exp Exp
--     deriving Show


tests = [
        testGroup "Display Function" [
            testProperty "Parser works for small expressions." (forAll (arbExp 0) displayParses)
            , testProperty "Parser works for large expressions." (forAll arbitrary displayParses)
--                testProperty "Length is preserved" (forAll randHydra sameLength)
--           ,    testProperty "Difference is correct" (forAll randHydra difference)
           ]
      ]

instance Arbitrary Exp where
   arbitrary = sized arbExp

arbExp :: Int -> Gen Exp
arbExp 0 = do
    frequency [(1, IntExp <$> arbitrary)
              ,(1, VarExp <$> varName)]

arbExp n = do
  frequency [(1, PlusExp <$> arbExp (n-1) <*> arbExp 0)
            ,(1, PlusExp <$> arbExp 0 <*> arbExp (n-1))
            ,(1, LetExp <$> varName <*> arbExp (n-1) <*> arbExp 0)
            ,(1, LetExp <$> varName <*> arbExp 0 <*> arbExp (n-1))
            ]

varName = resize 4 (listOf1 $ elements ['a'..'z'])

displayParses exp =
  let (e1,_) = parse (display exp)
    in e1 == exp
