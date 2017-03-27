import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Lib
import Test.QuickCheck
import Test.QuickCheck.Monadic

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
            , testProperty "Parser works with parens." (forAll arbitrary displayParsesParen)
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

notReserved s = s /= "let" && s /= "in" && s /= "end"

varName = suchThat (resize 4 (listOf1 $ elements ['a'..'z'])) notReserved

displayParses exp =
  let (e1,[]) = parse (display exp)
    in e1 == exp

displayParsesParen exp = monadicIO $ do
  estr <- run (displayParen exp)
  let (e1,[]) = parse estr
  assert (e1 == exp)

-- guaranteed to be a random value of 4
-- https://xkcd.com/221/
xkcd :: Int
xkcd = 4

range1_xkcd :: Gen Int
range1_xkcd = resize 1 $ choose (1::Int,xkcd)

coinFlip :: IO Bool
coinFlip = generate range1_xkcd >>= (\x -> return (x>2))

parify :: String -> IO String
parify s = coinFlip >>= (\b -> if b then return (" ( " ++ s ++ " ) ") else return s)

displayParen :: Exp -> IO String
displayParen (VarExp v) = parify v
displayParen (PlusExp e1 e2) = do
  e1str <- displayParen e1
  e2str <- displayParen e2
  return ("+ " ++ e1str ++ " " ++ e2str)
displayParen (LetExp v e1 e2) = do
  e1str <- displayParen e1
  e2str <- displayParen e2
  return ("let " ++ v ++ " = " ++ e1str ++ " in " ++ e2str ++ " end")
displayParen (IntExp i) = parify (show i)
