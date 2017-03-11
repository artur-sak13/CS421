module Lib
    ( someFunc
    , Exp(..)
    , display
    , parse
    ) where

import Text.Regex.TDFA

data Exp = PlusExp Exp Exp
         | IntExp Integer
         | VarExp String
         | LetExp String Exp Exp
    deriving (Show,Eq)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

display :: Exp -> String
display (VarExp v) = v
display (PlusExp e1 e2) = "+ " ++ display e1 ++ " " ++ display e2
display (LetExp v e1 e2) = "let " ++ v ++ " = " ++ display e1 ++ " in " ++ display e2 ++ " end"
display (IntExp i) = show i

isInt :: String -> Bool
isInt i = i =~ "[0-9]+"

isSymbol :: String -> String -> Bool
isSymbol s v = s == v

parseSymbol s (x:xs) =
  if s == x
     then (s,xs)
     else error $ "Parse error, expected " ++ s ++ " but got " ++ x ++ "."

-- Grammar
--
-- E -> + E E
--    | int
--    | var
--    | ( E )
--    | let var = E in E end


parse xx = parseE (words xx)

parseE (x:xs) |  isSymbol "+" x = 
    let (e1, r1) = parseE xs
        (e2, r2) = parseE r1
    in (PlusExp e1 e2, r2)

parseE (x:xs) 
    | isInt x = (IntExp (read x), xs)
    | otherwise = let var = xs in parseE var
