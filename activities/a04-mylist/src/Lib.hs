module Lib
    ( someFunc
    , list2mylist
    , mylist2list
    ) where

data MyList a = Cons a (MyList a)
              | Nil
   deriving (Show,Eq)

list2mylist :: [a] -> MyList a
list2mylist []     = Nil
list2mylist (x:xs) = Cons x (list2mylist xs)  

mylist2list :: MyList a -> [a]
mylist2list Nil         = []
mylist2list (Cons x xs) = x : mylist2list xs

someFunc :: IO ()
someFunc = putStrLn "someFunc"
