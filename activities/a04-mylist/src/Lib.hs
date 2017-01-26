module Lib
    ( someFunc
    , list2mylist
    , mylist2list
    ) where

data MyList a = Cons a (MyList a)
              | Nil
   deriving (Show,Eq)

list2mylist :: [a] -> MyList a
list2mylist = undefined

mylist2list :: MyList a -> [a]
mylist2list = undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"
