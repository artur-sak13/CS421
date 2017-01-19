module Lib
    ( someFunc
    , chop
    ) where

-- Your code here!
-- Feel free to write helper functions.

-- chop :: [Int] -> [Int]

chop :: [Int] -> [Int]
chop [] = []
chop (0:xs) = 0 : chop xs
chop [x] = [x-1]
chop (x:y:xs) = (x - 1) : y+l : xs
    where l = length xs + 1

someFunc :: IO ()
someFunc = putStrLn "someFunc"
