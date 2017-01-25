module Lib
    ( someFunc
    , chop
    ) where

-- Your code here!
-- Feel free to write helper functions.

-- chop :: [Int] -> [Int]

chop :: [Int] -> [Int]
chop (x:xs)
	| (null xs && (x /= 0))	= (x - 1) : []
	| (null xs && (x == 0)) = x : []
	| (x == 0) 				= x : chop xs
	| otherwise 			= (x - 1) : (xs !! 0 + (length xs) : drop 1 xs)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
