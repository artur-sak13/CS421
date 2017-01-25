module Lib
    ( someFunc
    , fix
    , bubble
    , pmod
    ) where

-- bubble
--  Performs one pass of bubble sort
--  Just so we'd have something to test fix with.

bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs) | x <= y    = x : bubble (y:xs)
                | otherwise = y:x:bubble xs

-- pmod
--   takes (a,b) and returns (b, a mod b) when a >= b
--   swaps otherwise
--   the fixpoint of this will compute GCD of a and b.

pmod (a,0) = (a,0)
pmod (a,b) | a >= b    = (b, a `mod` b)
           | otherwise = (b,a)

-- Implent fix
-- YOUR CODE HERE!
fix :: Eq a => (a->a) -> a -> a
fix f x = 	if x == result
			then x
			else fix f result
	where result = f x

someFunc :: IO ()
someFunc = putStrLn "someFunc"
