module Lib
    ( fix
    , fixk
    , foo
    , fook
    , multifoo
    , multifook
    , evenk
    , dropMax
    , dropMaxk
    ) where


-- Your problem
--
-- Using the techniques you learned in class, convert these functions to
-- equivalent CPS functions.

-- Here are the originals.

fix f x = 
  if x == result
    then x
    else fix f result
  where result = f x

multifoo p xx = fix (foo p) xx

foo p [] = []
foo p [x] | p x       = [x]
          | otherwise = []
foo p (x:y:xs) | p x = 2 * x + y : foo p xs
               | otherwise = x : foo p (y:xs)

-- Your code

-- Assume that the f argument takes a continuation now.

fixk = undefined

-- Write this by calling fixk.  The solution is one line.

multifook = undefined

fook = undefined

-- Utilities
-- Use these to test your code if you want

evenk x k = k $ even x

dropMax [] = []
dropMax [x] = [x]
dropMax (x:y:xs) =
  if x < y then x : dropMax xs
   else y : dropMax xs

dropMaxk []       k = k []
dropMaxk [x]      k = k [x]
dropMaxk (x:y:xs) k =
  if x < y then dropMaxk xs (\z -> k $ x:z)
   else dropMaxk xs (\z -> k $ y:z)
