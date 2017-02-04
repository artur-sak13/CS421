--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Main where

main :: IO ()
main = return ()

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake n _ | (n <= 0)   = []
mytake _ []             = []  
mytake n (x:xs)         = x : mytake(n - 1) xs

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ []      = []
mydrop n (x:xs)
    | (n <= 0)   = x:xs
    | otherwise  = mydrop(n - 1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
-- Must be an O(n) operation
rev :: [a] -> [a]
rev [] = []
rev l  = rev' l []
	where
		rev' [] l     = l
		rev' (x:xs) l = rev' xs (x:l)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app  x []    = x
app [] y     = y
app (x:xs) y = x : app xs y

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist []     = []
inclist (x:xs) = x + 1 : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist []      = 0
sumlist (x:xs)  = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys
myzip  _      _     = []

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
-- Use myzip to implement
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs x y = aux (myzip x y)
	where
		aux []         = []
		aux ((x,y):xs) = (x + y) : aux xs 

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

-- Alternate approach using list comprehension found online shown below
-- fib = [a | (a,b) <- iterate (\(a,b) -> (b, a+b)) (0,1)]

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add x []        = [x]
add x (y:ys)
	| x > y     = y : add x ys
	| x == y    = y : ys
	| otherwise = x : y : ys

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union xs []     = xs
union [] ys     = ys
union (x:xs) (y:ys)
	| (x < y)   = x : union xs (y:ys)
	| (x == y)  = union xs (y:ys)
	| otherwise = y : union (x:xs) ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] _  = []
intersect _ []  = []
intersect(x:xs)(y:ys)
	| (x == y)  = x : intersect xs ys
	| (x < y)   = intersect xs (y:ys)
	| otherwise = intersect (x:xs) ys


--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = union l (map (x:) l)
        where l = powerset xs


--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' l = map (+1) l

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' l  = foldr (+) 0 l

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons []     = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil         = []
cons2list (Cons x xs) = x : cons2list xs

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp n)    = n
eval (PlusExp s)   = sum (map eval s)
eval (MultExp s)   = foldr (*) 1 (map eval s)

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' l = foldr Cons Nil l

--- ### BinTree

-- BinTree
data BinTree a = Node a (BinTree a) (BinTree a)
		| Leaf
	deriving (Show)


--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf         = 0
sumTree (Node x y z) = x + sumTree y + sumTree z

--- ### SimpVal

-- SimpVal
data SimpVal = IntVal Integer
		| BoolVal Bool
		| StrVal String
		| ExnVal String
	deriving(Show)


--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp f (IntVal x) (IntVal y) = IntVal (f x y)
liftIntOp f  _          _         = ExnVal "not an IntVal!"


-- Resources used
-- ______________

-- rev

-- http://stackoverflow.com/questions/33283905/recursive-functions-that-reverse-a-list-in-haskell
-- http://softwareengineering.stackexchange.com/questions/323270/implementing-map-with-tail-recursion

-- addpairs

-- https://www.quora.com/How-do-I-sum-tuples-in-Haskell

-- fib

-- List comprehension implementation. Actual function used in this MP was created after reading the hint
-- http://stackoverflow.com/questions/16386719/create-infinite-list-with-fibonacci-numbers

-- powerset

-- https://en.wikipedia.org/wiki/Power_set
-- https://www.reddit.com/r/programming/comments/225f0/beautiful_haskell_implementation_of_maths_power/
-- https://mail.haskell.org/pipermail/haskell-cafe/2003-June/004484.html
