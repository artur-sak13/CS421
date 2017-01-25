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
mytake = undefined

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop = undefined

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev = undefined

--- ### app

-- don't forget to put the type declaration or you will lose points!
app = undefined

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist = undefined

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist = undefined

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip = undefined

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs = undefined

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones = undefined

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats = undefined

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib = undefined

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add = undefined

--- ### union

-- don't forget to put the type declaration or you will lose points!
union = undefined

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect = undefined

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset = undefined

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' = undefined

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' = undefined

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
list2cons = undefined

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list = undefined

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval = undefined

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' = undefined

--- ### BinTree

-- BinTree

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree = undefined

--- ### SimpVal

-- SimpVal

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp = undefined
