--- Given Code
--- ==========

module Main where

import Data.List (intercalate)

-- for stack underflow errors
underflow :: a
underflow = error "Value stack underflow!"

--- The Types
--- ---------

type ForthState = (IStack, CStack, Dictionary, Output)
type IStack     = [Integer]
type CStack     = [[String]]
type Dictionary = [(String, [Entry])]
type Output     = [String]

data Entry = Prim (IStack -> IStack)
           | Def [String]
           | Num Integer
           | Unknown String

instance Show Entry where
    show (Prim f)    = "Prim"
    show (Def s)     = show s
    show (Num i)     = show i
    show (Unknown s) = "Unknown: " ++ s

--- Dictionary Access
--- -----------------

--- ### Lookups

-- handle input lookups (and integers)
dlookup :: String -> Dictionary -> Entry
dlookup word dict
    = case lookup word dict of
        Just (x:_) -> x
        _          -> case reads word of
                        [(i,"")] -> Num i
                        _        -> Unknown word

--- ### Insert

-- handle inserting things into the dictionary
dinsert :: String -> Entry -> Dictionary -> Dictionary
dinsert key val dict
    = case lookup key dict of
        Just vals -> (key, val:vals) : dict
        Nothing   -> (key, [val])    : dict

--- Initial State
--- -------------

-- initial integer stack
initialIStack :: IStack
initialIStack = []

-- initial call stack
initialCStack :: CStack
initialCStack = []

-- initial output
initialOutput :: [String]
initialOutput = []

-- initial ForthState
initialForthState :: ForthState
initialForthState = (initialIStack, initialCStack, initialDictionary, initialOutput)

--- The Read-Eval-Print Loop
--- ------------------------

repl :: ForthState -> IO ()
repl state
    = do putStr "> "
         input <- getLine
         if input == "quit"
            then do putStrLn "Bye!"
                    return ()
            else let (is, cs, d, output) = eval (words input) state
                 in  do mapM_ putStrLn output
                        repl (is, cs, d, [])

main = do putStrLn "Welcome to your Forth interpreter!"
          repl initialForthState


--- Problems
--- ========

--- Lifters
--- -------

--- ### `liftIntOp`

liftIntOp :: (Integer -> Integer -> Integer) -> IStack -> IStack
liftIntOp op (x:y:xs) = (y `op` x) : xs
liftIntOp _  _        = underflow

--- ### `liftCompOp`

liftCompOp :: (Integer -> Integer -> Bool) -> IStack -> IStack
liftCompOp op (x:y:xs)
    | (y `op` x) = -1:xs
    | otherwise  = 0:xs

liftCompOp _ _ = underflow

--- The Dictionary
--- --------------

initialDictionary :: Dictionary
initialDictionary = initArith ++ initComp

--- ### Arithmetic Operators

initArith :: Dictionary
initArith = [
    ("+",  [Prim (liftIntOp (+))]),
    ("-", [Prim (liftIntOp (-))]),
    ("*", [Prim (liftIntOp (*))]),
    ("/", [Prim (liftIntOp (div))])
    ]

--- ### Comparison Operators

initComp :: Dictionary
initComp = [
    ("<", [Prim (liftCompOp (<))]),
    (">", [Prim (liftCompOp (>))]),
    ("<=", [Prim (liftCompOp (<=))]),
    (">=", [Prim (liftCompOp (>=))]),
    ("==", [Prim (liftCompOp (==))]),
    ("!=", [Prim (liftCompOp (/=))])
    ]

--- The Parser
--- ----------

-- get the first well-nested string of tokens and the rest
splitWellNested :: Eq a => (a, a) -> [a] -> ([a], [a])
splitWellNested (start,end) words = splitWN 0 [] words
    where
        splitWN 0 acc (word:rest)
            | word == end    = (reverse acc, rest)
        splitWN n acc (word:rest)
            | word == start  = splitWN (n+1) (word:acc) rest
            | word == end    = splitWN (n-1) (word:acc) rest
            | otherwise      = splitWN n     (word:acc) rest
        splitWN _ acc []     = (reverse acc, [])

--- ### Input Parser `splitIf`

-- ifs have an optional `else` which also must be well-nested
splitIf :: [String] -> ([String], [String], [String])

splitIf list = let 
    (a,b) = (splitWellNested ("if","then")list)
    (c,d) = (splitWellNested ("if","else") a)
    (e,f) = (splitWellNested ("else","then") a)
    in case f of 
        [] -> (c,d,b)
        _ -> (e++["then"],tail f, b)

--- The Evaluator
--- -------------

eval :: [String] -> ForthState -> ForthState

-- empty input and empty call stack -> return current state
eval [] (istack, [],       dict, out) = (istack, [], dict, reverse out)

-- empty input and non-empty call stack -> pop element off call stack
eval [] (istack, c:cstack, dict, out) = eval c (istack, cstack, dict, out)

eval (".":words) (i:istack, cstack, dict, out)
    = eval words (istack, cstack, dict, show i : out)
eval (".":words) _ = underflow

--- ### Printing the Stack
eval (".S":words) (istack, cstack, dict, out)
    = eval words (istack, cstack, dict, (intercalate " " $ map show (reverse istack)):out)

--- ### Stack Manipulations
-- 'dup'

eval ("dup":words) ([], cstack, dict, out) = ([], cstack, dict, out)
eval ("dup":words) (i:istack, cstack, dict, out) =
    eval words (i:i:istack, cstack, dict, out)

-- 'swap'
eval("swap":words)(i:j:istack, cstack, dict, out) =
    eval words (j:i:istack, cstack, dict, out)
eval("swap":words) _ = underflow

-- 'drop'
eval ("drop":words)([], cstack, dict, out) = ([], cstack, dict, out)
eval ("drop":words)(i:istack, cstack, dict, out) = 
    eval words(istack, cstack, dict, out)

-- 'rot'

eval ("rot" :words)(i:j:k:istack, cstack, dict, out) =
    eval words (k:i:j:istack, cstack, dict, out)

eval ("rot" :words) _ = underflow

--- ### User definitions

eval (":":words)(istack, cstack, dict, out) = 
    let
        ((k:v), rest) = splitWellNested (":",";") words
        hash = dinsert k (Def v) dict
    in
        eval rest (istack, cstack, hash, out)

--- ### Conditionals

eval ("if":words) (i:istack, cstack, dict, out) = 
    let 
        (tru, fal, thn) = splitIf words
    in case i of 
        0  -> eval (fal++thn) (istack, cstack, dict, out)
        _  -> eval (tru++thn) (istack, cstack, dict, out)

--- ### Loops
eval ("begin":words)(istack, cstack, dict, out) = 
    let (x, xs) = splitWellNested ("begin", "again") words
    in eval x (istack, ("begin":x):xs:cstack, dict, out)

--- ### Lookup in dictionary

eval ("exit":words)(istack, cstack, dict, out) =
    if(cstack == []) 
        then underflow 
        else 
            case (head cstack) of
                ("begin":res) -> eval [] (istack, (tail cstack), dict, out)
                []            -> eval [] (istack, [], dict,out)
                otherwise     -> eval ("exit":words) (istack, (tail cstack), dict, out)

-- otherwise it should be handled by `dlookup` to see if it's a `Num`, `Prim`,
-- `Def`, or `Unknown`
eval (word:words) (istack, cstack, dict, out)
    = case dlookup word dict of
        Def def   -> eval def   (istack,   words:cstack, dict, out)
        Prim f    -> eval words (f istack, cstack,       dict, out)
        Num i     -> eval words (i:istack, cstack,       dict, out)
        Unknown s -> eval [] (istack, [], dict, ("Unknown symbol: " ++ s) : out)
