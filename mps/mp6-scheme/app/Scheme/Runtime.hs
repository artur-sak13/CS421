{-# LANGUAGE FlexibleContexts #-}

module Scheme.Runtime where

import Scheme.Core
import Scheme.Parse
import Scheme.Eval

import qualified Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable

--- ### Helper functions for lifting and lowering

lowerBool :: Val -> Bool
lowerBool (Boolean False) = False
lowerBool _ = True

lowerInt :: Val -> EvalState Int
lowerInt (Number i) = return i
lowerInt v = throwError $ TypeError v

lowerList :: Val -> EvalState [Val]
lowerList (List xx) = return xx
lowerList v = throwError $ TypeError v

liftIntVargOp :: (Int -> Int -> Int) -> Int -> Val
liftIntVargOp f c = PrimFunc p where
  p [] = return $ Number c
  p [x] = Number . f c <$> lowerInt x
  p xx = Number . foldl1 f <$> mapM lowerInt xx

liftBoolVargOp :: ([Bool] -> Bool) -> Val
liftBoolVargOp f = PrimFunc $ return . Boolean . f . map lowerBool

-- TODO
liftIntBinOp :: (Int -> Int -> Int) -> Val
liftIntBinOp _ =
  -- You should replace the following line with your own implementation
  PrimFunc . const $ unimplemented "Lifting binary integer operator (`liftIntBinOp`)"

-- TODO
liftIntUnaryOp :: (Int -> Int) -> Val
liftIntUnaryOp _ =
  -- You should replace the following line with your own implementation
  PrimFunc . const $ unimplemented "Lifting unary integer operator (`liftIntUnaryOp`)"

liftBoolUnaryOp :: (Bool -> Bool) -> Val
liftBoolUnaryOp f = PrimFunc p where
  p [Boolean x] = return $ Boolean $ f x
  p v = throwError $ UnexpectedArgs v

-- TODO
liftCompOp :: (Int -> Int -> Bool) -> Val
liftCompOp _ =
  -- You should replace the following line with your own implementation
  PrimFunc . const $ unimplemented "Lifting comparison operator (`liftCompOp`)"

--- ### Primtive operations

-- Primitive function `car`
-- TODO
car :: [Val] -> EvalState Val
car = const $ unimplemented "Primitive function `car`"

-- Primitive function `cdr`
-- TODO
cdr :: [Val] -> EvalState Val
cdr = const $ unimplemented "Primitive function `cdr`"

-- Primitive function `cons`
-- TODO
cons :: [Val] -> EvalState Val
cons = const $ unimplemented "Primitive function `cons`"

-- Primitive function `append`
append :: [Val] -> EvalState Val
append [] = return $ List []
append [x] = return x
append vv = foldlM append' (List []) (map flattenList vv) where
  append' (List []) x = return x
  append' (List xs) (List ys) = return $ List (xs ++ ys)
  append' (List xs) (DottedList ys y) = return $ DottedList (xs ++ ys) y
  append' _ acc = throwError $ TypeError acc

-- Primitive function `apply`
-- It applies a function to a list of parameters
-- TODO
-- Examples:
--   (apply + '(1 2 3))  => 6
--   (apply car '((1 2 3)))  => 1
applyPrim :: [Val] -> EvalState Val
applyPrim = const $ unimplemented "Primitive function `apply`"

-- Primitive function `eval`
-- It evaluates the single argument as an expression
-- All you have to do is to check the number of arguments and
-- feed the single argument to the evaluator!
-- TODO
-- Examples:
--   (eval '(+ 1 2 3))  => 6
evalPrim :: [Val] -> EvalState Val
evalPrim = const $ unimplemented "Primitive function `eval`"

-- Primitive function `=`, throwing type error for mismatch
-- `=` is a comparison operator for numbers and booleans
-- TODO
-- Examples:
--   (= 1 1) => #t
--   (= #f #t) => #f
--   (= #f #f) => #t
--   (= 'a 10) => Type error
--   (= 'a 'b) => Type error
equalSign :: [Val] -> EvalState Val
equalSign = const $ unimplemented "Primitive function `=`"

-- Primitive function `eq?`, not throwing any error
-- `eq?` is a comparison operator for atom values (numbers, booleans, and symbols)
-- Returns `#f` on type mismatch or unsupported types (functions etc)
-- TODO
-- Examples:
--   (eq? 1 1) => #t
--   (eq? #f #t) => #f
--   (eq? #f #f) => #t
--   (eq? 'a 10) => #f
--   (eq? 'a 'a) => #t
eq :: [Val] -> EvalState Val
eq = const $ unimplemented "Primitive function `eq?`"

-- Primitive function `list?` predicate
-- `(list? arg)` determines whether `arg` is a non-dotted list
-- or an empty list (null)
-- TODO
isList :: [Val] -> EvalState Val
isList = const $ unimplemented "Primitive function `list?`"

-- Primitive function `symbol?` predicate
-- TODO
isSymbol :: [Val] -> EvalState Val
isSymbol = const $ unimplemented "Primitive function `symbol?`"

-- Primitive function `pair?` predicate
-- Any `List` or `DottedList` is a pair
-- TODO
isPair :: [Val] -> EvalState Val
isPair = const $ unimplemented "Primitive function `pair?`"

-- Primitive function `number?` predicate
-- TODO
isNumber :: [Val] -> EvalState Val
isNumber = const $ unimplemented "Primitive function `number?`"

-- Primitive function `boolean?` predicate
-- TODO
isBoolean :: [Val] -> EvalState Val
isBoolean = const $ unimplemented "Primitive function `boolean?`"

-- Primitive function `null?` predicate
-- An empty list or its *equivalent* value is null
-- Note: Think about what's equivalent
-- TODO
isNull :: [Val] -> EvalState Val
isNull = const $ unimplemented "Primitive function `null?`"

--- ### Runtime

runtime :: Env
runtime = H.fromList [ ("+", liftIntVargOp (+) 0)
                     , ("-", liftIntVargOp (-) 0)
                     , ("and", liftBoolVargOp and)
                     , ("or", liftBoolVargOp or)
                     , ("cons", PrimFunc cons)
                     , ("append", PrimFunc append)
                     , ("symbol?", PrimFunc isSymbol)
                     , ("list?", PrimFunc isList)
                     -- TODO: Insert more runtime bindings here
                     ]
