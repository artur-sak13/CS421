{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Scheme.Eval where

import Scheme.Core

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except
import Debug.Trace
-- ### Evaluation helpers

-- Evaluates a symbol to string
-- Throws an error if value is not a symbol
-- Examples:
--   getSym (Symbol "x")  ==> "x"
--   getSym (Number 1)    ==> Not a symbol: x
getSym :: Val -> EvalState String
getSym (Symbol x) = return x
getSym         v  = throwError $ NotASymbol v

-- `let` and `let*`
getBinding :: Val -> EvalState (String, Val)
getBinding (List [c, e]) = liftM2 (,) (getSym c) (eval e)
getBinding v = throwError $ NotAListOfTwo v

-- Evaluates a list of two to a tuple
-- Throws an error if value is not a list of two
-- This is useful in special form `cond`, since each clause
-- is expected to be exactly a two-element list
-- Examples:
--   getListOf2 (List (Number 1) (Symbol "x"))
--     ==> ((Number 1), (Symbol "x"))
--   getListOf2 (List (Number 1))
--     ==> Not a list of two elements: (1)
getListOf2 :: Val -> EvalState (Val, Val)
getListOf2 (List [c, e]) = return (c, e)
getListOf2 v = throwError $ NotAListOfTwo v

--- ### Keywords

-- When evaluating special forms, a list form starting with a keyword
-- is expected to match the special form syntax.
keywords :: [String]
keywords = [ "define"
           , "lambda"
           , "cond"
           , "let"
           , "let*"
           , "define-macro"
           , "quasiquote"
           , "unquote"
           ]

-- ### The monadic evaluator
-- Unlike evaluators in previous MPs, `eval` does not take any environment!
-- This is because the environment is encapsulated in the `EvalState` monad.
-- To access the environment, all you have to do is `get`, `modify` or `put`!
eval :: Val -> EvalState Val

-- Self-evaluating expressions
eval v@(Number _) = return v
eval v@(Boolean _) = return v

-- Symbol evaluates to the value bound to it
eval (Symbol sym) =
  do env <- get
     case H.lookup sym env of
       Nothing -> throwError $ UndefSymbolError sym
       Just val -> return val

-- Dotted list may just be an equivalent representation of List.
-- We simply try to flatten the list. If it's still dotted after
-- flattening, it's an invalid expression.
eval expr@(DottedList _ _) = case flattenList expr of
  DottedList _ _ -> throwError $ InvalidExpression expr
  v -> eval v

-- List evaluates as a form of the following
-- 1. Special form (`define`, `let`, `let*`, `cond`, `quote`, `quasiquote`,
--    `unquote`, `define-macro`, ...)
-- 2. Macro expansion (Macro)
-- 3. Function application (Func)
-- 4. Primitive function application (PrimFunc)
eval expr@(List lst) = evalList $ map flattenList lst where
    --- Evaluator for forms
    invalidSpecialForm :: String -> EvalState e
    invalidSpecialForm frm = throwError $ InvalidSpecialForm frm expr

    evalList :: [Val] -> EvalState Val

    evalList [] = throwError $ InvalidExpression expr

    -- quote
    evalList [Symbol "quote", e] = return e

    -- unquote (illegal at surface evaluation)
    evalList [Symbol "unquote", e] = throwError $ UnquoteNotInQuasiquote e

    -- quasiquote
    evalList [Symbol "quasiquote", e] = evalQuasi 1 e where
      evalQuasi :: Int -> Val -> EvalState Val
      evalQuasi 0 (List [Symbol "unquote", v]) = throwError $ UnquoteNotInQuasiquote v
      evalQuasi 1 (List [Symbol "unquote", v]) = eval v
      evalQuasi n (List ee@[Symbol "quasiquote", _]) = List <$> evalQuasi (n+1) `mapM` ee
      evalQuasi n (List ee@[Symbol "unquote", _]) = List <$> evalQuasi (n-1) `mapM` ee
      evalQuasi n (List xx) = List <$> mapM (evalQuasi n) xx
      evalQuasi n (DottedList xx y) = DottedList <$> mapM (evalQuasi n) xx <*> evalQuasi n y
      evalQuasi _ v = return v

    -- Why comment these out? Because `if` can be defined as a macro!
    -- -- if-then
    -- evalList [Symbol "if", condE, thenE] =
    --   eval condE >>= \c -> if lowerBool c then eval thenE else return Void
    -- -- if-then-else
    -- evalList [Symbol "if", condE, thenE, elseE] =
    --   eval condE >>= \c -> eval $ if lowerBool c then thenE else elseE

    -- cond
    evalList ((Symbol "cond"):[]) = throwError $ InvalidSpecialForm "cond" expr
    evalList ((Symbol "cond"):rest) = evalCond rest where
      evalCond (cond : xs) =
        do (c,e) <- getListOf2 cond
           case c of
             List con -> do tmp <- eval c
                            evalCond $ (List([tmp,e]):xs)

             Boolean tf -> case tf of
               True -> eval e
               _ -> if not $ null xs then evalCond xs else return Void

             Symbol sym -> case sym of
               "else" -> if not $ null xs then throwError $ InvalidSpecialForm "cond" expr else eval e
               "True" -> throwError $ UndefSymbolError "True"
               "False" -> throwError $ UndefSymbolError "False"
               _ -> eval e

             dif -> eval e

    -- let
    evalList [Symbol "let", List(defs), body] =
      do env <- get
         lst <- H.fromList <$> mapM getBinding defs
         put $ H.union lst env
         v <- eval body
         put env
         return v

    -- let*
    evalList [Symbol "let*", List(defs), body] = evalLetStar defs body where
      evalLetStar [] body =
        do v <- eval body
           return v
      evalLetStar (x:xs) body =
        do env <- get
           (k,v) <- getBinding x
           modify $ H.insert k v
           v <- evalLetStar xs body
           put env
           return v


    -- lambda
    evalList [Symbol "lambda", List(params), body] =
      do env <- get
         val <- (\argVal -> Func argVal body env) <$> mapM getSym params
         return val

    -- define function
    evalList [Symbol "define", List (Symbol fname : args), body] =
      do env <- get
         val <- (\argVal -> Func argVal body env) <$> mapM getSym args
         modify $ H.insert fname val
         return Void

    -- define variable
    evalList[Symbol "define", (Symbol x), sexp] =
      do env <- get
         val <- eval sexp
         modify $ H.insert x val
         return Void

    -- define-macro
    evalList[Symbol "define-macro", List(Symbol fname : args), expr] =
      do env <- get
         val <- (\argVal -> Macro argVal expr) <$> mapM getSym args
         modify $ H.insert fname val
         return Void

    -- invalid use of keyword, throw a diagnostic
    evalList (Symbol sym : _) | elem sym keywords = invalidSpecialForm sym

    -- application
    evalList (fexpr:args) = eval fexpr >>= aux where
      -- Macro expansion
      aux (Macro fmls body) | length fmls == length args =
        do env <- get
           mapM(\(f,a) -> modify $ H.insert f a) $ zip fmls args
           blob <- eval body
           put env
           ret <- eval blob
           return ret

      -- Function application evaluate arguments, and feed `f` along with the evaluated
      -- arguments to `apply`
      aux f =
        do mArgs <- mapM eval args
           ret <- apply f mArgs
           return ret

eval val = throwError $ InvalidExpression val

-- Function application
apply :: Val -> [Val] -> EvalState Val
-- Function
apply (Func fmls body cenv) args | length fmls == length args =
  do env <- get
     put $ H.union cenv env
     mapM (\(f,a) -> modify $ H.insert f a) $ zip fmls args
     ret <- eval body
     put env
     return ret

-- Primitive
-- Since a primitive function has type `[Val] -> EvalState Val`, all you
-- need is to apply it to arguments
apply (PrimFunc p) args = p args
-- Other values are not applicable
apply f args = throwError $ CannotApply f args
