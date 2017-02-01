{-# LANGUAGE FlexibleContexts #-}
module Main where

import Text.Parsec

-- The Types

data Val = IntVal Integer
   deriving (Show,Eq)

data Exp = IntExp Integer
         | IntOpExp String Exp Exp
   deriving (Show,Eq)

type Env = [(String,Exp)]

-- The Parser
--
-- Treat this as a "black box"; you are not expected to understand it.  Yet.


-- Lexicals

int = do digits <- many1 digit <?> "An integer"
         spaces
         return (read digits :: Integer)

symbol s = do string s
              spaces
              return s

-- Expressions

intExp = do i <- int
            return $ IntExp i

mulOp =    do { symbol "*" ; return $ IntOpExp "*" }
       <|> do { symbol "/" ; return $ IntOpExp "/" }

addOp =    do { symbol "+" ; return $ IntOpExp "+" }
       <|> do { symbol "-" ; return $ IntOpExp "-" }

expr = term `chainl1` addOp
term = factor `chainl1` mulOp
factor = atom
atom = intExp
-- Evaluator

eval :: Exp -> Env -> Val
eval (IntExp i) _ = IntVal i

eval (IntOpExp "+" e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
  in IntVal (getInt v1 + getInt v2)
eval (IntOpExp "*" e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
  in IntVal (getInt v1 * getInt v2)
eval (IntOpExp "-" e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
  in IntVal (getInt v1 - getInt v2)

getInt (IntVal i) = i
getInt _          = 0

-- REPL

repl :: Env -> IO ()
repl env =
  do putStr "interpreter> "
     input <- getLine
     case parse expr "stdin" input of
       Right exp -> let result = eval exp env in
                    do putStrLn (show result)
                       repl env
       Left msg -> do putStrLn $ show msg
                      repl env
-- Main
--

main :: IO ()
main = repl []
