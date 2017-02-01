module Main where

import Text.Parsec

-- The Types

data Val = IntVal Integer
   deriving (Show,Eq)

data Exp = IntExp Integer
   deriving (Show,Eq)

type Env = [(String,Exp)]

-- The Parser
--
-- Treat this as a "black box"; you are not expected to understand it.  Yet.


-- Lexicals

int = do digits <- many1 digit <?> "An integer"
         spaces
         return (read digits :: Integer)

-- Expressions

intExp = do i <- int
            return $ IntExp i

-- Evaluator

eval :: Exp -> Env -> Val
eval (IntExp i) _ = IntVal i

-- REPL

repl :: Env -> IO ()
repl env =
  do putStr "interpreter> "
     input <- getLine
     case parse intExp "stdin" input of
       Right exp -> let result = eval exp env in
                    do putStrLn (show result)
                       repl env
       Left msg -> do putStrLn $ show msg
                      repl env

-- Main
--

main :: IO ()
main = repl []
