module Main where
import Scheme.Runtime
import Scheme.Core
import Scheme.Parse
import Scheme.Eval

import Prelude hiding (lookup)
import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

--- ### REPL

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: Env -> IO ()
repl env = do
  putStr "scheme> "
  l <- getLine                                        -- Read
  case parse exprP "Expression" l of                  -- Parse
    Left err -> print err                             -- Diagnostics
    Right expr ->
      case runExcept $ runStateT (eval expr) env of   -- Eval
        -- TODO:
        -- Insert line here: If error, print error
        -- Insert line here: If return value is void,
        --                    loop with new env without printing
        -- Insert line here: Otherwise, print and loop with new env
        --
        -- The following line may be removed when you're done implementing
        --  the cases above:
        _ -> print "Error in Main.hs: Finish implementing repl"
  repl env                                            -- Loop with old env

main :: IO ()
main = repl runtime
