module Spec where

import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Timeout (timeout)
import Control.Exception (try, evaluate, SomeException)

import Tests

main :: IO ()
main = do putStrLn ""
          putStrLn "Running Tests"
          putStrLn "============="
          results <- runTests
          mapM_ (putStrLn . showTR) results
          putStrLn ""
          let score = scoreMP results
          putStrLn $ "Score: " ++ show score ++ " / 100"
          if score >= 100
            then do putStrLn "All tests passed."
                    exitWith ExitSuccess
            else do putStrLn "Some tests failed."
                    exitWith $ ExitFailure 1

data Score = Pass
           | Fail [Bool]
           | Exception
           | Timeout
           deriving Show

type TestResult = (Score, String)

showTR :: TestResult -> String
showTR (score, name) = show score ++ ": " ++ name

scoreMP :: [TestResult] -> Int
scoreMP tests = let passed = length [t | t@(Pass, _) <- tests]
                in  (passed * 100) `div` (length tests)

runTests :: IO [TestResult]
runTests = let handleTest (test, name) = do score <- runTest test
                                            return (score, name)
           in  mapM handleTest allTests

runTest :: [Bool] -> IO Score
runTest tests
    = let test  = case and tests of
                   True  -> Pass
                   False -> Fail tests
          onExn = const Exception :: SomeException -> Score
          tryT  = try (evaluate test) >>= return . either onExn id
      in  timeout 1000000 tryT >>= return . maybe Timeout id
