--- Getting Started
--- ===============

--- Testing Your Code
--- -----------------

module Tests where

import Main hiding (main)
import Data.HashMap.Strict as H

allTests :: [([Bool], String)]
allTests = [ (tests_lifting, "Lifting functions")
           , (tests_constexps, "ConstExps")
           , (tests_intopexp, "IntOpExp")
           , (tests_boolcompopexps, "BoolAndCompOpExp")
           , (tests_ifexp, "IfExp")
           , (tests_varexp, "VarExp")
           , (tests_funexp, "FunExp")
           , (tests_appexp1, "AppExp1")
           , (tests_appexp2, "AppExp2")
           , (tests_letexp1, "LetExp1")
           , (tests_letexp2, "LetExp2")
           , (tests_letexp3, "LetExp3")
           , (tests_seqstmt, "SeqStmt")
           , (tests_ifstmt, "IfStmt")
           , (tests_setstmt, "SetStmt")
           , (tests_procedurestmt, "ProcedureStmt")
           , (tests_callstmt1, "CallStmt1")
           , (tests_callstmt2, "CallStmt2")
           ]


--- Problems
--- ========

--- Lifting Functions
--- -----------------

tests_lifting :: [Bool]
tests_lifting =         [ liftBoolOp (&&) (BoolVal True) (BoolVal False) == BoolVal False
                        , liftBoolOp (||) (BoolVal True) (BoolVal False) == BoolVal True
                        , liftBoolOp (||) (IntVal 1) (IntVal 0) == ExnVal "Cannot lift"
                        , liftBoolOp (||) (BoolVal True) (IntVal 0) == ExnVal "Cannot lift"
                        , liftCompOp (<=) (IntVal 5) (IntVal 4) == BoolVal False
                        , liftCompOp (>) (IntVal 5) (IntVal 4) == BoolVal True
                        , liftCompOp (==) (BoolVal True) (BoolVal True) == ExnVal "Cannot lift"
                        , liftCompOp (==) (IntVal 5) (BoolVal True) == ExnVal "Cannot lift"
                        ]

--- Eval
--- ----

--- ### Constants

tests_constexps :: [Bool]
tests_constexps =       [ eval (IntExp 5) H.empty == IntVal 5
                        , eval (IntExp (-5)) H.empty == IntVal (-5)
                        , eval (BoolExp True) H.empty == BoolVal True
                        , eval (BoolExp False) H.empty == BoolVal False
                        ]

--- ### Variables

testenv1 :: HashMap [Char] Val
testenv1 = H.fromList [ ("x", IntVal 3)
                      , ("y", BoolVal True)
                      , ("z", ExnVal "ZzZz")
                      , ("f", CloVal [] (IntExp 0) H.empty)
                      ]

tests_varexp :: [Bool]
tests_varexp =          [ eval (VarExp "x") H.empty == ExnVal "No match in env"
                        , eval (VarExp "x") testenv1 == IntVal 3
                        , eval (VarExp "y") testenv1 == BoolVal True
                        , eval (VarExp "z") testenv1 == ExnVal "ZzZz"
                        , eval (VarExp "f") testenv1 == CloVal [] (IntExp 0) H.empty
                        , eval (VarExp "a") testenv1 == ExnVal "No match in env"
                        ]

--- ### Arithmetic

tests_intopexp :: [Bool]
tests_intopexp =        [ eval (IntOpExp "+" (IntExp 5) (IntExp 4)) H.empty == IntVal 9
                        , eval (IntOpExp "-" (IntOpExp "*" (IntExp 3) (IntExp 10)) (IntExp 7)) H.empty == IntVal 23
                        , eval (IntOpExp "/" (IntExp 6) (IntExp 2)) H.empty == IntVal 3
                        , eval (IntOpExp "/" (IntExp 6) (IntExp 0)) H.empty == ExnVal "Division by 0"
                        , eval (IntOpExp "+" (IntExp 6) (IntOpExp "/" (IntExp 4) (IntExp 0))) H.empty == ExnVal "Cannot lift"
                        ]

--- ### Boolean and Comparison Operators

tests_boolcompopexps :: [Bool]
tests_boolcompopexps =  [ eval (BoolOpExp "and" (BoolExp True) (BoolExp False)) H.empty == BoolVal False
                        , eval (BoolOpExp "or" (BoolExp True) (BoolExp False)) H.empty == BoolVal True
                        , eval (BoolOpExp "or" (IntExp 3) (BoolExp False)) H.empty == ExnVal "Cannot lift"
                        , eval (CompOpExp "/=" (IntExp 4) (IntExp 6)) H.empty == BoolVal True
                        , eval (CompOpExp "<" (IntExp 6) (IntExp 6)) H.empty == BoolVal False
                        , eval (CompOpExp ">=" (BoolExp True) (IntExp 6)) H.empty == ExnVal "Cannot lift"
                        ]

--- ### If Expressions

tests_ifexp :: [Bool]
tests_ifexp =           [ eval (IfExp (BoolExp True) (IntExp 5) (IntExp 10)) H.empty == IntVal 5
                        , eval (IfExp (BoolExp False) (IntExp 5) (IntExp 10)) H.empty == IntVal 10
                        , eval (IfExp (BoolOpExp "or" (BoolExp True) (BoolExp False)) (IntExp 5) (IntExp 10)) H.empty == IntVal 5
                        , eval (IfExp (BoolExp True) (IntOpExp "+" (IntExp 5) (IntExp 4)) (IntExp 10)) H.empty == IntVal 9
                        , eval (IfExp (BoolExp False) (IntExp 5) (IntOpExp "+" (IntExp 5) (IntExp 4))) H.empty == IntVal 9
                        , eval (IfExp (BoolExp True) (IntExp 5) (IntOpExp "/" (IntExp 5) (IntExp 0))) H.empty == IntVal 5
                        , eval (IfExp (BoolExp False) (IntOpExp "/" (IntExp 5) (IntExp 0)) (IntExp 10)) H.empty == IntVal 10
                        , eval (IfExp (IntExp 1) (IntExp 5) (IntExp 10)) H.empty == ExnVal "Condition is not a Bool"
                        , eval (IfExp (IntOpExp "/" (IntExp 5) (IntExp 0)) (IntExp 5) (IntExp 10)) H.empty == ExnVal "Condition is not a Bool"
                        ]

--- ### Functions and Function Application

tests_funexp :: [Bool]
tests_funexp =          [ eval (FunExp [] (IntExp 5)) H.empty == CloVal [] (IntExp 5) H.empty
                        , eval (FunExp [] (IntOpExp "+" (IntExp 5) (IntExp (-5)))) H.empty == CloVal [] (IntOpExp "+" (IntExp 5) (IntExp (-5))) H.empty
                        , eval (FunExp ["a", "b", "c"] (IntExp 5)) H.empty == CloVal ["a", "b", "c"] (IntExp 5) H.empty
                        , eval (FunExp [] (IntExp 5)) testenv1 == CloVal [] (IntExp 5) testenv1
                        , eval (FunExp ["a"] (IntOpExp "/" (VarExp "a") (IntExp 0))) testenv1 == CloVal ["a"] (IntOpExp "/" (VarExp "a") (IntExp 0)) testenv1
                        ]

tests_appexp1 :: [Bool]
tests_appexp1 =         [ eval (AppExp (FunExp [] (IntExp 5)) []) H.empty == IntVal 5
                        , eval (AppExp (FunExp [] (BoolOpExp "and" (BoolExp True) (BoolExp True))) []) H.empty == BoolVal True
                        , eval (AppExp (FunExp [] (VarExp "x")) []) H.empty == ExnVal "No match in env"
                        , eval (AppExp (FunExp [] (VarExp "x")) []) testenv1 == IntVal 3
                        , eval (AppExp (FunExp ["a"] (VarExp "a")) [IntExp 5]) H.empty == IntVal 5
                        , eval (AppExp (FunExp ["a"] (VarExp "a")) [IntOpExp "+" (IntExp 5) (IntExp 4)]) H.empty == IntVal 9
                        , eval (AppExp (IntExp 6) []) H.empty == ExnVal "Apply to non-closure"
                        , eval (AppExp (VarExp "f") []) H.empty == ExnVal "Apply to non-closure"
                        , eval (AppExp (VarExp "f") []) testenv1 == IntVal 0
                        ]

testenv2 :: HashMap [Char] Val
testenv2 = H.fromList [ ("f", CloVal ["a"] (IntOpExp "*" (VarExp "a") (VarExp "x")) testenv1)
                      , ("g", CloVal ["y"] (BoolOpExp "and" (VarExp "y") (CompOpExp "==" (VarExp "x") (IntExp 3))) testenv1)
                      , ("h", CloVal ["x"] (IntOpExp "*" (VarExp "x") (VarExp "x")) testenv1)
                      , ("k", CloVal ["f", "x"] (IntOpExp "*" (VarExp "x") (AppExp (VarExp "f") [VarExp "x"])) testenv1)
                      , ("x", ExnVal "Oops!")
                      ]

tests_appexp2 :: [Bool]
tests_appexp2 =         [ eval (AppExp (VarExp "f") [IntExp 3]) testenv2 == IntVal 9
                        , eval (AppExp (VarExp "g") [BoolExp True]) testenv2 == BoolVal True
                        , eval (AppExp (VarExp "g") [BoolExp False]) testenv2 == BoolVal False
                        , eval (AppExp (VarExp "k") [VarExp "h", IntExp 2]) testenv2 == IntVal 8
                        , eval (AppExp (VarExp "k") [VarExp "h", IntExp 3]) testenv2 == IntVal 27
                        , eval (AppExp (VarExp "k") [VarExp "f", IntExp 2]) testenv2 == IntVal 12
                        , eval (AppExp (VarExp "k") [VarExp "g", IntExp 2]) testenv2 == ExnVal "Cannot lift"
                        , eval (AppExp (VarExp "k") [VarExp "x", IntExp 2]) testenv2 == ExnVal "Cannot lift"
                        ]

--- ### Let Expressions

tests_letexp1 :: [Bool]
tests_letexp1 =         [ eval (LetExp [] (IntExp 5)) H.empty == IntVal 5
                        , eval (LetExp [] (IntOpExp "+" (IntExp 5) (IntExp 4))) H.empty == IntVal 9
                        , eval (LetExp [] (VarExp "x")) H.empty == ExnVal "No match in env"
                        , eval (LetExp [] (VarExp "x")) testenv1 == IntVal 3
                        , eval (LetExp [("x", IntExp 5)] (VarExp "x")) H.empty == IntVal 5
                        , eval (LetExp [("x", IntExp 5)] (VarExp "x")) testenv1 == IntVal 5
                        , eval (LetExp [("x", IntOpExp "+" (IntExp 5) (IntExp 4))] (VarExp "x")) H.empty == IntVal 9
                        ]

tests_letexp2 :: [Bool]
tests_letexp2 =         [ eval (LetExp [("x", IntExp 5), ("y", IntExp 4)] (IntOpExp "+" (VarExp "x") (VarExp "y"))) H.empty == IntVal 9
                        , eval (LetExp [("x", IntExp 5), ("y", IntExp 4)] (IntOpExp "+" (VarExp "x") (VarExp "y"))) testenv1 == IntVal 9
                        , eval (LetExp [("z", IntExp 5), ("y", IntExp 4)] (IntOpExp "+" (VarExp "x") (VarExp "y"))) testenv1 == IntVal 7
                        , eval (LetExp [("f", AppExp (VarExp "f") [])] (VarExp "f")) H.empty == ExnVal "Apply to non-closure"
                        , eval (LetExp [("f", AppExp (VarExp "f") [])] (VarExp "f")) testenv1 == IntVal 0
                        , eval (LetExp [("x", IntExp 5)] (LetExp [("x", IntExp 6)] (VarExp "x"))) H.empty == IntVal 6
                        , eval (LetExp [("x", IntExp 5)] (LetExp [("x", IntOpExp "*" (IntExp 2) (VarExp "x"))] (VarExp "x"))) H.empty == IntVal 10
                        ]

tests_letexp3 :: [Bool]
tests_letexp3 =         [ eval (LetExp [("x", IntExp 5)] (LetExp [("y", VarExp "x"), ("x", IntExp 6)] (VarExp "y"))) H.empty == IntVal 5
                        , eval (LetExp [("x", IntExp 5)] (LetExp [("x", IntExp 6), ("y", VarExp "x")] (VarExp "y"))) H.empty == IntVal 5
                        , eval (LetExp [("x", IntExp 5)] (LetExp [("y", VarExp "x"), ("x", IntExp 6)] (IntOpExp "+" (VarExp "x") (VarExp "y")))) H.empty == IntVal 11
                        , eval (LetExp [("x", IntExp 5)] (LetExp [("x", IntExp 6), ("y", VarExp "x")] (IntOpExp "+" (VarExp "x") (VarExp "y")))) H.empty == IntVal 11
                        , eval (LetExp [("x", LetExp [("x", IntExp 5)] (IntOpExp "*" (VarExp "x") (IntExp 2)))] (VarExp "x")) H.empty == IntVal 10
                        , eval (LetExp [("x", FunExp ["f", "x"] (AppExp (VarExp "f") [VarExp "x"]))] (AppExp (VarExp "x") [FunExp ["x"] (IntOpExp "*" (VarExp "x") (IntExp 2)), IntExp 17])) H.empty == IntVal 34
                        ]

--- Statements
--- ----------

--- ### Set Statements

tests_setstmt :: [Bool]
tests_setstmt =         [ exec (SetStmt "x" (IntExp 5)) H.empty H.empty == ("", H.empty, H.fromList [("x", IntVal 5)])
                        , exec (SetStmt "x" (IntExp 5)) H.empty (H.fromList [("x", IntVal 6)]) == ("", H.empty, H.fromList [("x", IntVal 5)])
                        , exec (SetStmt "x" (IntOpExp "+" (IntExp 5) (IntExp 4))) H.empty H.empty == ("", H.empty, H.fromList [("x", IntVal 9)])
                        , exec (SeqStmt [(SetStmt "x" (IntExp 5)), PrintStmt (VarExp "x")] ) H.empty H.empty == ("5", H.empty, H.fromList [("x", IntVal 5)])
                        , exec (SeqStmt [(SetStmt "f" (FunExp ["x"] (IntOpExp "*" (VarExp "x") (IntExp 2)))), PrintStmt (AppExp (VarExp "f") [IntExp 17])]) H.empty H.empty == ("34", H.empty, H.fromList [("f", CloVal ["x"] (IntOpExp "*" (VarExp "x") (IntExp 2)) H.empty)])
                        , exec (SeqStmt [(SetStmt "x" (FunExp ["x"] (IntOpExp "*" (VarExp "x") (IntExp 2)))), PrintStmt (AppExp (VarExp "x") [IntExp 17])]) H.empty H.empty == ("34", H.empty, H.fromList [("x", CloVal ["x"] (IntOpExp "*" (VarExp "x") (IntExp 2)) H.empty)])
                        ]

--- ### Sequencing

tests_seqstmt :: [Bool]
tests_seqstmt =         [ exec (SeqStmt [PrintStmt (IntExp 5)]) H.empty H.empty == ("5", H.empty, H.empty)
                        , exec (SeqStmt [PrintStmt (IntExp 4), PrintStmt (IntExp 2)]) H.empty H.empty == ("42", H.empty, H.empty)
                        , exec (SeqStmt [PrintStmt (VarExp "x"), PrintStmt (IntExp 2)]) H.empty H.empty == ("exn: No match in env2", H.empty, H.empty)
                        ]

--- ### If Statements

tests_ifstmt :: [Bool]
tests_ifstmt =          [ exec (IfStmt (BoolExp True) (PrintStmt (IntExp 5)) (PrintStmt (IntExp 10))) H.empty H.empty == ("5", H.empty, H.empty)
                        , exec (IfStmt (BoolExp False) (PrintStmt (IntExp 5)) (PrintStmt (IntExp 10))) H.empty H.empty == ("10", H.empty, H.empty)
                        , exec (IfStmt (BoolExp True) (SeqStmt [PrintStmt (IntExp 4), PrintStmt (IntExp 2)]) (PrintStmt (IntExp 10))) H.empty H.empty == ("42", H.empty, H.empty)
                        , exec (IfStmt (BoolExp False) (PrintStmt (IntExp 5)) (SeqStmt [PrintStmt (IntExp 4), PrintStmt (IntExp 2)])) H.empty H.empty == ("42", H.empty, H.empty)
                        , exec (IfStmt (IntExp 1) (PrintStmt (IntExp 5)) (PrintStmt (IntExp 10))) H.empty H.empty == ("exn: Condition is not a Bool", H.empty, H.empty)
                        , exec (IfStmt (VarExp "x") (PrintStmt (IntExp 5)) (PrintStmt (IntExp 10))) H.empty H.empty == ("exn: Condition is not a Bool", H.empty, H.empty)
                        , exec (IfStmt (FunExp [] (IntExp 0)) (PrintStmt (IntExp 5)) (PrintStmt (IntExp 10))) H.empty H.empty == ("exn: Condition is not a Bool", H.empty, H.empty)
                        ]

--- ### Procedure and Call Statements

tests_procedurestmt :: [Bool]
tests_procedurestmt =   [ exec (ProcedureStmt "p" [] (PrintStmt (IntExp 5))) H.empty H.empty == ("", H.fromList [("p", ProcedureStmt "p" [] (PrintStmt (IntExp 5)))], H.empty)
                        , exec (ProcedureStmt "p" [] (PrintStmt (IntExp 5))) (H.fromList [("p", ProcedureStmt "p" [] (PrintStmt (IntExp 0)))]) H.empty == ("", H.fromList [("p", ProcedureStmt "p" [] (PrintStmt (IntExp 5)))], H.empty)
                        , exec (ProcedureStmt "p" [] (PrintStmt (VarExp "x"))) H.empty H.empty == ("", H.fromList [("p", ProcedureStmt "p" [] (PrintStmt (VarExp "x")))], H.empty)
                        , exec (ProcedureStmt "p" [] (SetStmt "x" (IntExp 5))) H.empty H.empty == ("", H.fromList [("p", ProcedureStmt "p" [] (SetStmt "x" (IntExp 5)))], H.empty)
                        , exec (ProcedureStmt "p" [] (ProcedureStmt "q" [] (PrintStmt (VarExp "x")))) H.empty H.empty == ("", H.fromList [("p", ProcedureStmt "p" [] (ProcedureStmt "q" [] (PrintStmt (VarExp "x"))))], H.empty)
                        , exec (SeqStmt [SetStmt "x" (IntExp 5), ProcedureStmt "p" [] (PrintStmt (VarExp "x"))]) H.empty H.empty == ("", H.fromList [("p", ProcedureStmt "p" [] (PrintStmt (VarExp "x")))], H.fromList [("x", IntVal 5)])
                        , exec (SeqStmt [SetStmt "p" (IntExp 5), ProcedureStmt "p" [] (PrintStmt (VarExp "p"))]) H.empty H.empty == ("", H.fromList [("p", ProcedureStmt "p" [] (PrintStmt (VarExp "p")))], H.fromList [("p", IntVal 5)])
                        ]

testproc1 :: Stmt
testproc1 = ProcedureStmt "p" [] (PrintStmt (IntExp 5))
testproc2 :: Stmt
testproc2 = ProcedureStmt "p" [] (PrintStmt (VarExp "x"))
testproc3 :: Stmt
testproc3 = ProcedureStmt "p" ["x"] (PrintStmt (VarExp "x"))
testproc4 :: Stmt
testproc4 = ProcedureStmt "p" ["x"] (SetStmt "x" (IntOpExp "+" (VarExp "x") (IntExp 1)))

tests_callstmt1 :: [Bool]
tests_callstmt1 =       [ exec (CallStmt "p" []) (H.fromList [("p", testproc1)]) H.empty == ("5", H.fromList [("p", testproc1)], H.empty)
                        , exec (CallStmt "p" []) (H.fromList [("p", testproc2)]) (H.fromList [("x", IntVal 7)]) == ("7", H.fromList [("p", testproc2)], H.fromList [("x", IntVal 7)])
                        , exec (CallStmt "p" [IntExp 7]) (H.fromList [("p", testproc3)]) H.empty == ("7", H.fromList [("p", testproc3)], H.fromList [("x", IntVal 7)])
                        , exec (CallStmt "p" [IntExp 7]) (H.fromList [("p", testproc4)]) H.empty == ("", H.fromList [("p", testproc4)], H.fromList [("x", IntVal 8)])
                        , exec (CallStmt "p" []) H.empty H.empty == ("Procedure p undefined", H.empty, H.empty)
                        ]

tests_callstmt2 :: [Bool]
tests_callstmt2 =       [ exec (SeqStmt [ProcedureStmt "e" ["x"] (PrintStmt (BoolExp True)), CallStmt "e" [IntExp 23], PrintStmt (VarExp "x")]) H.empty H.empty == ("True23", H.fromList [("e", ProcedureStmt "e" ["x"] (PrintStmt (BoolExp True)))], H.fromList [("x", IntVal 23)])
                        , exec (SeqStmt [SetStmt "y" (IntExp 0), ProcedureStmt "c" [] (IfStmt (CompOpExp "<" (VarExp "y") (IntExp 10)) (SeqStmt [PrintStmt (VarExp "y"), SetStmt "y" (IntOpExp "+" (VarExp "y") (IntExp 1)), CallStmt "c" []]) (PrintStmt (BoolExp True))), CallStmt "c" []]) H.empty H.empty == ("0123456789True", H.fromList [("c", ProcedureStmt "c" [] (IfStmt (CompOpExp "<" (VarExp "y") (IntExp 10)) (SeqStmt [PrintStmt (VarExp "y"), SetStmt "y" (IntOpExp "+" (VarExp "y") (IntExp 1)), CallStmt "c" []]) (PrintStmt (BoolExp True))))], H.fromList [("y", IntVal 10)])
                        , exec (SeqStmt [ProcedureStmt "fog" ["f", "g", "x"] (SetStmt "x" (AppExp (VarExp "f") [(AppExp (VarExp "g") [VarExp "x"])])), CallStmt "fog" [FunExp ["x"] (IntOpExp "*" (VarExp "x") (IntExp 2)), FunExp ["x"] (IntOpExp "+" (VarExp "x") (IntExp 1)), IntExp 6], PrintStmt (VarExp "x")]) H.empty H.empty == ("14", H.fromList [("fog", ProcedureStmt "fog" ["f", "g", "x"] (SetStmt "x" (AppExp (VarExp "f") [(AppExp (VarExp "g") [VarExp "x"])])))], H.fromList [("x", IntVal 14), ("f", CloVal ["x"] (IntOpExp "*" (VarExp "x") (IntExp 2)) H.empty), ("g", CloVal ["x"] (IntOpExp "+" (VarExp "x") (IntExp 1)) H.empty)])
                        ]
