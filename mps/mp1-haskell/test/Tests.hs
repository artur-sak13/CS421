--- Getting Started
--- ===============

--- Testing Your Code
--- -----------------

module Tests where

import Main hiding (main)

allTests :: [([Bool], String)]
allTests = [ (tests_mytake, "mytake")
           , (tests_mydrop, "mydrop")
           , (tests_rev, "rev")
           , (tests_app, "app")
           , (tests_inclist, "inclist")
           , (tests_sumlist, "sumlist")
           , (tests_myzip, "myzip")
           , (tests_addpairs, "addpairs")
           , (tests_ones, "ones")
           , (tests_nats, "nats")
           , (tests_fib, "fib")
           , (tests_add, "add")
           , (tests_union, "union")
           , (tests_intersect, "intersect")
           , (tests_powerset, "powerset")
           , (tests_inclist', "inclist")
           , (tests_sumlist', "sumlist")
           , (tests_list2cons, "list2cons")
           , (tests_cons2list, "cons2list")
           , (tests_eval, "eval")
           , (tests_list2cons', "list2cons")
           , (tests_sumTree, "sumTree")
           , (tests_liftIntOp, "liftIntOp")
           ]


--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

tests_mytake :: [Bool]
tests_mytake =          [ mytake 4 [2,4,56] == [2,4,56]
                        , mytake 3 [] == ([] :: [()])
                        , mytake 1 ["hello", "world"] == ["hello"]
                        , mytake (-3) [1,2,3] == ([] :: [Integer])
                        , mytake 2 [True, False] == [True,False]
                        , mytake 0 [] == ([] :: [()])
                        , mytake 0 [5.1, 1.4] == ([] :: [Double])
                        ]

--- ### mydrop

tests_mydrop :: [Bool]
tests_mydrop =          [ mydrop 3 [2,4,56,7] == [7]
                        , mydrop 3 [] == ([] :: [()])
                        , mydrop 1 ["hello", "world"] == ["world"]
                        , mydrop (-3) [1,2,3] == [1,2,3]
                        , mydrop 0 [True, False] == [True, False]
                        , mydrop 0 [] == ([] :: [()])
                        , mydrop 2 [5.1, 1.4] == ([] :: [Double])
                        , mydrop 3 [5.1, 1.4] == ([] :: [Double])
                        ]

--- ### rev

tests_rev :: [Bool]
tests_rev =             [ rev [1,2,3] == [3,2,1]
                        , rev [] == ([] :: [()])
                        , rev ["hello", "world"] == ["world","hello"]
                        , rev [True, False, True] == [True,False,True]
                        ]

--- ### app

tests_app :: [Bool]
tests_app =             [ app [] [1,2,3] == [1,2,3]
                        , app [1,2,3] [] == [1,2,3]
                        , app [4,5] [1,2,3] == [4,5,1,2,3]
                        , app ["hello", "world"] ["and", "goodbye"] == ["hello","world","and","goodbye"]
                        , app "hello" "world" == "helloworld"
                        , app [] [] == ([] :: [()])
                        , app [[True, False], [False, True]] [[True, False, False]] == [[True,False],[False,True],[True,False,False]]
                        ]

--- ### inclist

tests_inclist :: [Bool]
tests_inclist =         [ inclist [1,2,3,4] == [2,3,4,5]
                        , inclist [-2,4,5,1] == [-1,5,6,2]
                        , inclist [] == ([] :: [Integer])
                        , inclist [2.3, 4.5, 7.6] == [3.3,5.5,8.6]
                        , inclist [1000000000000000000000000000000] == [1000000000000000000000000000001]
                        ]

--- ### sumlist

tests_sumlist :: [Bool]
tests_sumlist =         [ sumlist [] == 0
                        , sumlist [1,2,3] == 6
                        , sumlist [-3,2,5] == 4
                        , sumlist [3.3,2.8,-1.2] == 4.8999999999999995
                        , sumlist [1000000000000000000000000000000, -1000000000000000000000000000000] == 0
                        ]

--- ### myzip

tests_myzip :: [Bool]
tests_myzip =           [ myzip [1,2,3] [] == ([] :: [(Integer, ())])
                        , myzip [] [1,2,3] == ([] :: [((), Integer)])
                        , myzip [1,2,3] ["hello", "world"] == [(1,"hello"), (2,"world")]
                        , myzip [] [] == ([] :: [((), ())])
                        , myzip [True, False] ['t', 'f'] == [(True,'t'),(False,'f')]
                        , myzip [1,2,3] [1,2,3] == [(1,1),(2,2),(3,3)]
                        ]

--- ### addpairs

tests_addpairs :: [Bool]
tests_addpairs =        [ addpairs [1,2,3] [] == ([] :: [Integer])
                        , addpairs [1,2,3] [4,5,6] == [5,7,9]
                        , addpairs [1.2,3.4] [-1.2,8.9,7.6] == [0,12.3]
                        , addpairs [] [4,5,6] == ([] :: [Integer])
                        , addpairs [1000000000000000000000000000000] [-1000000000000000000000000000000] == [0]
                        ]

--- ### ones

tests_ones :: [Bool]
tests_ones =            [ take 15 ones == [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
                        , take 0 ones == ([] :: [Integer])
                        , take 1 ones == [1]
                        ]

--- ### nats

tests_nats :: [Bool]
tests_nats =            [ take 15 nats == [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14]
                        , take 0 nats == ([] :: [Integer])
                        , take 1 nats == [0]
                        ]

--- ### fib

tests_fib :: [Bool]
tests_fib =             [ take 10 fib == [0,1,1,2,3,5,8,13,21,34]
                        , take 0 fib == ([] :: [Integer])
                        , take 1 fib == [0]
                        , take 2 fib == [0,1]
                        ]

--- Set Theory
--- ----------

--- ### add

tests_add :: [Bool]
tests_add =             [ add 3 [] == [3]
                        , add 3 [1,2] == [1,2,3]
                        , add 3 [1,3,5] == [1,3,5]
                        , add 3 [1,5,8,9] == [1,3,5,8,9]
                        , add "hello" ["goodbye", "world"] == ["goodbye","hello","world"]
                        , add True [False] == [False,True]
                        ]

--- ### union

tests_union :: [Bool]
tests_union =           [ union [] [] == ([] :: [()])
                        , union [1,2,3] [] == [1,2,3]
                        , union [] [1,2,3] == [1,2,3]
                        , union [1,2,3] [1,2,3] == [1,2,3]
                        , union ["goodbye", "world"] ["humans", "smell"] == ["goodbye", "humans", "smell", "world"]
                        , union [1,2,3] [3,4,5] == [1,2,3,4,5]
                        ]

--- ### intersect

tests_intersect :: [Bool]
tests_intersect =       [ intersect [] [1,2,3] == ([] :: [Integer])
                        , intersect [1,2,3] [] == ([] :: [Integer])
                        , intersect [1,2,3] [3,4,45,89] == [3]
                        , intersect ["cruel", "hello", "world"] ["good", "hello", "world"] == ["hello", "world"]
                        , intersect [1,2,3] [1,2,3] == [1,2,3]
                        , intersect [] [] == ([] :: [()])
                        ]

--- ### powerset

tests_powerset :: [Bool]
tests_powerset =        [ powerset [] == ([[]] :: [[()]])
                        , powerset [1,2] == [[],[1],[1,2],[2]]
                        , powerset ["goodbye", "hello", "world"] == [[],["goodbye"],["goodbye","hello"],["goodbye","hello","world"],["goodbye","world"],["hello"],["hello","world"],["world"]]
                        , powerset [[]] == ([[],[[]]] :: [[[()]]])
                        ]

--- Higher Order Functions
--- ----------------------

--- ### inclist'

tests_inclist' :: [Bool]
tests_inclist' =        [ inclist' [1,2,3,4] == [2,3,4,5]
                        , inclist' [-2,4,5,1] == [-1,5,6,2]
                        , inclist' [] == ([] :: [Integer])
                        , inclist' [2.3, 4.5, 7.6] == [3.3,5.5,8.6]
                        , inclist' [1000000000000000000000000000000] == [1000000000000000000000000000001]
                        ]

--- ### sumlist'

tests_sumlist' :: [Bool]
tests_sumlist' =        [ sumlist' [] == 0
                        , sumlist' [1,2,3] == 6
                        , sumlist' [-3,2,5] == 4
                        , sumlist' [3.3,2.8,-1.2] == 4.8999999999999995
                        , sumlist' [1000000000000000000000000000000, -1000000000000000000000000000000] == 0
                        ]

--- Algebraic Data Types
--- --------------------

--- ### list2cons

tests_list2cons :: [Bool]
tests_list2cons =       [ show (list2cons ([] :: [Integer])) == show (Nil :: List Integer)
                        , show (list2cons [3,2,5]) == show (Cons 3 (Cons 2 (Cons 5 Nil)))
                        , show (list2cons ["hello", "world"]) == show (Cons "hello" (Cons "world" Nil))
                        , show (list2cons "hello") == show (Cons 'h' (Cons 'e' (Cons 'l' (Cons 'l' (Cons 'o' Nil)))))
                        , show (list2cons [[1,2,3], [4,5,6], [7,8,9]]) == show (Cons [1,2,3] (Cons [4,5,6] (Cons [7,8,9] Nil)))
                        , show (list2cons [Nil, Cons 5 Nil, Cons 2 (Cons 3 Nil)]) == show (Cons Nil (Cons (Cons 5 Nil) (Cons (Cons 2 (Cons 3 Nil)) Nil)))
                        ]

--- ### cons2list

tests_cons2list :: [Bool]
tests_cons2list =       [ show (cons2list (Nil :: List Integer)) == show ([] :: [Integer])
                        , show (cons2list (Cons 3 (Cons 4 Nil))) == show ([3,4])
                        , show (cons2list (Cons "goodbye" (Cons "world" Nil))) == show (["goodbye", "world"])
                        , show (cons2list (Cons Nil (Cons (Cons 2 Nil) (Cons (Cons 4 (Cons 8 Nil)) Nil)))) == show ([Nil,Cons 2 Nil,Cons 4 (Cons 8 Nil)])
                        , show (cons2list (Cons [] (Cons [1,2] (Cons [1,2,3,4] Nil)))) == show ([[],[1,2],[1,2,3,4]])
                        ]

--- ### eval

tests_eval :: [Bool]
tests_eval =            [ show (eval (IntExp 3)) == show (3)
                        , show (eval (PlusExp [])) == show (0)
                        , show (eval (MultExp [])) == show (1)
                        , show (eval (PlusExp [MultExp [IntExp 3, IntExp 5], PlusExp [IntExp 3], IntExp 5])) == show (23)
                        , show (eval (MultExp [IntExp 3, IntExp 45, IntExp (-2), PlusExp [IntExp 2, IntExp 5]])) == show (-1890)
                        , show (eval (PlusExp [IntExp 3, IntExp 45, IntExp (-2), MultExp [IntExp 2, IntExp 5]])) == show (56)
                        ]

--- ### list2cons'

tests_list2cons' :: [Bool]
tests_list2cons' =      [ show (list2cons' ([] :: [Integer])) == show (Nil :: List Integer)
                        , show (list2cons' [3,2,5]) == show (Cons 3 (Cons 2 (Cons 5 Nil)))
                        , show (list2cons' ["hello", "world"]) == show (Cons "hello" (Cons "world" Nil))
                        , show (list2cons' "hello") == show (Cons 'h' (Cons 'e' (Cons 'l' (Cons 'l' (Cons 'o' Nil)))))
                        , show (list2cons' [[1,2,3], [4,5,6], [7,8,9]]) == show (Cons [1,2,3] (Cons [4,5,6] (Cons [7,8,9] Nil)))
                        , show (list2cons' [Nil, Cons 5 Nil, Cons 2 (Cons 3 Nil)]) == show (Cons Nil (Cons (Cons 5 Nil) (Cons (Cons 2 (Cons 3 Nil)) Nil)))
                        ]

--- ### sumTree

tests_sumTree :: [Bool]
tests_sumTree =         [ show (sumTree Leaf) == show (0)
                        , show (sumTree (Node 3 Leaf (Node 5 (Node 8 Leaf Leaf) Leaf))) == show (16)
                        , show (sumTree (Node (-4) Leaf Leaf)) == show (-4)
                        , show (sumTree (Node 1.2 (Node 3.5 Leaf Leaf) (Node (-4.6) Leaf Leaf))) == show (0.10000000000000053)
                        ]

--- ### liftIntOp

tests_liftIntOp :: [Bool]
tests_liftIntOp =       [ show (liftIntOp (+) (IntVal 3) (IntVal 4)) == show (IntVal 7)
                        , show (liftIntOp (*) (IntVal 2) (IntVal (-5))) == show (IntVal (-10))
                        , show (liftIntOp (+) (BoolVal True) (IntVal 3)) == show (ExnVal "not an IntVal!")
                        , show (liftIntOp (+) (IntVal 5) (StrVal "hello")) == show (ExnVal "not an IntVal!")
                        , show (liftIntOp (+) (StrVal "hello") (ExnVal "not an IntVal!")) == show (ExnVal "not an IntVal!")
                        , show (liftIntOp (^) (IntVal 5) (IntVal 3)) == show (IntVal 125)
                        , show (liftIntOp (*) (liftIntOp (+) (IntVal 3) (IntVal 4)) (IntVal 3)) == show (IntVal 21)
                        , show (liftIntOp (*) (liftIntOp (+) (BoolVal False) (IntVal 4)) (IntVal 3)) == show (ExnVal "not an IntVal!")
                        ]
