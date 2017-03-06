--- Getting Started
--- ===============

--- Testing Your Code
--- -----------------

module Tests where

import Main hiding (main)

asOutput :: (String, [String]) -> Bool
asOutput (input, output) = let (_, _, _, out) = eval (words input) initialForthState
                               removeEmpty = Prelude.filter (not . Prelude.null)
                           in  removeEmpty out == removeEmpty output

asOutputs :: [(String, [String])] -> [Bool]
asOutputs = Prelude.map asOutput

allTests :: [([Bool], String)]
allTests = [ (tests_splitIf, "`splitIf` parser")
           , (tests_stackShow, "show command `.S`")
           , (tests_dup, "stack manip command `dup`")
           , (tests_drop, "stack manip command `drop`")
           , (tests_swap, "stack manip command `swap`")
           , (tests_rot, "stack manip command `rot`")
           , (tests_defn, "definition command `: ... ;`")
           , (tests_condition, "if command `if ... [else ...] then`")
           , (tests_loop, "loop command `begin ... again`")
           ]


--- Problems
--- ========

--- The Parser
--- ----------

--- ### Input Parser `splitIf`

tests_splitIf :: [Bool]
tests_splitIf =         [ splitIf (words ". 5 7 else 3 4 < if . else 5 then then 5 .")
                          == ( words ". 5 7"
                             , words "3 4 < if . else 5 then"
                             , words "5 ."
                             )
                        , splitIf (words ". 5 7 3 4 < if . else 5 then then 5 .")
                          == ( words ". 5 7 3 4 < if . else 5 then"
                             , []
                             , words "5 ."
                             )
                        , splitIf (words ". 5 if : 3 4 ; then else 0 if 3 else 0 if 4 then then 5 6 then . .S")
                          == ( words ". 5 if : 3 4 ; then"
                             , words "0 if 3 else 0 if 4 then then 5 6"
                             , words ". .S"
                             )
                        , splitIf (words "begin 3 3 dup . if 4 - dup else 5 - dup then again then 5 5 .")
                          == ( words "begin 3 3 dup . if 4 - dup else 5 - dup then again"
                             , words ""
                             , words "5 5 ."
                             )
                        ]

--- The Evaluator
--- -------------

--- ### Printing the Stack

tests_stackShow :: [Bool]
tests_stackShow = asOutputs [ ("2 3 4 .S", ["2 3 4"])
                            , ("3 4 2 5 .S 2 4 6 .S", ["3 4 2 5", "3 4 2 5 2 4 6"])
                            , ("9 3 2 . 2 4 .S 2 5 .S 2 .", ["2", "9 3 2 4", "9 3 2 4 2 5", "2"])
                            ]

--- ### Stack Manipulations

tests_dup :: [Bool]
tests_dup =       asOutputs [ ("2 3 4 dup .S", ["2 3 4 4"])
                            , ("2 3 4 . .S dup .S", ["4", "2 3", "2 3 3"])
                            ]

tests_drop :: [Bool]
tests_drop =      asOutputs [ ("2 3 4 drop .S", ["2 3"])
                            , ("2 3 4 . .S drop .S", ["4", "2 3", "2"])
                            , ("6 23 drop . 5 5 drop . .S", ["6", "5", ""])
                            ]

tests_swap :: [Bool]
tests_swap =      asOutputs [ ("2 3 4 swap .S", ["2 4 3"])
                            , ("2 3 4 . .S swap 4 5 swap .S", ["4", "2 3", "3 2 5 4"])
                            , ("6 23 drop . 5 5 6 2 swap . swap .S", ["6", "6", "5 2 5"])
                            ]
tests_rot :: [Bool]
tests_rot =       asOutputs [ ("2 3 4 rot .S", ["3 4 2"])
                            , ("2 3 4 . .S 5 rot 4 5 swap .S rot .", ["4", "2 3", "3 5 2 5 4", "2"])
                            , ("6 23 drop . 5 5 6 2 rot . swap rot .S", ["6", "5", "2 6 5"])
                            ]

--- ### User definitions

tests_defn :: [Bool]
tests_defn =      asOutputs [ (": square dup * ; 4 square .", ["16"])
                            , (": dupRot dup rot ; 4 4 5 dupRot .S", ["4 5 5 4"])
                            , (": print dup . ; 4 4 5 print . print .S", ["5", "5", "4", "4 4"])
                            , (": . dup ; 5 5 . .S", ["5", "5"])
                            , (": print dup . : print . ; ; 5 6 7 print print print print", ["7", "7", "6", "5"])
                            ]

--- ### Conditionals

tests_condition :: [Bool]
tests_condition = asOutputs [ ("3 4 < if 10 else 20 then .", ["10"])
                            , ("3 4 > if 10 else 20 then .", ["20"])
                            , ("3 4 > if 10 then .S", [])
                            , ("3 4 < if 10 then .S", ["10"])
                            , ("3 4 < if 3 4 < if 10 else 20 then else 30 then .", ["10"])
                            , ("3 4 < if 3 4 > if 10 else 20 then else 30 then .", ["20"])
                            , ("3 4 > if 3 4 < if 10 else 20 then else 30 then .", ["30"])
                            ]

--- ### Loops

tests_loop :: [Bool]
tests_loop =      asOutputs [ ("4 begin dup . dup 0 > if 1 - else exit then again"
                              , ["4", "3", "2", "1", "0"]
                              )
                            , (factFunc ++ " 4 fact . 5 fact .", ["24", "120"])
                            , ( factFunc ++ " " ++ seqFunc
                                ++ " 7 12 seq fact . fact . fact . fact . fact ."
                              , ["479001600", "39916800", "3628800", "362880", "40320"]
                              )
                            ]
    where
        exitOnZero = ": exitOnZero dup if else drop exit then ;"
        factFunc   = exitOnZero ++ " : fact dup begin 1 - exitOnZero dup rot * swap again ;"
        exitIfSame = ": exitIfSame dup rot dup rot - swap rot rot if else exit then ;"
        seqFunc    = exitIfSame ++ " : seq begin exitIfSame swap dup 1 + rot again drop ;"
