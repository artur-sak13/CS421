--- Getting Started
--- ===============

--- Testing Your Code
--- -----------------

module Tests where

import Main hiding (main)
import Scheme.Core
import Scheme.Parse
import Scheme.Eval
import Scheme.Runtime
import Text.ParserCombinators.Parsec (parse)
import Data.HashMap.Strict (empty, fromList)
import Data.List (isInfixOf,intercalate)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

allTests :: [([Bool], String)]
allTests = let prefix str (ts, desc) = (ts, str ++ desc)
            in map (prefix "Runtime: ") tests_runtime ++
               map (prefix "Evaluator: ") tests_evaluator

withRuntime :: [String] -> [String]
withRuntime codes = aux runtime codes
  where aux runtime [] = []
        aux runtime (code:codes) =
          case parse exprP "Expression" code of                   -- Parse
            Left err -> [show err]                          -- Diagnostics
            Right expr ->
              case runExcept $ runStateT (eval expr) runtime of   -- Eval
                Left err -> [show err]                      -- Diagnostics
                Right (val, env) -> show val : aux env codes

makeTests :: String -> [[String]] -> String
makeTests name tests = name ++ " :: [Bool]\n" ++ name ++ " = [ withRuntime " ++ results ++  padding ++ "]\n"
      where padding = replicate (length name + 3) ' '
            results = intercalate (padding ++ ", withRuntime ") $ map (\ test -> show test ++ " == " ++ show (withRuntime test) ++ "\n") tests

--- ([Input], [ParsedAs], Output)
type TestCase  = ([String], [Val], String)
type TestSuite = (String, [TestCase])

boolTests :: (TestCase -> Bool) -> TestSuite -> ([Bool], String)
boolTests run (desc, tcs) = (map run tcs, desc)


--- Problems (Part 1)
--- =================

--- Environment
--- -----------

tests_runtime :: [([Bool], String)]
tests_runtime = [ (tests_arithRuntime,  "Arithmetic Operators")
                , (tests_boolRuntime,   "Boolean Operators")
                , (tests_compRuntime,   "Comparison Operators")
                , (tests_listOps,  "List Operations")
                , (tests_unaryRuntime,  "Unary Operators")
                , (tests_equality,  "= and eq? Operators")
                , (tests_modulo, "modulo")
                , (tests_type_preds, "dynamic types")
                ]

--- ### Arithmetic Operators

tests_arithRuntime :: [Bool]
tests_arithRuntime = [ withRuntime ["(+)"] == ["0"]
                     , withRuntime ["(-)"] == ["0"]
                     , withRuntime ["(*)"] == ["1"]
                     , withRuntime ["(+ 3 4 5)"] == ["12"]
                     , withRuntime ["(- 3 4 5)"] == ["-6"]
                     , withRuntime ["(* 7 8 9)"] == ["504"]
                     ]

--- ### Boolean Operators

tests_boolRuntime :: [Bool]
tests_boolRuntime = [ withRuntime ["(and #t #t #t #t)"] == ["#t"]
                    , withRuntime ["(and #t #t #t #t)"] == ["#t"]
                    , withRuntime ["(and #t #t 'nil)"] == ["#t"]
                    , withRuntime ["(and 't 't #f #t)"] == ["#f"]
                    , withRuntime ["(and 3 5 2 't)"] == ["#t"]
                    , withRuntime ["(or #t #t #t #t)"] == ["#t"]
                    , withRuntime ["(or #t #t #t #f)"] == ["#t"]
                    , withRuntime ["(and)"] == ["#t"]
                    , withRuntime ["(or)"] == ["#f"]
                    , withRuntime ["(or 'nil 3 5 #t)"] == ["#t"]
                    ]

--- ### Comparison Operators

tests_compRuntime :: [Bool]
tests_compRuntime = [ withRuntime ["(<)"] == ["#t"]
                    , withRuntime ["(>)"] == ["#t"]
                    , withRuntime ["(<=)"] == ["#t"]
                    , withRuntime ["(>=)"] == ["#t"]
                    , withRuntime ["(=)"] == ["#t"]
                    , withRuntime ["(< 3 4 5)"] == ["#t"]
                    , withRuntime ["(< 3 3 5)"] == ["#f"]
                    , withRuntime ["(> 3 4 5)"] == ["#f"]
                    , withRuntime ["(> 3 2 1)"] == ["#t"]
                    , withRuntime ["(= 3 3 3)"] == ["#t"]
                    , withRuntime ["(= 3 3 1)"] == ["#f"]
                    ]


--- ### List Operators
tests_listOps :: [Bool]
tests_listOps = [ withRuntime ["(car '(10))"] == ["10"]
                , withRuntime ["(cdr '(10 20 30))"] == ["(20 30)"]
                , withRuntime ["(cdr '(10 20 . 30))"] == ["(20 . 30)"]
                , withRuntime ["(cdr '(10 . (20 30)))"] == ["(20 30)"]
                , withRuntime ["(cdr '(10 . 'a))"] == ["(quote a)"]
                , withRuntime ["(cdr '(10))"] == ["()"]
                , withRuntime ["(cons 10 20)"] == ["(10 . 20)"]
                , withRuntime ["(car (cons 10 (cons 20 '())))"] == ["10"]
                , withRuntime ["(car (cons 2 3))"] == ["2"]
                , withRuntime ["(cdr (cons 2 3))"] == ["3"]
                , withRuntime ["(list 'a 'b 'c 'd)"] == ["(a b c d)"]
                , withRuntime ["(list (+ 10 20) (+ 30 40))"] == ["(30 70)"]
                , withRuntime ["(cons 10 (list 20 30))"] == ["(10 20 30)"]
                , withRuntime ["(cons '() '(. ()))"] == ["(())"]
                ]

--- ### Unary Boolean Operators
tests_unaryRuntime :: [Bool]
tests_unaryRuntime = [ withRuntime ["(not)"] == ["Error: Unexpected arguments or wrong number of arguments ()"]
                     , withRuntime ["(not 7 2)"] == ["Error: Unexpected arguments or wrong number of arguments (7 2)"]
                     , withRuntime ["(not #t)"] == ["#f"]
                     , withRuntime ["(not #t)"] == ["#f"]
                     , withRuntime ["(not '#f)"] == ["#t"]
                     , withRuntime ["(not #f)"] == ["#t"]
                     , withRuntime ["(not #f #t)"] == ["Error: Unexpected arguments or wrong number of arguments (#f #t)"]
                     , withRuntime ["(not #f #t 3)"] == ["Error: Unexpected arguments or wrong number of arguments (#f #t 3)"]
                     , withRuntime ["(not #f 3)"] == ["Error: Unexpected arguments or wrong number of arguments (#f 3)"]
                     , withRuntime ["(not 3)"] == ["#f"]
                     ]

tests_equality :: [Bool]
tests_equality = [ withRuntime ["(=)"] == ["#t"]
                 , withRuntime ["(eq?)"] == ["#t"]
                 , withRuntime ["(= 10 20)"] == ["#f"]
                 , withRuntime ["(= 10 10)"] == ["#t"]
                 , withRuntime ["(= 10 10 10)"] == ["#t"]
                 , withRuntime ["(= 10 10 10 10 10 10 10 10 10 10 10 10 ((lambda (x) 10) 1) 10 10 10)"] == ["#t"]
                 , withRuntime ["(= 10 10 10 10 30 10 10 10)"] == ["#f"]
                 , withRuntime ["(= 'a 'a)"] == ["Error: Value a has unexpected type Symbol"]
                 , withRuntime ["(define x ((lambda () (+ 20 100))))", "(= ''120 ''120)"] == ["", "Error: Value (quote 120) has unexpected type List"]
                 , withRuntime ["(define x ((lambda () (+ 20 100))))", "(define y x)", "(= x 120)", "(= x y)", "(= 'y 'y)"] == ["", "", "#t", "#t", "Error: Value y has unexpected type Symbol"]
                 , withRuntime ["(= (lambda (x) (x)) (lambda (x) (x)))"] == ["Error: Value #<function:(\955 (x) ...)> has unexpected type Func"]
                 , withRuntime ["(eq? 'a 'a)"] == ["#t"]
                 , withRuntime ["(eq? 'a 'a 'a)"] == ["#t"]
                 , withRuntime ["(eq? 'a 'a 'b 'a (car (cons 'a 'b)))"] == ["#f"]
                 , withRuntime ["(eq? '() '())"] == ["#f"]
                 , withRuntime ["(define a (- (- 10)))", "(eq? 10 a)"] == ["", "#t"]
                 , withRuntime ["(eq? (lambda () ()) (lambda (f) (lambda () ())))"] == ["#f"]
                 , withRuntime ["(eq? ((lambda () 1)) (((lambda () (lambda () 1)))))"] == ["#t"]
                 ]

tests_modulo :: [Bool]
tests_modulo = [ withRuntime ["(modulo)"] == ["Error: Unexpected arguments or wrong number of arguments ()"]
               , withRuntime ["(modulo 1)"] == ["Error: Unexpected arguments or wrong number of arguments (1)"]
               , withRuntime ["(modulo 4 3)"] == ["1"]
               , withRuntime ["(modulo 9 3)"] == ["0"]
               , withRuntime ["(modulo 9 (- 2))"] == ["-1"]
               , withRuntime ["(modulo (- 9) 2)"] == ["1"]
               ]

tests_type_preds :: [Bool]
tests_type_preds = [ withRuntime ["(symbol? 'a)"] == ["#t"]
                   , withRuntime ["(symbol? 'b)"] == ["#t"]
                   , withRuntime ["(define (caddddddr x) (car (cdr (cdr (cdr (cdr (cdr (cdr x))))))))", "(define sentence '(Can you cancel this MP ? . (No)))", "sentence", "(symbol? (caddddddr sentence))"] == ["", "", "(Can you cancel this MP ? No)", "#t"]
                   , withRuntime ["(symbol?)"] == ["Error: Unexpected arguments or wrong number of arguments ()"]
                   , withRuntime ["(symbol? 3)"] == ["#f"]
                   , withRuntime ["(symbol? (car ((lambda (x) (cons x '(3 5))) 'y)))"] == ["#t"]
                   , withRuntime ["(list? '(3 5))"] == ["#t"]
                   , withRuntime ["(list? '())"] == ["#t"]
                   , withRuntime ["(list? '(3 . (6 . 7)))"] == ["#f"]
                   , withRuntime ["(list? '(3 . (6 . (1 2 3))))"] == ["#t"]
                   , withRuntime ["(list? '(3 . (6 . ())))"] == ["#t"]
                   , withRuntime ["(list? '(3 5 . 6))"] == ["#f"]
                   , withRuntime ["(list? '(3 5 (6 . 7)))"] == ["#t"]
                   , withRuntime ["(list? 3)"] == ["#f"]
                   , withRuntime ["(list? 3 5)"] == ["Error: Unexpected arguments or wrong number of arguments (3 5)"]
                   , withRuntime ["(list?)"] == ["Error: Unexpected arguments or wrong number of arguments ()"]
                   , withRuntime ["(pair?)"] == ["Error: Unexpected arguments or wrong number of arguments ()"]
                   , withRuntime ["(pair? 3)"] == ["#f"]
                   , withRuntime ["(pair? '(3 . 6))"] == ["#t"]
                   , withRuntime ["(pair? ((lambda (x) (cons x '(3 5))) 'x))"] == ["#t"]
                   , withRuntime ["(number? '(3))"] == ["#f"]
                   , withRuntime ["(pair? '())"] == ["#f"]
                   , withRuntime ["(pair? 1 2)"] == ["Error: Unexpected arguments or wrong number of arguments (1 2)"]
                   , withRuntime ["(number? ((lambda () 3)))"] == ["#t"]
                   , withRuntime ["(boolean? (lambda (x) 10))"] == ["#f"]
                   , withRuntime ["(boolean? #f)"] == ["#t"]
                   , withRuntime ["(boolean? 'True)", "(boolean? ''#t)", "(boolean? ''#f)"] == ["#f", "#f", "#f"]
                   , withRuntime ["(number? 10)"] == ["#t"]
                   , withRuntime ["(define n ((lambda (x) (+ 1 x)) 3))", "(number? n)"] == ["", "#t"]
                   , withRuntime ["(number? #t)"] == ["#f"]
                   , withRuntime ["(number?)"] == ["Error: Unexpected arguments or wrong number of arguments ()"]
                   , withRuntime ["(boolean?)"] == ["Error: Unexpected arguments or wrong number of arguments ()"]
                   , withRuntime ["(boolean? 3 #f)"] == ["Error: Unexpected arguments or wrong number of arguments (3 #f)"]
                   , withRuntime ["(null? '())"] == ["#t"]
                   , withRuntime ["(null? '('()))"] == ["#f"]
                   , withRuntime ["(null? '(3 5))"] == ["#f"]
                   , withRuntime ["(null? '(. (. ())))"] == ["#t"]
                   , withRuntime ["(null? (cons '() '(. ())))"] == ["#f"]
                   , withRuntime ["(null?)"] == ["Error: Unexpected arguments or wrong number of arguments ()"]
                   , withRuntime ["(null? '() '())"] == ["Error: Unexpected arguments or wrong number of arguments (() ())"]
                   ]

--- Evaluation
--- ----------

tests_evaluator :: [([Bool], String)]
tests_evaluator = [ (tests_atoms, "self-evaluating atoms")
                  , (tests_lookup, "value lookup")
                  , (tests_define,  "define special form")
                  , (tests_lambda, "lambda form")
                  , (tests_hofs, "simple HOF test")
                  , (tests_cond, "conds tests")
                  , (tests_let, "let")
                  , (tests_letstar, "let*")
                  , (tests_quote_eval, "quote and eval")
                  , (tests_define_macro, "define-macro")
                  , (tests_quote_eval, "quote, quasiquote, and eval")
                  , (tests_apply, "apply")
                  , (tests_eval, "eval")
                  ]

tests_atoms :: [Bool]
tests_atoms = [ withRuntime ["10"] == ["10"]
              , withRuntime ["#t", "#f"] == ["#t", "#f"]
              ]

tests_lookup :: [Bool]
tests_lookup = [ withRuntime ["+", "-"] == ["#<primitive>", "#<primitive>"]
               , withRuntime ["Mattox"] == ["Error: Symbol Mattox is undefined"]
               ]

tests_define :: [Bool]
tests_define = [ withRuntime ["(define x 10)","x","y"] == ["","10","Error: Symbol y is undefined"]
               , withRuntime ["(define (id x) x)","(id 10)"] == ["","10"]
               , withRuntime ["(define (inc y) (+ y 1))","(inc 10)"] == ["","11"]
               , withRuntime ["(define (plus a b) (+ a b))","(plus 10 20)","(plus (plus 10 20) (plus 30 40))"] == ["","30","100"]
               , withRuntime ["(define (fact n) (cond ((< n 1) 1) (else (* n (fact (- n 1))))))","(fact 5)"] == ["","120"]
               , withRuntime ["(define foo (lambda (x) (+ 10 x)))","(foo 20)"] == ["","30"]
               ]

tests_lambda :: [Bool]
tests_lambda = [ withRuntime ["(lambda (x) (+ x 10))"] == ["#<function:(\955 (x) ...)>"]
               , withRuntime ["((lambda (x) (+ (((lambda () (lambda () x)))) 10)) 20)"] == ["30"]
               , withRuntime ["(lambda (X) ((lambda (f) (X (lambda (arg) ((f f) arg)))) (lambda (f) (X (lambda (arg) ((f f) arg))))))"] == ["#<function:(\955 (X) ...)>"]
               , withRuntime ["(lambda X ((lambda (f) (X (lambda (arg) ((f f) arg)))) (lambda (f) (X (lambda (arg) ((f f) arg))))))"] == ["Error: Invalid pattern in special form `lambda`: (lambda X ((lambda (f) (X (lambda (arg) ((f f) arg)))) (lambda (f) (X (lambda (arg) ((f f) arg))))))"]
               ]

tests_hofs :: [Bool]
tests_hofs = [ withRuntime ["(define (twice f x) (f (f x)))","(define (inc x) (+ x 10))","(twice inc 10)"] == ["","","30"]
             , withRuntime ["(define (twice f x) (f (f x)))","(define g ((lambda () twice)))","(define (inc x) (+ x 10))","(g inc 10)"] == ["","","","30"]
             , withRuntime ["(define Y (lambda (X) ((lambda (f) (X (lambda (arg) ((f f) arg)))) (lambda (f) (X (lambda (arg) ((f f) arg)))))))", "(define fact (Y (lambda (f) (lambda (n) (cond ((= n 0) 1) (else (* n (f (- n 1)))))))))", "(fact 10)"] == ["", "", "3628800"]
             ]

tests_cond :: [Bool]
tests_cond = [ withRuntime ["(cond (#f 1) (#f 2))"] == [""]
             , withRuntime ["(cond (else 1) (#t 3))"] == ["Error: Invalid pattern in special form `cond`: (cond (else 1) (#t 3))"]
             , withRuntime ["(cond)"] == ["Error: Invalid pattern in special form `cond`: (cond)"]
             , withRuntime ["(cond ('a 1) ((+ 1 2) (+ 3 4)) (else 5))"] == ["1"]
             , withRuntime ["(cond ((+ 4 3) 'a) ((- 4 2) 'b))"] == ["a"]
             , withRuntime ["(cond (#f 'a) ((- 4 2) 'b))"] == ["b"]
             , withRuntime ["(cond ((+ 4 3) 'a) ((- 4 2) 'b))"] == ["a"]
             , withRuntime ["(cond (False 'a) ((- 4 2) 'b))"] == ["Error: Symbol False is undefined"]
             , withRuntime ["(cond (True 'a) ((- 4 2) 'b))"] == ["Error: Symbol True is undefined"]
             , withRuntime ["(cond ((not 'a) 1) ((+ 1 2) (+ 3 4)) (else 5))"] == ["7"]
             , withRuntime ["(cond (#f 'a) (#f (cond (#f 1))) (((lambda () 'a)) (cond ((+ 1 2 3) 7))))"] == ["7"]
             ]

tests_let :: [Bool]
tests_let = [ withRuntime ["(let ((x 5) (y 10)) (+ x y)) "] == ["15"]
            , withRuntime ["(define x 20)","(define y 30)","(let ((x 11) (y 4)) (- (* x y) 2))"] == ["","","42"]
            , withRuntime ["(define x 20)","(define y 30)","(let ((x 11) (y 4)) (- (* x y) 2))","x","y"] == ["","","42","20","30"]
            , withRuntime ["(define x 20)","(define y 30)","(let ((x 11) (y x)) (- (* x y) 2))"] == ["","","218"]
            , withRuntime ["(define x 20)","(define y 30)","(let ((x 11) (y x)) (- (* x y) 2))","x","y"] == ["","","218","20","30"]
            ]

tests_letstar :: [Bool]
tests_letstar = [ withRuntime ["(let* ((x 5) (y 10)) (+ x y)) "] == ["15"]
                , withRuntime ["(define x 20)","(define y 30)","(let* ((x 11) (y x)) (- (* x y) 2))"] == ["","","119"]
                , withRuntime ["(define x 20)","(define y 30)","(let* ((x 11) (y x)) (- (* x y) 2))","x","y"] == ["","","119","20","30"]
                -- This is constructing and calling a recursive 'fac' using Y combinator
                , withRuntime ["(let* ((Y (lambda (X) ((lambda (f) (X (lambda (arg) ((f f) arg)))) (lambda (f) (X (lambda (arg) ((f f) arg))))))) (fac (Y (lambda (f) (lambda (n) (cond ((= n 0) 1) (else (* n (f (- n 1)))))))))) (fac 10))"] == ["3628800"]
                ]

tests_quote_eval :: [Bool]
tests_quote_eval = [ withRuntime ["'a"] == ["a"]
                   , withRuntime ["'5"] == ["5"]
                   , withRuntime ["(quote a)"] == ["a"]
                   , withRuntime ["'*first-val*"] == ["*first-val*"]
                   , withRuntime ["''a"] == ["(quote a)"]
                   , withRuntime ["(car (quote (a b c)))"] == ["a"]
                   , withRuntime ["(define (make-list x y z) `(,x ,y ,z))", "(car (make-list ((lambda (b) b) 'a) 'b 'c))"] == ["", "a"]
                   , withRuntime ["(car '(a b . c))"] == ["a"]
                   , withRuntime ["(car ''(a b c))"] == ["quote"]
                   , withRuntime ["'(2 3 4)"] == ["(2 3 4)"]
                   , withRuntime ["(list (+ 2 3))"] == ["(5)"]
                   , withRuntime ["'( (+ 2 3))"] == ["((+ 2 3))"]
                   , withRuntime ["'(+ 2 3)"] == ["(+ 2 3)"]
                   , withRuntime ["(eval '(+ 1 2))"] == ["3"]
                   , withRuntime ["(eval ''(+ 1 2))"] == ["(+ 1 2)"]
                   , withRuntime ["(eval (eval ''(+ 1 2)))"] == ["3"]
                   , withRuntime ["(define a '(+ x 1))","(define x 5)","(eval a)","(define a 5)","``(+ ,,a 1)","``(+ ,,a ,a)","`(+ a ,,a)","``(+ a ,,a)","(eval ``(+ ,,a 1))","(eval (eval ``(+ ,,a 1)))"] == ["","","6","","(quasiquote (+ (unquote 5) 1))","(quasiquote (+ (unquote 5) (unquote a)))","Error: `unquote` not in a `quasiquote` context: a"]
                   , withRuntime ["(define a '(+ x 1))","(define x 5)","(eval a)","(define a 5)","``(+ ,,a 1)","``(+ ,,a ,a)","``(+ a ,,a)","(eval ``(+ ,,a 1))","(eval (eval ``(+ ,,a 1)))"] == ["","","6","","(quasiquote (+ (unquote 5) 1))","(quasiquote (+ (unquote 5) (unquote a)))","(quasiquote (+ a (unquote 5)))","(+ 5 1)","6"]
                   ]

tests_define_macro :: [Bool]
tests_define_macro = [ withRuntime ["(define-macro (if con then else) `(cond (,con ,then) (else ,else)))","if","(define a 5)","(if (> a 2) 10 20)","(if (< a 2) 10 20)","(define (fact n) (if (< n 1) 1 (* n (fact (- n 1)))))","(fact 10)"] == ["","#<macro (con then else) ...>","","10","20","","3628800"]
                     , withRuntime ["(define-macro (if con then else) `(cond (,con ,then) (else ,else)))","(define-macro (mkplus e) (if (eq? (car e) '-) (cons '+ (cdr e)) e))","mkplus","(mkplus (- 5 4))"] == ["","","#<macro (e) ...>","9"]
                     , withRuntime ["(define-macro (if con then else) `(cond (,con ,then) (else else)))", "(if #f 2 3)"] == ["", "Error: Symbol else is undefined"]
                     ]


tests_apply :: [Bool]
tests_apply = [ withRuntime ["(apply + '(1 2 3))"] == ["6"]
              , withRuntime ["(apply + '(1 (car '(1 2)) 3))"] == ["Error: Value (car (quote (1 2))) has unexpected type List"]
              , withRuntime ["(apply car '((1 2 3)))"] == ["1"]
              , withRuntime ["(apply ((lambda () car)) '((1 2 3)))"] == ["1"]
              , withRuntime ["(apply (((lambda () (lambda () car)))) '((1 2 3)))"] == ["1"]
              , withRuntime ["(define mycar (lambda (x) (car x)))", "(apply mycar '((1 2 3)))"] == ["", "1"]
              ]

tests_eval :: [Bool]
tests_eval = [ withRuntime ["(eval 1)"] == ["1"]
             , withRuntime ["(eval 'a)"] == ["Error: Symbol a is undefined"]
             , withRuntime ["(eval '(lambda (f x) (f x)))"] == ["#<function:(\955 (f x) ...)>"]
             , withRuntime ["(eval '(eval '(eval '(+ ((lambda (x y z) 1) '() '() '()) 2 3))))"] == ["6"]
             , withRuntime ["(eval '(apply + '(1 2 3)))"] == ["6"]
             ]
