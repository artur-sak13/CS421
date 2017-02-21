--- Getting Started
--- ===============

--- Testing Your Code
--- -----------------

module Tests where

import Data.List ((\\))
import Main hiding (main)

allTests :: [([Bool], String)]
allTests = [ (tests_factk, "factk tests")
           , (tests_evenoddk, "evenoddk tests")
           , (tests_isSimple, "isSimple tests")
           , (tests_cpsExp_intVar, "cpsExp: Integer and Variable expressions")
           , (tests_cpsExp_simpOp, "cpsExp: Simple Operator expressions")
           , (tests_cpsExp_nonSimpOp, "cpsExp: Non-Simple Operator expressions")
           , (tests_cpsExp_simpIf, "cpsExp: Simple If expressions")
           , (tests_cpsExp_nonSimpIf, "cpsExp: Non-Simple If expressions")
           , (tests_cpsExp_ap, "cpsExp: Application expressions")
           , (tests_cpsDecl, "cpsDecl tests")
           ]


--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

tests_factk :: [Bool]
tests_factk = [ factk 15 (*2)               == 2615348736000
              , factk 12 ((*2) . (+1))      == 958003202
              , factk 3 (flip factk show)   == "720"
              , factk 5 (flip factk (*2))   == 13379005826898254115176236108180745173505492666276059620591342704603267114489925978733748330543969962616315275786428181105068817178816243719796962228779300011929921042513920000000000000000000000000000
              , factk 7 (show . (`div` 2))  == "2520"
              , factk 0 show                == "1"
              ]

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

tests_evenoddk :: [Bool]
tests_evenoddk = [ evenoddk [1,2,3,4]     (+1)               (*2)            == 7
                 , evenoddk [2,24,-12,15] ((*2) . (`div` 2)) (+1)            == 16
                 , evenoddk [1,23,8,-12]  show               (const "hello") == "-4"
                 , evenoddk [7845,1,24]   (const "hello")    (const "oeun")  == "hello"
                 , evenoddk [9]           id                 (*2)            == 18
                 ]

--- Automated Translation
--- ---------------------

--- ### Define `isSimple`

expSimple :: [String]
expSimple = expVar ++ expInt ++ expSimpOp ++ expSimpIf

expNonSimple :: [String]
expNonSimple = expNonSimpOp ++ expNonSimpIf ++ expAp

tests_isSimple :: [Bool]
tests_isSimple =    map (isSimple . toExp)       expSimple
                 ++ map (not . isSimple . toExp) expNonSimple

--- ### `cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)`

toExp :: String -> Exp
toExp = (\(Right exp) -> exp) . parseExp

cpsExpRun :: String -> Exp
cpsExpRun input = fst $ cpsExp (toExp input) (VarExp "k") 1

freeVars :: Exp -> [String]
freeVars (VarExp v)       = [v]
freeVars (IntExp i)       = []
freeVars (LamExp v b)     = freeVars b \\ [v]
freeVars (OpExp op e1 e2) = freeVars e1 ++ freeVars e2
freeVars (IfExp c e1 e2)  = freeVars c ++ freeVars e1 ++ freeVars e2
freeVars (AppExp f a)     = freeVars f ++ freeVars a

replaceWith :: (String, String) -> Exp -> Exp
replaceWith (v1,v2) (VarExp v)
    | v1 == v                  = VarExp v2
    | otherwise                = VarExp v
replaceWith (v1,v2) (LamExp v b)
    | v1 == v                  = LamExp v b
    | otherwise                = LamExp v (replaceWith (v1,v2) b)
replaceWith b (IntExp i)       = IntExp i
replaceWith b (OpExp op e1 e2) = OpExp op (replaceWith b e1) (replaceWith b e2)
replaceWith b (IfExp c e1 e2)  = IfExp (replaceWith b c) (replaceWith b e1) (replaceWith b e2)
replaceWith b (AppExp f a)     = AppExp (replaceWith b f) (replaceWith b a)

equivExp :: Exp -> Exp -> Bool
equivExp (VarExp v1)         (VarExp v2)         = v1 == v2
equivExp (IntExp i1)         (IntExp i2)         = i1 == i2
equivExp (OpExp op1 e11 e12) (OpExp op2 e21 e22) = op1 == op2 && equivExp e11 e21 && equivExp e12 e22
equivExp (AppExp f1 a1)      (AppExp f2 a2)      = equivExp f1 f2 && equivExp a1 a2
equivExp (IfExp c1 e11 e12)  (IfExp c2 e21 e22)  = equivExp c1 c2 && equivExp e11 e21 && equivExp e12 e22
equivExp (LamExp v1 b1)      (LamExp v2 b2)
    | v1 == v2                                   = equivExp b1 b2
    | v1 /= v2 && v1 `elem` freeVars b2          = let newVar = concat $ freeVars b1
                                                       lamExp = LamExp newVar $ replaceWith (v1, newVar) b1
                                                   in  equivExp lamExp (LamExp v2 b2)
    | otherwise                                  = equivExp b1 $ replaceWith (v2, v1) b2
equivExp _                   _                   = False

cpsExpAll :: ([String], [String]) -> [Bool]
cpsExpAll (inputs, outputs) = zipWith equivExp (map cpsExpRun inputs) (map toExp outputs)

--- #### Define `cpsExp` for Integer and Variable Expressions

expVar :: [String]
expVar = [ "y", "x" ]
expVarK :: [String]
expVarK = [ "k y", "k x" ]

expInt :: [String]
expInt = [ "34", "18", "212" ]
expIntK :: [String]
expIntK = [ "k 34", "k 18", "k 212" ]

tests_cpsExp_intVar :: [Bool]
tests_cpsExp_intVar = cpsExpAll (expVar ++ expInt, expVarK ++ expIntK)

--- #### Define `cpsExp` for If Expressions

expSimpIf :: [String]
expSimpIf = [ "if 3 < 5 then 44 + x else 14 * 2"
            , "if 16 == y then y + 12 else 7"
            ]
expSimpIfK :: [String]
expSimpIfK = [ "if 3 < 5 then k (44 + x) else k (14 * 2)"
             , "if 16 == y then k (y + 12) else k (7)"
             ]

tests_cpsExp_simpIf :: [Bool]
tests_cpsExp_simpIf = cpsExpAll (expSimpIf, expSimpIfK)

expNonSimpIf :: [String]
expNonSimpIf = [ "if p (3 + a) then 15 + 7 else 12"
               , "if 3 < (f a) then 15 * g k else 232"
               , "17 * 8 + (if f a then b else c d)"
               ]
expNonSimpIfK :: [String]
expNonSimpIfK = [ "p (3 + a) (\\v1 -> (if v1 then k (15 + 7) else k 12))"
                , "f a (\\v3 -> (\\v1 -> (if v1 then g k (\\v2 -> k (15 * v2)) else k 232)) (3 < v3))"
                , "f a (\\v2 -> (if v2 then (\\v1 -> k ((17 * 8) + v1)) b else c d (\\v1 -> k ((17 * 8) + v1))))"
                ]

tests_cpsExp_nonSimpIf :: [Bool]
tests_cpsExp_nonSimpIf = cpsExpAll (expNonSimpIf, expNonSimpIfK)

--- #### Define `cpsExp` for Operator Expressions

expSimpOp :: [String]
expSimpOp = [ "3 * x"
            , "15 + (3 * x)"
            ]
expSimpOpK :: [String]
expSimpOpK = [ "k (3 * x)"
             , "k (15 + (3 * x))"
             ]

tests_cpsExp_simpOp :: [Bool]
tests_cpsExp_simpOp = cpsExpAll (expSimpOp, expSimpOpK)

expNonSimpOp :: [String]
expNonSimpOp = [ "4 * f a"
               , "5 < (3 + g q)"
               ]
expNonSimpOpK :: [String]
expNonSimpOpK = [ "f a (\\v1 -> k (4 * v1))"
                , "g q (\\v2 -> (\\v1 -> k (5 < v1)) (3 + v2))"
                ]

tests_cpsExp_nonSimpOp :: [Bool]
tests_cpsExp_nonSimpOp = cpsExpAll (expNonSimpOp, expNonSimpOpK)

--- #### Define `cpsExp` for Application Expressions

expAp :: [String]
expAp = [ "f a", "f (g + 3)"
        , "15 * (p a)", "p (12 + y)"
        , "if p (a + 3) then 5 < g b else 8 < q"
        ]
expApK :: [String]
expApK = [ "f a k", "f (g + 3) k"
         , "p a (\\v1 -> k (15 * v1))", "p (12 + y) k"
         , "p (a + 3) (\\v1 -> (if v1 then g b (\\v2 -> k (5 < v2)) else k (8 < q)))"
         ]

tests_cpsExp_ap :: [Bool]
tests_cpsExp_ap = cpsExpAll (expAp, expApK)

--- ### Define `cpsDecl`

cpsDeclStrs :: [String]
cpsDeclStrs = [ "f x = x + 1"
              , "f x g = if x < 3 then g x else g (g x)"
              , "f x g h = if h x < g (g x) then h (g x) else g (h x)"
              ]
cpsDeclStrsK :: [String]
cpsDeclStrsK = [ "f x k = k (x + 1)"
               , "f x g k = (if (x < 3) then g x k else g x (\\v1 -> g v1 k))"
               , "f x g h k = h x (\\v7 -> g x (\\v11 -> g v11 (\\v9 -> (\\v1 -> (if v1 then g x (\\v3 -> h v3 k) else h x (\\v5 -> g v5 k))) (v7 < v9))))"
               ]

toStmt :: String -> Stmt
toStmt = (\(Right stmt) -> stmt) . parseDecl

equivStmt :: Stmt -> Stmt -> Bool
equivStmt (Decl f1 ps1 b1) (Decl f2 ps2 b2) = f1 == f2 && ps1 == ps2 && equivExp b1 b2

tests_cpsDecl :: [Bool]
tests_cpsDecl = zipWith equivStmt (map (cpsDecl . toStmt) cpsDeclStrs) (map toStmt cpsDeclStrsK)
