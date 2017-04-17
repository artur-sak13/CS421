import Test.Framework (defaultMain, testGroup, plusTestOptions, topt_maximum_generated_tests)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as S

import Lib

main :: IO ()
main = defaultMain tests

mkset :: [String] -> S.HashSet Symbol
mkset xx = S.fromList $ map (\x -> if x == "eps" then Epsilon else Symbol x) xx

mkhash :: [(String,[String])] -> H.HashMap Symbol (S.HashSet Symbol)
mkhash = foldr (\(s,items) hs -> H.insert (Symbol s) (mkset items) hs) H.empty

syms1 = map Symbol [ "x"]
syms2 = map Symbol [ "x",  "y"]
syms3 = map Symbol [ "x",  "A"]
syms4 = map Symbol [ "A",  "z"]
syms5 = map Symbol [ "A",  "B"]
syms6 = map Symbol [ "A",  "B",  "q"]

fs1 = H.empty
fs2 = mkhash [("A",["i","j"])
             ,("B",["a","b"])]
fs3 = mkhash [("A",["eps", "d"])
             ,("B",["c"])]
fs4 = mkhash [("A",["eps"])
             ,("B",["eps", "c"])]

testFirst :: [Symbol] -> H.HashMap Symbol (S.HashSet Symbol) -> (S.HashSet Symbol) -> Bool
testFirst syms fs expect =
  first fs syms == expect

testGetFirst grm expect =
  getFirstSet grm == expect

testIsLLTrue = isLL

testIsLLFalse grm  = not $ isLL grm

testOne name test =
  plusTestOptions (mempty { topt_maximum_generated_tests = Just 1}) $ testProperty name test

tests = [
        testGroup "Calls to First" [
                testOne "first set test 1-1" (testFirst syms1 fs1 (mkset ["x"]))
               ,testOne "first set test 1-2" (testFirst syms1 fs2 (mkset ["x"]))
               ,testOne "first set test 1-3" (testFirst syms1 fs3 (mkset ["x"]))
               ,testOne "first set test 1-4" (testFirst syms1 fs4 (mkset ["x"]))
               ,testOne "first set test 2-1" (testFirst syms2 fs1 (mkset ["x"]))
               ,testOne "first set test 2-2" (testFirst syms2 fs2 (mkset ["x"]))
               ,testOne "first set test 2-3" (testFirst syms2 fs3 (mkset ["x"]))
               ,testOne "first set test 2-4" (testFirst syms2 fs4 (mkset ["x"]))
               ,testOne "first set test 3-1" (testFirst syms3 fs1 (mkset ["x"]))
               ,testOne "first set test 3-2" (testFirst syms3 fs2 (mkset ["x"]))
               ,testOne "first set test 3-3" (testFirst syms3 fs3 (mkset ["x"]))
               ,testOne "first set test 3-4" (testFirst syms3 fs4 (mkset ["x"]))
               ,testOne "first set test 4-1" (testFirst syms4 fs1 (mkset ["A"]))
               ,testOne "first set test 4-2" (testFirst syms4 fs2 (mkset ["A", "i", "j"]))
               ,testOne "first set test 4-3" (testFirst syms4 fs3 (mkset ["A", "d", "z"]))
               ,testOne "first set test 4-4" (testFirst syms4 fs4 (mkset ["A", "z"]))
               ,testOne "first set test 5-1" (testFirst syms5 fs1 (mkset ["A"]))
               ,testOne "first set test 5-2" (testFirst syms5 fs2 (mkset ["A", "i", "j"]))
               ,testOne "first set test 5-3" (testFirst syms5 fs3 (mkset ["A", "d", "B", "c"]))
               ,testOne "first set test 5-4" (testFirst syms5 fs4 (mkset ["A", "B", "c", "eps"]))
               ,testOne "first set test 6-1" (testFirst syms6 fs1 (mkset ["A"]))
               ,testOne "first set test 6-2" (testFirst syms6 fs2 (mkset ["A", "i", "j"]))
               ,testOne "first set test 6-3" (testFirst syms6 fs3 (mkset ["A", "d", "B", "c"]))
               ,testOne "first set test 6-4" (testFirst syms6 fs4 (mkset ["A", "B", "c", "q"]))
           ]
        , testGroup "Calls to getFirstSet" [
            testOne "get first set test ll1" (testGetFirst g_ll1 fs_ll1)
           ,testOne "get first set test ll2" (testGetFirst g_ll2 fs_ll2)
           ,testOne "get first set test ll3" (testGetFirst g_ll3 fs_ll3)
           ,testOne "get first set test rec1" (testGetFirst g_rec1 fs_rec1)
           ,testOne "get first set test rec2" (testGetFirst g_rec2 fs_rec2)
           ,testOne "get first set test rec3" (testGetFirst g_rec3 fs_rec3)
           ,testOne "get first set test rec4" (testGetFirst g_rec4 fs_rec4)
           ,testOne "get first set test cp1" (testGetFirst g_cp1 fs_cp1)
           ,testOne "get first set test cp2" (testGetFirst g_cp2 fs_cp2)
           ,testOne "get first set test cp3" (testGetFirst g_cp3 fs_cp3)
           ,testOne "get first set test cp4" (testGetFirst g_cp4 fs_cp4)
           ]
        , testGroup "Calls to isLL on LL grammars" [
            testOne "get first set test ll1" (testIsLLTrue g_ll1)
           ,testOne "get first set test ll2" (testIsLLTrue g_ll2)
           ,testOne "get first set test ll3" (testIsLLTrue g_ll3)
           ]
        , testGroup "Calls to isLL on left-recursive grammars" [
            testOne "get first set test rec1" (testIsLLFalse g_rec1)
           ,testOne "get first set test rec2" (testIsLLFalse g_rec2)
           ,testOne "get first set test rec3" (testIsLLFalse g_rec3)
           ,testOne "get first set test rec4" (testIsLLFalse g_rec4)
           ]
        , testGroup "Calls to isLL on common prefix grammars" [
            testOne "get first set cp 1" (testIsLLFalse g_cp1)
           ,testOne "get first set cp 2" (testIsLLFalse g_cp2)
           ,testOne "get first set cp 3" (testIsLLFalse g_cp3)
           ,testOne "get first set cp 4" (testIsLLFalse g_cp4)
           ]
      ]

ll1 = "S -> x\n"
g_ll1 = run grammar ll1
fs_ll1 = mkhash [("S",["x"])]

ll2 = "S -> x\n | E y\nE -> z S\n | F S\nF -> a E\n | b S\n | q\n"
g_ll2 = run grammar ll2
fs_ll2 = mkhash [("S",["x","E","z","F","a","b","q"])
                ,("E",["z","F","a","b","q"])
                ,("F",["a","b","q"])]

ll3 = "S -> x S\n | E y\nE -> z E\n | F S\nF -> a F\n | b S\n | q\n"
g_ll3 = run grammar ll3
fs_ll3 = mkhash [("S",["x","E","z","F","a","b","q"])
                ,("E",["z","F","a","b","q"])
                ,("F",["a","b","q"])]

rec1 = "S -> S x\n | y\n"
g_rec1 = run grammar rec1
fs_rec1 = mkhash [("S",["S","y"])]

rec2 = "S -> x E\n | y\nE -> a E\n | E x\n"
g_rec2 = run grammar rec2
fs_rec2 = mkhash [("S",["x","y"])
                 ,("E",["a","E"])]

rec3 = "S -> x S\n | E y\nE -> z E\n | F S\nF -> a F\n | S b\n | q\n"
g_rec3 = run grammar rec3
fs_rec3 = mkhash [("S",["x","E","z","F","a","S","q"])
                ,("E",["z","F","a","S","q","x","E"])
                ,("F",["z","F","a","S","q","x","E"])]

rec4 = "S -> x S\n | E y\nE -> z E\n | F S\nF -> a F\n | b S\n | eps\n"
g_rec4 = run grammar rec4
fs_rec4 = mkhash [("S",["x","E","z","F","S","a","b"])
                ,("E",["x","E","z","F","S","a","b"])
                ,("F",["a","b","eps"])]

cp1 = "S -> x\n  | x y\n"
g_cp1 = run grammar cp1
fs_cp1 = mkhash [("S",["x"])]

cp2 = "S -> x\n  | a y\n  | E x\nE-> q r\n| a\n"
g_cp2 = run grammar cp2
fs_cp2 = mkhash [("S",["E","x","a","q"])
                ,("E",["q","a"])]

cp3 = "S -> x\n  | a y\n  | E x\nE-> q r\n| F\nF -> o x\n | a\n"
g_cp3 = run grammar cp3
fs_cp3 = mkhash [("S",["E","F","x","a","q","o"])
                ,("E",["q","F","o","a"])
                ,("F",["o","a"])
                ]

cp4 = "S -> x\n  | a y\n  | E x\nE-> q r\n| eps\n"
g_cp4 = run grammar cp4
fs_cp4 = mkhash [("S",["E","x","a","q"])
                ,("E",["q","eps"])
                ]

prop1 b = b == False
  where types = (b :: Bool)

prop2 i = i == 42
  where types = (i :: Int)
