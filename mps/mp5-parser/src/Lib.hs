{-# LANGUAGE DeriveGeneric #-}

module Lib where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as S
import Data.Functor.Identity

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)
import Text.Parsec.Char

import GHC.Generics (Generic)
import Data.Hashable
import Data.List (intercalate)
import Debug.Trace
import Data.Typeable

-- The Types

data Symbol = Symbol String
           | Epsilon
   deriving (Eq,Generic)

data Production = Production String [[Symbol]]
   deriving Eq

data Grammar = Grammar [Production] (S.HashSet Symbol) (S.HashSet Symbol)

instance Show Symbol where
  show (Symbol s) = s
  show Epsilon = "ε"

instance Show Production where
  show (Production s xx) = aux header xx
    where header = s ++ " -> "
          padding = replicate (length header - 2) ' ' ++ "| "
          aux _ [] = ""
          aux prefix (x:xs) = prefix ++ unwords (map show x) ++ "\n" ++ aux padding xs

instance Hashable Symbol

showGrammar xx = concatMap show xx

-- The Parser

-- Pretty name for Parser types
type Parser = ParsecT String () Identity

-- for testing a parser directly
run :: Parser a -> String -> a
run p s =
    case parse p "<stdin>" s of
        Right x -> x
        Left x  -> error $ show x

-- inlinews - parses spaces and tabs but not newlines
inlinews :: Parser String
inlinews = many (oneOf " \t") <?> "whitespace"

-- stringws - parses a string and consumes trailing whitespace
stringws :: String -> Parser String
stringws s = do _ <- string s
                _ <- inlinews
                return s

-- ident - parse a non-epsilon identifier, at least one upper or lowercase letter
ident :: Parser String
ident = do i <- try $ do ii <- many1 (oneOf (['a'..'z'] ++ ['A'..'Z'])) <?> "an identifier"
                         if ii == "eps" then fail "eps not expected" else return ii
           _ <- inlinews
           return i

-- realIdent - like ident, but returns it as a Symbol
realIdent :: Parser Symbol
realIdent = do t <- ident <?> "an identifier"
               _ <- inlinews
               return (Symbol t)
-- epsilon - parse "eps" or "ε" returning Epsilon
epsilon :: Parser Symbol
epsilon = do _ <- (string "ε" <|> string "eps") <?> "an epsilon"
             _ <- inlinews
             return Epsilon

-- epsilonLine - parse a production that only has an epsilon in it.
epsilonLine :: Parser [Symbol]
epsilonLine = do _ <- epsilon
                 _ <- endOfLine
                 return [Epsilon]

-- tokenLine - parse a production that is not an epsilon.
tokenLine :: Parser [Symbol]
tokenLine = do tt <- many1 realIdent
               _ <- endOfLine
               return tt

-- initialProduction - parse an initial production line
initialProduction :: Parser (String,[Symbol])
initialProduction = do s <- ident
                       _ <- stringws "->"
                       tt <- epsilonLine <|> tokenLine
                       return (s,tt)

continueProduction :: Parser [Symbol]
continueProduction = do try $ do _ <- inlinews
                                 stringws "|"
                        res <- epsilonLine <|> tokenLine
                        return res

production :: Parser Production
production = do (s,x) <- initialProduction
                xs <- many continueProduction
                return (Production s (x:xs))

grammar :: Parser Grammar
grammar = do p <- many1 production
             return (Grammar p (terminals p) (nonTerminals p))

p0 = "S -> x S y\n | z q Y\n"
p1 = "S -> x S y\n | z q Y\nY -> x Y y \n| eps\n"

-- Some analysis

nonTerminals :: [Production] -> S.HashSet Symbol
nonTerminals g = S.fromList $ map (\ (Production s _) -> Symbol s) g

symbols :: [Production] -> S.HashSet Symbol
symbols [] = S.empty
symbols ((Production s xx):ps) = S.union (S.insert (Symbol s) $ S.fromList (concat xx)) (symbols ps)

terminals :: [Production] -> S.HashSet Symbol
terminals g = S.difference (symbols g) (nonTerminals g)

fix f x =
  if x == result
    then x
    else fix f result
  where result = f x

-- getFirstSet grammar
-- calculate the first sets of the nonterminals in a grammar
getFirstSet :: Grammar -> H.HashMap Symbol (S.HashSet Symbol)
getFirstSet (Grammar psets terminals nonterminals) =
     fix aux initial
     where initial = H.fromList (zip (S.toList nonterminals) (repeat S.empty))
           aux fs = foldl updateFirst fs psets

-- first fs symbols
-- return the first set of a set of symbols
first :: H.HashMap Symbol (S.HashSet Symbol) -> [Symbol] -> S.HashSet Symbol
first fs []                = S.singleton Epsilon
first fs (Epsilon:syms)    = first fs syms
first fs ((Symbol s):syms) =
    let fs_t = S.union (H.lookupDefault S.empty (Symbol s) fs) (S.singleton (Symbol s))
    in if S.member Epsilon fs_t && not (null syms)
     then S.union (S.delete Epsilon fs_t) (first fs syms)
     else fs_t

updateFirst fs (Production s xx) =
  case H.lookup (Symbol s) fs of
    Nothing -> fs
    Just v1 -> H.insert (Symbol s) (S.union v1 (newFst fs xx)) fs

newFst fs_t [] = S.empty
newFst fs_t (x:xs) = S.union (first fs_t x) (newFst fs_t xs)


-- isLL
isLL :: Grammar -> Bool
isLL g = let (Grammar psets terminals nonterminals) = g
  in ((hasCommonPrefix psets) && not(isLeftRecursive g))

isLeftRecursive g =
  let hshmp = (getFirstSet g)
      keys = H.keys hshmp
      in checkKeys hshmp keys


checkKeys fs (k:ks) =
  case H.lookup k fs of
    Nothing -> False
    Just st  -> if S.member k st
      then True
      else if not(null ks)
        then False || (checkKeys fs ks)
        else False

hasCommonPrefix ((Production s xx):ps) =
  let syms =  S.fromList((concat xx))
      k    = S.singleton (Symbol s)
  in if not (null (S.intersection k syms))
      then True
      else if not(null ps)
        then False || (hasCommonPrefix ps)
        else False
