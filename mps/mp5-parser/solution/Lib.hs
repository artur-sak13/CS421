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

-- First Set

fix f x =
  if x == result
    then x
    else fix f result
  where result = f x

getFirstSet :: Grammar -> H.HashMap Symbol (S.HashSet Symbol)
getFirstSet (Grammar psets nonterminals terminals) =
     fix aux initial
     where initial = H.fromList (zip (S.toList nonterminals) (repeat S.empty))
           aux fs = undefined

-- first fs symbols
-- return the first set of a set of symbols
first :: S.HashSet Symbol -> [Symbol] -> S.HashSet Symbol
first fs [] = S.empty


someFunc :: IO ()
someFunc = putStrLn "someFunc"
