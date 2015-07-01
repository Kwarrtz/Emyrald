
module Emyrald.Parse (parseExpr, parseProg) where

import Emyrald.Expr (Expr (..), symBase)

import Text.ParserCombinators.Parsec
import Data.Char
import Data.Functor

-- | The function 'parseExpr' parses a string
--   representing an Emyrald expression into
--   its haskellian form (an 'Expr')
parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""

-- | The function 'parseProg' parses a string
--   representain a valid Emyrald program into
--   a list of 'Expr's
parseProg :: String -> Either ParseError [Expr]
parseProg = parse prog ""

prog = expr `sepBy` spaces

expr = numLit <|>
       charLit <|>
       funcApp <|>
       symLit

numLit = try $ do s <- option "" (string "-")
                  n <- many1 digit
                  return $ Num $ read $ s ++ n

charLit = do { char '&'; Num . toInteger . ord <$> anyChar }

funcApp = do
  f <- try $ do
    s <- symLit
    spaces
    char '['
    spaces
    return s
  a <- expr
  spaces
  char ']'
  return $ App f a

symLit = do { char '\''; s <- symLit; return $ Sym s } <|> symBase <$> many (oneOf symChar)
  
symChar = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_!@#$%^*+=`~./<>?;:|"

