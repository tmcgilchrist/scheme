{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module Scheme.Parser (
    readExpr
  , parseExpr
  , parseString
  , parseAtom
  , parseNumber
  , LispVal (..)
  ) where

import           Control.Monad.Except
import qualified Data.Text as T
import           Scheme.Data
import           Text.ParserCombinators.Parsec hiding (spaces)

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

readExpr :: T.Text -> ThrowsError LispVal
readExpr input = case (parse parseExpr "lisp" (T.unpack input)) of
  Left err  -> throwError $ Parser err
  Right val -> return val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return . String . T.pack $ x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = T.pack $ first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do _ <- char '('
               x <- try parseList <|> parseDottedList
               _ <- char ')'
               return x

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted =
  char '\'' >> parseExpr >>= \x -> return $ List [Atom "quote", x]
