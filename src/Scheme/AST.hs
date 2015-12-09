{-# LANGUAGE OverloadedStrings #-}
module Scheme.AST (
    LispVal (..)
  , unwordsList
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

data LispVal =
    Atom Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String Text
  | Bool Bool

instance Show LispVal where show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal (String contents) = T.unwords ["\"", contents, "\""]
showVal (Atom name) = name
showVal (Number contents) = T.pack $ show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = T.unwords ["(", unwordsList contents, ")"]
showVal (DottedList h t) = T.unwords ["(", unwordsList h, " . ", showVal t, ")"]

unwordsList :: [LispVal] -> T.Text
unwordsList = T.unwords . Prelude.map showVal
