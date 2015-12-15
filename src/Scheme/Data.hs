{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Scheme.Data (
    ThrowsError
  , LispVal (..)
  , LispError (..)
  , Env
  , showVal
  ) where

import           Control.Monad.Error
-- TODO convert to Except
--import           Control.Monad.Except
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.ParserCombinators.Parsec (ParseError)

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

type Env = IORef [(Text, IORef LispVal)]

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ (T.unpack . unwordsList $ found)
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default e)             = "Default error at " ++ show e

instance Show LispError where show = showError

data LispVal =
    Atom Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String Text
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func { params :: [Text], vargs :: (Maybe Text),
          body :: [LispVal], closure :: Env }

instance Show LispVal where show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal (String contents) = T.unwords ["\"", contents, "\""]
showVal (Atom name) = name
showVal (Number contents) = T.pack $ show contents
showVal (Bool True) = "#true"
showVal (Bool False) = "#false"
showVal (List contents) = T.unwords ["(", unwordsList contents, ")"]
showVal (DottedList h t) = T.unwords ["(", unwordsList h, " . ", showVal t, ")"]
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vargs = varargs, body = _body, closure = _env}) =
  T.pack $ "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ (T.unpack arg)) ++ ") ...)"

unwordsList :: [LispVal] -> T.Text
unwordsList = T.unwords . Prelude.map showVal
