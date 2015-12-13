{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Scheme.Data (
    ThrowsError
  , LispError (..)
  ) where

import           Control.Monad.Error
-- TODO convert to Except
--import           Control.Monad.Except

import qualified Data.Text as T
import           Scheme.AST (unwordsList, LispVal)
import           Text.ParserCombinators.Parsec (ParseError)

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

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
