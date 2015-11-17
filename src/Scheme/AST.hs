module Scheme.AST (
    LispVal (..)
  ) where

import Data.Text

data LispVal =
    Atom Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String Text
  | Bool Bool
