{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Except
import           Control.Monad.Trans
import qualified Data.Text as T
import           Scheme
import           System.Environment
-- TODO Remove partial functions
-- TODO Use optparse-applicative

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne . T.pack $ args !! 0
    otherwise -> putStrLn "Program takes only 0 or 1 argument"
