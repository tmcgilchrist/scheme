{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Except
import           Control.Monad.Trans
import qualified Data.Text as T
import           Scheme.Parser
import           System.Environment
-- TODO Remove partial functions
-- TODO Use optparse-applicative

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ (liftM show) $ readExpr (T.pack $ args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
