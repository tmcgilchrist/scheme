{-# LANGUAGE OverloadedStrings #-}
module Scheme (
    module X
  , runRepl
  , evalAndPrint
  ) where

import           Control.Monad
import           Control.Monad.Except
import qualified Data.Text as T
import           Scheme.AST as X
import           Scheme.Parser as X
import           Scheme.RTS as X
import           System.IO
import           Text.ParserCombinators.Parsec hiding (spaces)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return . T.unpack . extractValue . trapError $ (liftM (T.pack . show) $ (readExpr . T.pack $ expr) >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "λ >>> ") evalAndPrint
