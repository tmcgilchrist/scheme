{-# LANGUAGE OverloadedStrings #-}
module Scheme (
    module X
  , runRepl
  , runOne
  , evalAndPrint
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Data.Text (Text)
import qualified Data.Text as T
import           Scheme.AST as X
import           Scheme.Data as X
import           Scheme.Parser as X
import           Scheme.RTS as X
import           System.IO
import           Text.ParserCombinators.Parsec hiding (spaces)

flushStr :: Text -> IO ()
flushStr str = (putStr . T.unpack $ str) >> hFlush stdout

readPrompt :: Text -> IO Text
readPrompt prompt = flushStr prompt >> fmap T.pack getLine

evalString :: Env -> Text -> IO Text
evalString env expr = runIOThrows $ liftM (T.pack . show) $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> Text -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn . T.unpack

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred' prompt action = do
  result <- prompt
  if pred' result
    then return ()
    else action result >> until_ pred' prompt action

runOne :: Text -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "λ >>> ") . evalAndPrint
