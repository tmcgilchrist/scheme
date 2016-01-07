{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import           Scheme
import           System.Environment
-- TODO Remove partial functions
-- TODO Use optparse-applicative

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne $ T.pack <$> args
