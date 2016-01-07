{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Scheme.RTS (
    Env
  , eval
  , trapError
  , runIOThrows
  , isBound
  , bindVars
  , getVar
  , setVar
  , liftThrows
  , primitiveBindings
  , nullEnv
  , ThrowsError
  , LispVal (..)
  ) where

import           Control.Monad
import           Control.Monad.Error
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import           Scheme.Data
import           Scheme.Parser (readExpr, readExprList)
import           System.IO

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

nullEnv :: IO Env
nullEnv = newIORef []

-- Is this a hoist?
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError Text -> IO Text
runIOThrows action = runErrorT (trapError action) >>= return . extractValue
  where
    extractValue (Right val) = val
    extractValue (Left _) = "error"

isBound :: Env -> Text -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (pure True) . lookup var

getVar :: Env -> Text -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" (T.unpack var))
                         (liftIO . readIORef)
                         (lookup var env)

setVar :: Env -> Text -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" (T.unpack var))
    (liftIO . (flip writeIORef value))
    (lookup var env)
  return value

defineVar :: Env -> Text -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(Text, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
       extendEnv bindings' env = liftM (++ env) (mapM addBinding bindings')
       addBinding (var, value) = do
         ref <- newIORef value
         return (var, ref)

makeFunc :: Monad m => Maybe Text -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc vargs' env params' body' = return $ Func (map showVal params') vargs' body' env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ErrorT LispError IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ErrorT LispError IO LispVal
makeVarArgs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom id') = getVar env id'
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred', conseq, alt]) =
     do result <- eval env pred'
        case result of
             Bool False -> eval env alt
             _  -> eval env conseq
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var

eval env (List (Atom "define" : List (Atom var : params') : body')) =
  makeNormalFunc env params' body' >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') vargs' : body')) =
  makeVarArgs vargs' env params' body' >>= defineVar env var
eval env (List (Atom "lambda" : List params' : body')) =
     makeNormalFunc env params' body'
eval env (List (Atom "lambda" : DottedList params' varargs : body')) =
     makeVarArgs varargs env params' body'
eval env (List (Atom "lambda" : varargs@(Atom _) : body')) =
     makeVarArgs varargs env [] body'

eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func )  args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params' vargs' body' closure') args =
  if num params' /= num args && vargs' == Nothing
  then throwError $ NumArgs (num params') args
  else (liftIO $ bindVars closure' $ zip params' args) >>= bindVarArgs vargs' >>= evalBody
  where
    remainingArgs = drop (length params') args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body'
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
      Nothing -> return env
apply a _ = throwError $ NotFunction "Not a function" (show a)

ioPrimitives :: [(T.Text, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

primitives :: [(T.Text, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc' IOFunc) ioPrimitives
                                ++ map (makeFunc' PrimitiveFunc) primitives)
  where makeFunc' constructor (var, func) = (var, constructor func)

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc [] = pure $ List []

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile (T.unpack filename) mode
makePort _ (a:_) = throwError $ TypeMismatch "Must pass string filename to makePort" a
makePort _ [] = throwError $ TypeMismatch "Must pass filename to makePort" (List [])

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ T.pack <$> hGetLine port) >>= liftThrows . readExpr
readProc (a:_) = throwError $ TypeMismatch "ReadProc expects a Port or []" a

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc (a:_) = throwError $ TypeMismatch "WriteProc unexpected type" a
writeProc [] = throwError $ TypeMismatch "WriteProc unexpected type" (List [])

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = do
  let c = liftIO $ readFile . T.unpack $ filename
  liftM String $ T.pack <$> c
readContents a = throwError $ NumArgs 1 a

load :: Text -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile . T.unpack $ filename) >>= liftThrows . readExprList . T.pack

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll a = throwError $ NumArgs 1 a

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _xs)]         = return x
car [DottedList (x : _xs) _] = return x
car [badArg]                 = throwError $ TypeMismatch "pair" badArg
car badArgList               = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_x : xs)]        = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                              (all eqvPair $ zip arg1 arg2)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left _err -> False
                                Right (Bool val) -> val
                                Right _ -> False

eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop  = boolBinop unpackNum

strBoolBinop :: (Text -> Text -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _op           []  = throwError $ NumArgs 2 []
numericBinop _op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params'        = mapM unpackNum params' >>= return . Number . foldl1 op

unpackStr :: LispVal -> ThrowsError Text
unpackStr (String s) = return $ s
unpackStr (Number s) = return . T.pack . show $ s
unpackStr (Bool s)   = return . T.pack . show $ s
unpackStr notText  = throwError $ TypeMismatch "string" notText

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads (T.unpack n) in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
  unpacked1 <- unpacker arg1
  unpacked2 <- unpacker arg2
  return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

trapError :: (Show a, MonadError a m) => m Text -> m Text
trapError action = catchError action (return . T.pack . show)
