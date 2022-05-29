module Hal.Types.Env where

import Hal.Types.Exceptions
import Data.IORef
import Control.Monad.Except
import Data.Functor

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO
type Env = IORef [(String, IORef LispVal)]

-- data LispVal = Atom String
--              | List [LispVal]
--              | DottedList [LispVal] LispVal
--              | Number Double
--              | String String
--              | Bool Bool
--              | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
--              | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env }
--              | IOFunc ([LispVal] -> IOThrowsError LispVal)


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Double
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String]
                    , vararg :: Maybe String
                    , body :: [LispVal]
                    , closure :: Env
                    }
--             | IOFunc ([LispVal] -> IOThrowsError LispVal)

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"

showVal (Func {params=args, vararg=varargs, body=body, closure=env}) =
    "(lambda (" ++ unwords (fmap show args) ++
      (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"

-- showVal (IOFunc _) = "<IO primitive>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . fmap showVal

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)
