module Hal.Types.REPL where

import System.IO

import Hal.Types.Primitives
import Hal.Parser
import Hal.Types.Exceptions
import Control.Monad.Except
import System.Exit (exitWith, ExitCode (ExitFailure))
import Data.Functor
import Hal.Types.Lib
import Hal.Types.Env
import Hal.Types.Env (LispVal(String))

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

extractValue :: ThrowsError a -> IO a
extractValue (Right val) = pure val
extractValue (Left err) = hPrint stderr err >> exitWith (ExitFailure 84)

-- evalString :: String -> IO String
-- evalString expr = extractValue . trapError $ show <$> (readExpr expr >>= eval)

-- evalAndPrint :: String -> IO ()
-- evalAndPrint expr = evalString expr >>= putStrLn

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ (liftThrows $ readExpr expr) >>= eval env <&> show

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

-- runRepl :: IO ()
-- runRepl = until_ quitCase (readPrompt "Lisp>>> ") evalAndPrint
--     where quitCase "quit" = True
--           quitCase (a:_) = undefined 
--           quitCase as = False

-- runRepl :: IO ()
-- runRepl = until_ (== ":q") (readPrompt "Lisp>>> ") evalAndPrint

-- runIOThrows :: IOThrowsError String -> IO String
-- runIOThrows action = runExceptT (trapError action) <&> extractValue

-- runIOThrows :: IOThrowsError String -> IO String
-- runIOThrows action = runErrorT (trapError action) >>= return . extractValue

runIOThrows :: IOThrowsError a -> IO a
runIOThrows = runExceptT >=> extractValue

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint