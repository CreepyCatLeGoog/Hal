module Main where

import System.Environment
import Control.Applicative
import Control.Monad
import Control.Exception
import System.Exit
import System.IO
import Text.Printf
import Data.Fixed
import Data.List
import Debug.Trace
import Control.Monad.Except

import Hal.Parser
import Hal.Types.Cons
import Hal.Types.Exceptions
import Hal.Types.Lib
import Hal.Types.Primitives
import Hal.Types.REPL
import Hal.Types.Env


parseFile :: Env -> String -> IO ()
parseFile env input = case runParser (many (parseExpr <* parseSpaces) <* parseEndFile) input of
    Right (res, []) -> evalFile env res
    Right (res, str') -> throwIO $ PartialParse str'
    Left err -> throwIO err

readFiles :: Env ->[FilePath] -> IO ()
readFiles env [] = throwIO BadSpecialForm
readFiles env [file] = parseFile env =<< readFile file
readFiles env (x:xs) = readFile x >>= parseFile env >> readFiles env xs

evalFile :: Env -> [LispVal] -> IO ()
evalFile env [] = throwIO BadSpecialForm
evalFile env [file] = runIOThrows (eval env file) >>= print 
evalFile env (x:xs) = runIOThrows (eval env x) >> evalFile env xs

main :: IO ()
main = do
          args <- getArgs
          env <- primitiveBindings
          case args of
               [] -> runRepl
               ["-i"] -> runRepl
               files -> readFiles env files


-- evalAndPrint :: Env -> String -> IO ()
-- evalAndPrint env expr =  evalString env expr >>= putStrLn

-- evalString :: Env -> String -> IO String
-- evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

-- runRepl :: IO ()
-- runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

-- main :: IO ()
-- main = do
--     args <- getArgs
--     case args of
--         [] -> pure ()
        -- files -> readFiles files >>= either (\e -> print (displayException e) >> exitWith (ExitFailure 84)) print
        -- case readExpr (readFile' files) of
        --     Just x -> print x
        --     Nothing -> exitWith (ExitFailure 84)

-- main :: IO ()
-- main = do
--           args <- getArgs
--           env <- primitiveBindings
--           case args of
--                [] -> runRepl
--                ["-i"] -> runRepl
--                files -> readFiles env files >>= print
            --    case readExpr (readFile' files) of
            --         Just x -> print x
            --         Nothing -> exitWith (ExitFailure 84)

-- main :: IO ()
-- main = do args <- getArgs
--           case length args of
--                0 -> runRepl
--                1 -> runOne $ args !! 0
--                otherwise -> putStrLn "Program takes only 0 or 1 argument"