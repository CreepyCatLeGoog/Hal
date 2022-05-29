{-# LANGUAGE LambdaCase #-}
module Hal.Types.Cons
where

import Hal.Types.Lib
import Hal.Types.Exceptions
import Control.Monad.Except
import Hal.Types.Env


car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError TypeMismatch
car badArgList              = throwError $ NumArgs 1

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError TypeMismatch
cdr badArgList              = throwError $ NumArgs 8

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 3

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]             =
    return $ Bool $ length arg1 == length arg2 && all eqvPair (zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
            Right (Bool v') -> v'
            _ -> False
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2

equal :: [LispVal] -> ThrowsError LispVal
equal [Number n, Number m] = pure $ Bool $ n == m
equal [String n, String m] = pure $ Bool $ n == m
equal [Bool n, Bool m] = pure $ Bool $ n == m
equal [_, _] = throwError TypeMismatch 
equal badArgList = throwError $ NumArgs 2