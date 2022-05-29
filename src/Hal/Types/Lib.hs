module Hal.Types.Lib
where

import Data.Maybe
import Data.Map.Strict ( lookup )
import Hal.Types.Exceptions
    ( ThrowsError,
      LispError(TypeMismatch, NotFunction, BadSpecialForm, NumArgs) )
import Prelude hiding (map, lookup)
import Control.Monad.Except
import Data.IORef
import Hal.Types.Env

-- eval :: LispVal -> Maybe LispVal
-- eval val@(String _) = Just val
-- eval val@(Number _) = Just val
-- eval val@(Bool _) = Just val
-- eval (List [Atom "quote", val]) = Just val
-- eval (List (Atom func : args)) = Just $ apply func $ mapMaybe eval args
-- eval _ = Nothing


-- numBoolBinop op [Number a ,Number b] = Bool $ op a b
-- numBoolBinop _ _ = throwError $ NumArgs 2


-- numBoolBinop  = boolBinop unpackNum
-- strBoolBinop  = boolBinop unpackStr
-- boolBoolBinop = boolBinop unpackBool

-- unwordsList :: [LispVal] -> String
-- unwordsList = unwords . fmap showVal

-- instance Show LispVal where
--   show (String s) = "\"" ++ s ++ "\""
--   show (Atom name) = name
--   show (Number n) = show n
--   show (Bool True) = "#t"
--   show (Bool False) = "#f"
--   show (List xs) = "(" ++ unwordsList xs ++ ")"
--   show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
--   show (PrimitiveFunc _) = "<primitive>"
--   show (Func {params = args, vararg = varargs, body = body, closure = env}) =
--    "(lambda (" ++ unwords (fmap show args) ++
--       (case varargs of
--          Nothing -> ""
--          Just arg -> " . " ++ arg) ++ ") ...)"



-- showVal :: LispVal -> String
-- showVal (String s) = "\"" ++ s ++ "\""
-- showVal (Atom name) = name
-- showVal (Number n) = show n
-- showVal (Bool True) = "#t"
-- showVal (Bool False) = "#f"
-- showVal (List xs) = "(" ++ unwordsList xs ++ ")"
-- showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
-- showVal (PrimitiveFunc _) = "<primitive>"

-- showVal (Func {params=args, vararg=varargs, body=body, closure=env}) =
--     "(lambda (" ++ unwords (fmap show args) ++
--       (case varargs of
--             Nothing -> ""
--             Just arg -> " . " ++ arg) ++ ") ...)"

-- showVal (IOFunc _) = "<IO primitive>"