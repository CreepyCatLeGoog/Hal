{-# LANGUAGE LambdaCase #-}
module Hal.Types.Primitives where


import Hal.Types.Lib
import Hal.Types.Exceptions
import Hal.Types.Cons
import Control.Monad.Except
import Data.Map.Strict as M
import Data.Function (on)
import Debug.Trace
import Hal.Types.Env
import Data.Maybe

-- primitives :: M.Map String ([LispVal] -> ThrowsError LispVal)
-- primitives :: [(String, [LispVal] -> LispVal)]
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("div", numericBinop (/))
  , ("mod", numericBinop pogMod)
  , ("<", numBoolBinop (<))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  ] where pogMod a b = fromIntegral $ round a `mod` round b

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args

apply (Func params varargs body closure) args =
    if num params /= num args && isNothing varargs
       then throwError $ NumArgs 9
       else liftIO (bindVars closure $ zip params args)
            >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = Prelude.drop (length params) args
          num = toInteger . length
          evalBody env = last <$> mapM (eval env) body
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env
-- apply (IOFunc func) args = func args
apply _ _ = undefined
-- boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
-- boolBinop unpacker op [a1, a2] = do
--    left <- unpacker a1
--    right <- unpacker a2
--    pure $ Bool $ left `op` right
-- boolBinop _ _ _ = throwError $ NumArgs 5


boolBinop :: (LispVal -> ThrowsError a) 
          -> (a -> a -> Bool) 
          -> [LispVal] 
          -> ThrowsError LispVal
boolBinop unpacker op [x,y] = do
    left <- unpacker x
    right <- unpacker y
    return $ Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 9

numBoolBinop  = boolBinop unpackNum

-- unpackNum :: LispVal -> ThrowsError Double
-- unpackNum (Number bee) = pure bee
-- unpackNum _ = throwError TypeMismatch


unpackNum :: LispVal -> ThrowsError Double
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch

-- numericBinop :: (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
-- numericBinop op []  = throwError $ NumArgs 6
-- numericBinop op [_] = throwError $ NumArgs 7
-- numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op


numericBinop :: (Double -> Double -> Double) 
             -> [LispVal] 
             -> ThrowsError LispVal
numericBinop op single@[_] = throwError $ NumArgs 50
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        Bool True -> eval env conseq
        otherwise  -> throwError $ TypeMismatch
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError BadSpecialForm

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ Prelude.map makePrimitiveFunc primitives)
     where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

makeFunc varargs env params body = return $ Func (Prelude.map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal