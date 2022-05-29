module Hal.Types.Exceptions
where

import Control.Monad
import GHC.Generics (Constructor, Par1)
import GHC.Exception.Type (Exception(displayException))
import Control.Monad.Except
import Data.Functor

data LispError = NumArgs Integer
               | TypeMismatch
               | Parserr ParseError
               | BadSpecialForm
               | NotFunction String
               | UnboundVar String
               | Default String
data ParseError = PartialParse String
                | NoParse

instance Show LispError where
  show (UnboundVar varname) = "Unbound variable: " ++ varname
  show BadSpecialForm = "Bad special form"
  show (NotFunction func) = "Not a function: " ++ show func
  show (NumArgs expected) = "Unexpected Num found" ++ show expected
  show TypeMismatch = "Invalid type"
  show (Parserr parseErr) = "Parse error: " ++ show parseErr
  show (Default str) = "Default Error " ++ str

instance Exception LispError
instance Show ParseError where
    show NoParse = "cannot parse"
    show (PartialParse rest) = "cannot parse remaining: " ++ rest
instance Exception ParseError

type ThrowsError = Either LispError
type ThrowsParseError = Either ParseError