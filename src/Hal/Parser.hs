{-# LANGUAGE LambdaCase #-}
module Hal.Parser
where
import Control.Applicative
import System.IO (BufferMode (NoBuffering))
import Data.Char
import Hal.Types.Env
import Hal.Types.Exceptions
import Control.Monad.Except
import Text.Read (readMaybe)

newtype Parser a = Parser {
    runParser :: String -> Either ParseError (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser parse -- <$>
        where
            parse str = case runParser parser str of
                Left err -> Left err
                Right (a, str') -> Right (fct a, str')

instance Applicative Parser where
    pure x = Parser parse
        where
            parse str = Right (x, str)
    liftA2 f p1 p2 = Parser parse
        where
            parse str = case runParser p1 str of
                Left err -> Left err
                Right (j, str) -> case runParser p2 str of
                    Left err -> Left err
                    Right (j', str) -> Right (f j j', str)

instance Alternative Parser where
    empty = Parser $ const (Left NoParse)
    p1 <|> p2 = Parser $ \xs -> case runParser p1 xs of
        Right j -> Right j
        Left  _ -> runParser p2 xs

instance Monad Parser where
    p1 >>= p2 = Parser $ \str -> case runParser p1 str of
        Left err -> Left err
        Right (a, str') -> runParser (p2 a) str'

parseChar :: Char -> Parser Char
parseChar a = Parser parse
    where 
        parse :: [Char] -> Either ParseError (Char, String)
        parse [] = Left NoParse 
        parse (x:xs)
            | x == a = Right (x, xs)
            | otherwise = Left $ PartialParse xs

parseAnyChar :: String -> Parser Char
parseAnyChar [] = empty
parseAnyChar (x:xs) = Parser parse
    where
        parse :: [Char] -> Either ParseError (Char, String)
        parse [] = Left NoParse 
        parse s = case runParser (parseChar x) s of
            Right x -> Right x
            Left _ -> runParser (parseAnyChar xs) s

-- parseAnd :: Parser a -> Parser b -> Parser (a,b)
-- parseAnd p1 p2 = Parser parse
--     where 
--         parse xs = case runParser p1 xs of
--             Nothing -> Nothing
--             Just (j, xs) -> case runParser p2 xs of
--                 Nothing -> Nothing
--                 Just (j', xs) -> Just ((j, j'), xs)

parseNumber :: Parser LispVal
parseNumber = do
    s <- parseInt
    s' <- maybe "" ('.':) <$> optional (parseChar '.' *> some parseDigit)
    case readMaybe (show s++s') of
        Just num -> pure $ Number num
        Nothing -> empty

parseDigit :: Parser Char
parseDigit = parseAnyChar "1234567890" 

parseUInt :: Parser Int
parseUInt = Parser parse
    where 
        parse str = case runParser (some parseDigit) str of
            Right(n, str) -> Right(read n, str)
            Left err -> Left NoParse

parseInt :: Parser Int
parseInt =  liftA2 sign (many $ parseAnyChar "+-") parseUInt
    where
        sign [] num = num
        sign ('-':xs) num = negate $ sign xs num
        sign (_:xs) num = sign xs num

removeBlanks :: String -> String
removeBlanks = filter (not . isSpace)

parseString :: String -> Parser String
parseString [] = pure []
parseString (x:xs) = (:) <$> parseChar x <*> parseString xs

parseIdentity :: a -> Parser a
parseIdentity a = Parser parse
    where
        parse str = Right (a, str)

space :: Parser Char
space = parseAnyChar " \n\r\t"

spaces :: Parser String
spaces = many space

symbol :: Parser Char
symbol = parseAnyChar "!#$%&|*+-/:<=>?@^_~"

letter :: Parser Char
letter = parseAnyChar "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseSpace :: Parser Char
parseSpace = parseAnyChar " \t\r\n"

parseSpaces :: Parser String
parseSpaces = many parseSpace

endBy :: Parser a -> Parser b -> Parser [a]
endBy a b = many (a <* b)

parseSepBy1 :: Parser a -> Parser sep -> Parser [a]
parseSepBy1 p sep = (:) <$> p <*> many (sep *> p)

parseSepBy :: Parser a -> Parser sep -> Parser [a]
parseSepBy p sep = parseSepBy1 p sep <|> pure []

getAnyChar :: Parser Char
getAnyChar = Parser $ \case
    (x:xs) -> Right (x, xs)
    [] -> Left NoParse

parseEndFile :: Parser ()
parseEndFile = Parser $ \s -> case runParser getAnyChar s of
    Left _ -> Right ((), "")
    Right _ -> Left NoParse 


parseLString :: Parser LispVal
parseLString = do
                parseChar '"'
                x <- many $ symbol <|> letter <|> space <|> parseDigit 
                parseChar '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> parseDigit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseList :: Parser LispVal
parseList = List <$> parseSepBy parseExpr parseSpaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr parseSpaces
    tail <- parseChar '.' *> spaces *> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    parseChar '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseNumber
             <|> parseAtom
             <|> parseLString
             <|> parseQuoted
             <|> do parseChar '(' *> parseSpaces
                    x <- parseDottedList <|> parseList
                    parseSpaces *> parseChar ')'
                    return x

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case runParser parser input of
    Left err -> throwError $ Parserr err
    Right (val, "") -> return val
    Right (val, a) -> throwError $ Parserr $ PartialParse a

-- readExpr :: String -> ThrowsError LispVal
-- readExpr input = case runParserE parseExpr input of
--      Left err -> trapError $ ParserE err
--      Right val -> return val