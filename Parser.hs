module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser (\s ->
    case p s of
      Just (x, rest) -> Just (f x, rest)
      Nothing -> Nothing)
        

instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  (Parser p1) <*> (Parser p2) = Parser (\s -> do
    (f, rest1) <- p1(s)
    (x, rest2) <- p2(rest1)
    return (f x, rest2))

instance Monad Parser where
  return x = pure x
  
  (Parser p) >>= f = Parser (\s -> 
    case p s of
    Nothing -> Nothing
    Just (x, rest) -> parse (f x) rest)

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser (\s -> p1 s <|> p2 s)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser (\s -> case s of
  (x:xs) | predicate x -> Just (x, xs)
  _ -> Nothing)

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

spaces :: Parser String
spaces = many (satisfy (== ' '))

variable :: Parser String
variable = some (satisfy (`elem` ['a'..'z']))

parseVar :: Parser Lambda
parseVar = Var <$> variable

parseAbs :: Parser Lambda
parseAbs = do
  char '\\'
  v <- variable
  char '.'
  e <- parseLambdaExpr
  return (Abs v e)

parseApp :: Parser Lambda
parseApp = do
  char '('
  e1 <- parseLambdaExpr
  spaces
  e2 <- parseLambdaExpr
  char ')'
  return (App e1 e2)

parseMacro :: Parser Lambda
parseMacro = Macro <$> macro
  
parseLambdaExpr :: Parser Lambda
parseLambdaExpr = parseVar <|> parseAbs <|> parseApp <|> parseMacro

parseLambda :: String -> Lambda
parseLambda s = case parse parseLambdaExpr s of
  Just (result, "") -> result
  _ -> error "Failed to parse lambda expression"

macro :: Parser String
macro = some (satisfy (`elem` (['A'..'Z'] ++ ['0'..'9'])))

parseBinding :: Parser Line
parseBinding = do
  name <- macro
  spaces
  char '='
  spaces
  expr <- parseLambdaExpr
  return (Binding name expr)

parseEval :: Parser Line
parseEval = Eval <$> parseLambdaExpr

parseLine :: String -> Either String Line
parseLine s = case parse (spaces *> (parseBinding <|> parseEval) <* spaces) s of
  Just (result, "") -> Right result
  _ -> Left "Failed to parse line"
