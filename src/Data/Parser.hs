module Data.Parser where

import Control.Applicative
import Data.Bifunctor
import Data.Char (isSpace)
import Data.List (uncons)

type Loc = (Int, Int)

type Error = (Loc, String)

type Input = (Loc, String)

newtype Parser a = Parser
  { runParser :: Input -> Either Error (Input, a)
  }

instance Functor Parser where
  fmap f p = Parser (second (second f) . runParser p)

-- More compact (horizontally) but harder to read and store values
instance Applicative Parser where
  pure x = Parser $ \s -> pure (s, x)
  Parser pf <*> ps =
    Parser $ \s -> do
      (s', f) <- pf s
      runParser (f <$> ps) s'

-- Less compact (vertically) but easier to read and store values
instance Monad Parser where
  Parser p >>= f =
    Parser $ \s -> do
      (s', x) <- p s
      runParser (f x) s'

instance Alternative Parser where
  empty = Parser . const . Left $ ((0, 0), "Empty Parser")
  (Parser p1) <|> (Parser p2) =
    Parser $ \s ->
      case (p1 s, p2 s) of
        (Right x, _) -> Right x
        (Left _, Right x) -> Right x
        (Left x, _) -> Left x

-- Shoddier Bifunctor implentation
mapErr :: (String -> String) -> Parser a -> Parser a
mapErr f p = Parser $ first (second f) . runParser p

throwP :: String -> Parser a
throwP s = Parser $ \(loc, _) -> Left (loc, s)

get :: Parser Input
get = Parser $ \s -> Right (s, s)

put :: Input -> a -> Parser a
put s x = Parser $ \_ -> Right (s, x)

char :: Char -> Parser Char
char c = charIf (== c) $ "char " ++ show c

charIf :: (Char -> Bool) -> String -> Parser Char
charIf p pattern =
  Parser $ \(loc@(row, col), s) -> do
    let err = "Expected " ++ pattern
    (c, s') <-
      maybe (Left (loc, err ++ ", reached end of input")) Right (uncons s)
    let next =
          if c == '\n'
            then (row + 1, 0)
            else (row, col + 1)
    if p c
      then pure ((next, s'), c)
      else Left (loc, err ++ ", got " ++ show c)

string :: String -> Parser String
string s = mapErr (++ " in string " ++ show s) (traverse char s)

ws :: Parser String
ws = many . charIf isSpace $ "whitespace"

sepMany :: Parser a -> Parser b -> Parser [b]
sepMany sep element = sepSome sep element <|> pure []

sepSome :: Parser a -> Parser b -> Parser [b]
sepSome sep element = (:) <$> element <*> many (sep *> element)
