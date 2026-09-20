module Data.Parser where

import Control.Applicative
import Data.Char (isSpace)
import Data.List (uncons)

type Loc = Int

type Desc = String

type Input = (Loc, String)

-- runParser acts as an unwrapper, shortens some expressions that would otherwise require patternmatching (Parser p) or return (rs, x) 
newtype Parser a = Parser
  { runParser :: Input -> Maybe (Input, a)
  }

instance Functor Parser where
  fmap f p =
    Parser $ \s -> do
      (s', x) <- runParser p s
      return (s', f x)

-- More compact (horizontally) but harder to read and store values
instance Applicative Parser where
  pure x = Parser $ \s -> Just (s, x)
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
  empty = Parser . const $ Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

-- No proper error handling
char :: Char -> Parser Char
char c = charIf (== c)

charIf :: (Char -> Bool) -> Parser Char
charIf p =
  Parser $ \(loc, s) -> do
    (c, cs) <- uncons s
    if p c
      then return ((loc + 1, cs), c)
      else Nothing

string :: String -> Parser String
string = traverse char

ws :: Parser String
ws = many $ charIf isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []
