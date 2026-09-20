module Data.Parser where

import Control.Applicative
import Data.Char (isSpace)
import Data.List (uncons)

type Loc = Int

type Desc = String

type Input = (Loc, String)

newtype Parser a = Parser
  { runParser :: Input -> Either Desc (Input, a)
  }

instance Functor Parser where
  fmap f p =
    Parser $ \s -> do
      (s', x) <- runParser p s
      pure (s', f x)

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
  empty = Parser . const . Left $ "Empty Parser"
  (Parser p1) <|> (Parser p2) =
    Parser $ \s ->
      case (p1 s, p2 s) of
        (Right x, _) -> Right x
        (Left _, Right x) -> Right x
        (Left x, _) -> Left x

mapLeft :: (Desc -> Desc) -> Parser a -> Parser a
mapLeft f p =
  Parser $ \s ->
    case runParser p s of
      Left x -> Left $ f x
      Right x -> Right x

-- No proper error handling
char :: Char -> Parser Char
char c = charIf (== c) $ "char " ++ show c

charIf :: (Char -> Bool) -> Desc -> Parser Char
charIf p desc =
  Parser $ \(loc, s) -> do
    let desc' = "Expected " ++ desc ++ " at " ++ show loc
    (c, cs) <- maybe (Left $ desc' ++ ", reached end of input") Right (uncons s)
    if p c
      then pure ((loc + 1, cs), c)
      else Left $ desc' ++ ", got " ++ show c

string :: String -> Parser String
string s = mapLeft (++ " in string " ++ show s) (traverse char s)

ws :: Parser String
ws = many . charIf isSpace $ "whitespace"

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []
