{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Parser where

import Control.Applicative
import Data.Bifunctor
import Data.Char (isSpace)
import Data.List (uncons)

type Loc = (Int, Int)

data Error = Error
  { loc :: Loc
  , desc :: String
  , imp :: Int
  }

instance Show Error where
  show (Error (row, col) desc _) = "(" ++ show row ++ ", " ++ show col ++ "): " ++ desc

type Input = (Loc, String)

newtype Parser a = Parser
  { runParser :: Input -> Either Error (Input, a)
  }

instance Functor Parser where
  fmap f p = Parser (fmap (fmap f) . runParser p)

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
  empty = Parser . const . Left $ Error (0, 0) "Empty Parser" 0
  (Parser p1) <|> (Parser p2) =
    let choose (Right x) _ = Right x
        choose (Left _) (Right x) = Right x
        choose (Left x) (Left y)
          | imp x >= imp y = Left x
          | otherwise = Left y
     in Parser $ \s -> choose (p1 s) (p2 s)

-- Shoddiest Bifunctor implentation
mapErr :: (String -> String) -> Parser a -> Int -> Parser a
mapErr f p imp =
  let mapDesc f (Error loc desc _) = Error loc (f desc) imp
   in Parser $ first (mapDesc f) . runParser p

throwP :: String -> Int -> Parser a
throwP s imp = Parser $ \(loc, _) -> Left $ Error loc s imp

get :: Parser Input
get = Parser $ \s -> Right (s, s)

put :: Input -> Parser ()
put s = Parser $ \_ -> Right (s, ())

char :: Char -> Parser Char
char c = charIf (== c) $ "char " ++ show c

charIf :: (Char -> Bool) -> String -> Parser Char
charIf p pattern =
  Parser $ \(loc@(row, col), s) -> do
    let err = "Expected " ++ pattern
    (c, s') <-
      maybe (Left $ Error loc (err ++ ", reached end of input") 1) Right (uncons s)
    let next =
          if c == '\n'
            then (row + 1, 0)
            else (row, col + 1)
    if p c
      then pure ((next, s'), c)
      else Left $ Error loc (err ++ ", got " ++ show c) 1

string :: String -> Parser String
string s = mapErr (++ " in string " ++ show s) (traverse char s) 1

ws :: Parser String
ws = many . charIf isSpace $ "whitespace"

sepMany :: Parser a -> Parser b -> Parser [b]
sepMany sep element = sepSome sep element <|> pure []

sepSome :: Parser a -> Parser b -> Parser [b]
sepSome sep element = (:) <$> element <*> many (sep *> element)
