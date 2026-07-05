module Main where

import Control.Applicative
import Control.Monad (replicateM)
import Data.Char
import Data.List
import Numeric

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Double
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)] -- Less efficient than Data.Map
  | JsonTuple (JsonValue, JsonValue)
  deriving (Eq)

instance Show JsonValue where
  show JsonNull = "null"
  show (JsonBool b) = show b
  show (JsonString s) = show s
  show (JsonNumber n) = show n
  show (JsonArray xs) = "[" ++ intercalate ", " (map show xs) ++ "]"
  show (JsonObject ps) =
    "{" ++ intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) ps) ++ "}"

-- No proper error handling
-- runParser acts as an unwrapper, shortens some expressions that would otherwise require patternmatching (Parser p) or return (rs, x) 
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
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
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

char :: Char -> Parser Char
char c = charIf (== c)

charIf :: (Char -> Bool) -> Parser Char
charIf p =
  Parser $ \s -> do
    (c, cs) <- uncons s
    if p c
      then return (cs, c)
      else Nothing

string :: String -> Parser String
string = traverse char

ws :: Parser String
ws = many $ charIf isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

parseNull :: Parser JsonValue
parseNull = JsonNull <$ string "null"

parseBool :: Parser JsonValue
parseBool = JsonBool <$> (True <$ string "true" <|> True <$ string "false")

parseDouble :: Parser Double
parseDouble = do
  sign <- minus <|> pure 1
  int <- read <$> digits
  dec <- read . ('0' :) <$> liftA2 (:) (char '.') digits <|> pure 0
  expo <-
    e *> liftA2 (*) (plus <|> minus <|> pure 1) (read <$> digits) <|> pure 0
  return $ fromIntegral sign * (fromIntegral int + dec) * (10 ^^ expo)
  where
    digits = some (charIf isDigit)
    e = char 'e' <|> char 'E'
    plus = 1 <$ char '+'
    minus = -1 <$ char '-'

parseNumber :: Parser JsonValue
parseNumber = JsonNumber <$> parseDouble

escapeChar :: Parser Char
escapeChar =
  ('"' <$ string "\\\"")
    <|> ('\\' <$ string "\\\\")
    <|> ('/' <$ string "\\/")
    <|> ('\b' <$ string "\\b")
    <|> ('\f' <$ string "\\f")
    <|> ('\n' <$ string "\\n")
    <|> ('\r' <$ string "\\r")
    <|> ('\t' <$ string "\\t")
    <|> (string "\\u" *> escapeUnicode)
  where
    escapeUnicode =
      chr . fst . head . readHex <$> replicateM 4 (charIf isHexDigit)

normalChar :: Parser Char
normalChar = charIf (liftA2 (&&) (/= '"') (/= '\\'))

stringLiteral :: Parser String
stringLiteral = char '"' *> many (normalChar <|> escapeChar) <* char '"'

parseString :: Parser JsonValue
parseString = JsonString <$> stringLiteral

parseArray :: Parser JsonValue
parseArray = JsonArray <$> (char '[' *> ws *> parseElements <* ws <* char ']')
  where
    parseElements = sepBy (ws *> char ',' <* ws) parseJson

parseObject :: Parser JsonValue
parseObject = do
  char '{' *> ws
  dict <- sepBy (ws *> char ',' <* ws) parsePair
  ws <* char '}'
  return $ JsonObject dict
  where
    parsePair = do
      key <- stringLiteral
      ws *> char ':' <* ws
      value <- parseJson
      return (key, value)

parseJson :: Parser JsonValue
parseJson =
  parseNull
    <|> parseBool
    <|> parseString
    <|> parseNumber
    <|> parseArray
    <|> parseObject

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  return $ snd <$> runParser parser input

getValue :: JsonValue -> [String] -> Maybe JsonValue
getValue (JsonObject []) _ = Nothing
getValue (JsonObject (x:xs)) keys@(k:ks) =
  if fst x == k
    then getValue (snd x) ks
    else getValue (JsonObject xs) keys
getValue x [] = Just x
getValue _ _ = Nothing

main :: IO ()
main = undefined
