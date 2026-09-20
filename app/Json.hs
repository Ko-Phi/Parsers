module Main where

import Control.Applicative
import Control.Monad (replicateM)
import Data.Char
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Parser
import Numeric (readHex)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Double
  | JsonArray [JsonValue]
  | JsonObject (Map.Map String JsonValue) -- Less efficient than Data.Map
  deriving (Eq)

instance Show JsonValue where
  show JsonNull = "null"
  show (JsonBool b) = show b
  show (JsonString s) = show s
  show (JsonNumber n) = show n
  show (JsonArray xs) = "[" ++ intercalate ", " (map show xs) ++ "]"
  show (JsonObject ps) =
    "{"
      ++ intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) (Map.toList ps))
      ++ "}"

parseNull :: Parser JsonValue
parseNull = JsonNull <$ string "null"

parseBool :: Parser JsonValue
parseBool = JsonBool <$> (True <$ string "true" <|> False <$ string "false")

parseDouble :: Parser Double
parseDouble = do
  sign <- minus <|> pure 1
  nat <- read <$> digits
  mantissa <- read . ('0' :) <$> liftA2 (:) (char '.') digits <|> pure 0
  expo <-
    e *> liftA2 (*) (plus <|> minus <|> pure 1) (read <$> digits) <|> pure 0
  pure $ sign * (nat + mantissa) * (10 ** expo)
  where
    digits = some (charIf isDigit "Expected digit")
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
      chr . fst . head . readHex <$> replicateM 4 (charIf isHexDigit "hexcode")

normalChar :: Parser Char
normalChar = charIf (liftA2 (&&) (/= '"') (/= '\\')) "non-quotation / escape"

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
  _ <- char '{' *> ws
  dict <- sepBy (ws *> char ',' <* ws) parsePair
  _ <- ws <* char '}'
  pure . JsonObject . Map.fromList $ dict
  where
    parsePair = do
      key <- stringLiteral
      _ <- ws *> char ':' <* ws
      value <- parseJson
      pure (key, value)

parseJson :: Parser JsonValue
parseJson =
  parseNull
    <|> parseBool
    <|> parseString
    <|> parseNumber
    <|> parseArray
    <|> parseObject

parseFile :: FilePath -> Parser a -> IO (Either String a)
parseFile fileName parser = do
  input <- readFile fileName
  pure $ snd <$> runParser parser (0, input)

getValue :: JsonValue -> [String] -> Maybe JsonValue
getValue x [] = Just x
getValue (JsonObject ps) (k:ks)
  | Map.null ps = Nothing
  | otherwise = do
    val <- Map.lookup k ps
    getValue val ks
getValue _ _ = Nothing

main :: IO ()
main = undefined
