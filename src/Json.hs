module Main where

import Control.Applicative
import Control.Monad (replicateM)
import Data.Char
import Data.Functor
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
  | JsonObject (Map.Map String JsonValue)
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
  s1 <- get
  mantissa <-
    case runParser (char '.') s1 of
      Right (s', _) -> put s' *> (read . ("0." ++) <$> digits')
      Left _ -> pure 0
  s2 <- get
  expo <-
    case runParser e s2 of
      Right (s2', _) ->
        put s2' *> liftA2 (*) (plus <|> minus <|> pure 1) (read <$> digits')
      Left _ -> pure 0
  pure $ sign * (nat + mantissa) * (10 ** expo)
  where
    digits = some (charIf isDigit "digit")
    digits' = id <#> digits $ 2
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
stringLiteral = char '"' *> (id <#> many (normalChar <|> escapeChar) $ 2) <* char '"'

parseString :: Parser JsonValue
parseString = JsonString <$> stringLiteral

parseArray :: Parser JsonValue
parseArray =
  JsonArray <$> do
    elements <- char '[' *> ws *> parseElements <* ws
    c <- charIf (const True) "']'"
    case c of
      ']' -> pure elements
      ',' -> throwP "Trailing comma" 3
      _ -> throwP ("Unexpected character " ++ show c ++ " at end of array") 3
  where
    parseElements = sepMany (ws *> char ',' <* ws) parseJson

parseObject :: Parser JsonValue
parseObject = do
  dict <- char '{' *> ws *> sepSome (ws *> char ',' <* ws) parsePair <* ws
  s <- get
  case runParser (char '}' <* ws) s of
    Right (s', _) -> put s' $> JsonObject (Map.fromList dict)
    Left _ -> char ',' *> parsePair *> empty -- impossible to succeed
  <|> (char '{' *> ws *> char '}') $> JsonObject Map.empty
  where
    parsePair = do
      key <- ("Invalid key: " ++) <#> (ws *> stringLiteral) $ 2
      _ <- id <#> (ws *> char ':' <* ws) $ 2
      value <- ("Invalid value in Object" ++) <#> parseJson $ 2
      pure (key, value)

parseJson :: Parser JsonValue
parseJson =
  parseNull
    <|> parseBool
    <|> parseString
    <|> parseNumber
    <|> parseArray
    <|> parseObject
    <|> throwP "Parse Error/Missing Value" 2

parseFile :: FilePath -> Parser a -> IO (Either Error (Input, a))
parseFile fileName parser = do
  input <- readFile fileName
  pure $ runParser parser ((0, 0), input)

getValue :: JsonValue -> [String] -> Either String JsonValue
getValue x [] = pure x
getValue (JsonObject ps) (k:ks)
  | Map.null ps = Left "Empty Object"
  | otherwise = do
    val <- maybe (Left "Key not found") Right (Map.lookup k ps)
    getValue val ks
getValue _ _ = Left "Non-Object"

main :: IO ()
main = undefined
