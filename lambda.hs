import Control.Applicative
import Data.Char
import Data.List

data Expression
  = Identifier String
  | Application Expression Expression
  | Lambda Expression Expression -- Where first expression is an identifier
  deriving (Eq)

instance Show Expression where
  show (Identifier s) = s
  show (Application (Identifier i1) (Identifier i2)) = i1 ++ " " ++ i2
  show (Application l@(Lambda _ _) e2) = "(" ++ show l ++ ")(" ++ show e2 ++ ")"
  show (Application e1 (Identifier i2)) = show e1 ++ " " ++ i2
  show (Application e1 e2) = show e1 ++ " (" ++ show e2 ++ ")"
  show (Lambda s e) = "λ" ++ show s ++ "." ++ show e

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f p =
    Parser $ \s -> do
      (s', x) <- runParser p s
      return (s', f x)

instance Applicative Parser where
  pure x = Parser $ \s -> return (s, x)
  pf <*> ps =
    Parser $ \s -> do
      (s', f) <- runParser pf s
      runParser (f <$> ps) s'

instance Monad Parser where
  p >>= f =
    Parser $ \s -> do
      (s', x) <- runParser p s
      runParser (f x) s'

instance MonadFail Parser where
  fail _ = empty

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s

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

notNull :: Parser [a] -> Parser [a]
notNull p =
  Parser $ \s -> do
    (s', xs) <- runParser p s
    if null xs
      then Nothing
      else return (s', xs)

ws :: Parser String
ws = many $ charIf isSpace

forceWs :: Parser String
forceWs = some $ charIf isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

identifier :: Parser Expression
identifier = do
  c <- charIf isAlpha
  cs <- many $ charIf isAlphaNum
  return $ Identifier (c : cs)

grouping :: Parser Expression
grouping = char '(' *> expression <* char ')'

application :: Parser Expression
application = do
  e <- notApplication
  forceWs
  xs <- sepBy forceWs notApplication
  return $ foldl Application e xs

unwrapId :: Expression -> String
unwrapId (Identifier i) = i
unwrapId _ = error "Must be an identifier"

lambda :: Parser Expression
lambda = do
  (char '\\' <|> char 'λ') >> ws
  is <- notNull $ sepBy forceWs identifier
  ws >> (string "->" <|> pure <$> char '.') >> ws
  e <- expression
  return $ foldr Lambda (Lambda (last is) e) (init is)

notApplication :: Parser Expression
notApplication = lambda <|> grouping <|> identifier

expression :: Parser Expression
expression = application <|> notApplication

replace :: String -> Expression -> Expression -> Expression
replace s e1 e2@(Identifier i) =
  if s == i
    then e1
    else e2
-- May duplicate indentifiers
replace s e1 (Application e2 e3) =
  Application (replace s e1 e2) (replace s e1 e3)
replace s e1 (Lambda i1 e2) = Lambda i1 (replace s e1 e2)

reduce :: Expression -> Expression
reduce (Application (Lambda (Identifier i) e1) e2) = replace i e2 e1
reduce x = x

apply e1 e2 = reduce (Application e1 e2)

applyM :: Expression -> [String] -> Expression
applyM e = foldl apply e . map Identifier
