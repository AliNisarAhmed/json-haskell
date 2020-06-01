module Main where

import Control.Applicative

main :: IO ()
main = undefined

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Eq, Show)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (Parser a) = Parser (fmap (fmap f) . a)
  -- fmap f (Parser p) =
  --   Parser $ \input -> do 
  --     (input', x) <- p input
  --     Just (input', f x)

instance Applicative Parser where
  pure x = Parser (\s -> Just (s, x))
  (<*>) (Parser p1)  (Parser p2) =
    Parser (\s -> do
        (s1, f) <- p1 s
        (s2, a) <- p2 s1
        return (s2, f a)
      )

instance Alternative Parser where
  empty = Parser (const Nothing)
  (<|>) (Parser p1) (Parser p2) =
    Parser (\s -> p1 s <|> p2 s)

charP :: Char -> Parser Char
charP c = Parser f
  where
    f (y:ys)
      | c == y = Just (ys, c)
    f _        = Nothing

stringP :: String -> Parser String
-- stringP = sequenceA . map charP
stringP = traverse charP

jsonNull :: Parser JsonValue
jsonNull = const JsonNull <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = convertToBool <$> (stringP "true" <|> stringP "false")
  where 
    convertToBool "true" = JsonBool True 
    convertToBool "false" = JsonBool False


-- The final whole JSON value parser
jsonValue :: Parser JsonValue
jsonValue = jsonBool <|> jsonNull