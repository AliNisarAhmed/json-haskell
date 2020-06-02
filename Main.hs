module Main where

import Control.Applicative (Alternative(..))
import Data.Char (isDigit, isSpace)

main :: IO ()
main = undefined

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
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


-------------------------------------------

-- Parsers --

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

-- based on the function span in std lib
spanP :: (Char -> Bool) -> Parser [Char]
spanP f =
  Parser $ \s ->
    let (token, rest) = span f s
    in Just (rest, token)

-- makes sure that the input to a parser is not an empty string
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \s -> do
    (s2, xs) <- p s
    if null xs
      then Nothing
      else Just (s2, xs)

jsonNumber :: Parser JsonValue
jsonNumber = (JsonNumber . read) <$> notNull (spanP isDigit)


-- JSON Strings start and end with a quote
-- *> :: f a -> f b -> f b
-- <* :: f a -> f b <- f a
-- these "apply" the Applicatives, but return only the indicated result
jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *>
                           elements
                           <* ws <* charP ']')
  where
    elements = sepBy (ws *> charP ',' <* ws) jsonValue


ws :: Parser String
ws = spanP isSpace


-- many :: Alternative f => f a -> f [a]
-- what we are doing here is taking a parser of single b
  -- and turning it into a parser of list of b
  -- using the many function from Alternative Typeclass
  -- more correctly, turning a parser of element-sep-element
  -- into a parser of element-sep-element-sep-element...
-- example
-- runParser (charP 'b') "bbb"
  -- >>> Just ("bb", 'b')
-- runParser (many $ charP 'b') "bbb"
  -- >>> Just ("", "bbb")
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element =
  (:) <$> element <*> many (sep *> element) <|> pure []


jsonObject :: Parser JsonValue
jsonObject =
  JsonObject <$>
    ( charP '{' *> ws *>
      sepBy (ws *> charP ',' <* ws) pair
      <* ws <* charP '}'
    )
    where
      pair =
        (\key _ value -> (key, value)) <$> stringLiteral
                                       <*> (ws *> charP ':' <* ws)
                                       <*> jsonValue

-- The final whole JSON value parser
jsonValue :: Parser JsonValue
jsonValue =
  jsonBool <|> jsonNull <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject