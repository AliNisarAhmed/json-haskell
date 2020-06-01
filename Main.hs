module Main where

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


charP :: Char -> Parser Char
charP c = Parser f
  where
    f (y:ys)
      | c == y = Just