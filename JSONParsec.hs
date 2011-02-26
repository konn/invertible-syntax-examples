module JSONParsec (parseJSON) where
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), empty, many)

import JSON

parseJSON = either (error.show) id . parse json ""

json, jsobj, jsarray, jsstring, jsnull, jsbool, jsnumber :: Parser JSON
json = jsobj <|> jsarray <|> jsstring
   <|> jsnull <|> jsbool <|> jsnumber
jsobj =
  Object <$> between (symbol "{") (symbol "}") (term `sepBy` symbol ",")
  where
    term = (,) <$> str <* symbol ":" <*> json
jsarray = Array <$> between (symbol "[") (symbol "]") (json `sepBy` symbol ",")
jsstring = String <$> str
jsnull = Null <$  symbol "null"
jsbool = Bool True  <$ symbol "true"
     <|> Bool False <$ symbol "false"
jsnumber = Number . read <$> many1 digit

str =  between (spaces *> char '"') (char '"' <* spaces)
               (many $ noneOf "\"")

symbol :: String -> Parser String
symbol str = spaces *> string str <* spaces
