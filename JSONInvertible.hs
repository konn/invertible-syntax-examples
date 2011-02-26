{-# LANGUAGE TemplateHaskell #-}
module JSONInvertible (json, parse) where
import Control.Monad
import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.TH
import Control.Isomorphism.Partial.Unsafe
import Text.Syntax
import Text.Syntax.Parser.Naive
import Text.Syntax.Printer.Naive
import Data.Char
import Prelude hiding (null, (.))
import Control.Category
import Data.Maybe

import JSON

defineIsomorphisms ''JSON

json, jsobj, jsarray, jsstring, jsnull, jsbool, jsnumber :: Syntax f => f JSON

json = jsobj <|> jsarray <|> jsstring <|> jsnull <|> jsbool <|> jsnumber
jsobj = object <$> between (symbol "{") (symbol "}") (term `sepBy` symbol ",")
  where
    term = str <* skipSpace' <* text ":" <* optSpace' <*> json
jsarray = array <$> between (symbol "[") (symbol "]") (json `sepBy` symbol ",")
jsstring = string <$> str
jsnull = null <$> symbol "null"
jsbool = bool . element True  <$> symbol "true"
     <|> bool . element False <$> symbol "false"
jsnumber = number . Iso read' show' <$> many1 digit
  where
    read' = liftM fst . listToMaybe . reads
    show' = return . show

str :: Syntax f => f String
str =  between (optSpace' *> text "\"") (text "\"" <* optSpace')
               (many (subset (/='"') <$> token))

digit :: Syntax f => f Char
digit = subset isDigit <$> token

symbol :: Syntax f => String -> f ()
symbol = between skipSpace' skipSpace' . text

skipSpace', optSpace' :: Syntax delta => delta ()
skipSpace' = ignore [] <$> many (text " " <|> text "\n")
optSpace' =  ignore [()] <$> many (text " " <|> text "\n")
