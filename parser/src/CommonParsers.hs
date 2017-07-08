module CommonParsers where

import Text.Parsec (ParsecT, many, oneOf)
import Control.Monad.Identity

type Parser a = ParsecT String () Identity a

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"
