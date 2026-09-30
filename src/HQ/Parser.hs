{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Parser where

import Relude hiding (many, some)
import Text.Megaparsec
import Text.Megaparsec.Char (char, spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

parseTop :: String -> Parser a -> Text -> Either (ParseErrorBundle Text Void) a
parseTop name p = parse (sc *> p <* eof) name

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

keyword :: Text -> a -> Parser a
keyword w v = symbol w $> v

parens :: Parser a -> Parser a
parens p = lexeme (char '(') *> p <* lexeme (char ')')

brackets :: Parser a -> Parser a
brackets p = lexeme (char '[') *> p <* lexeme (char ']')

braces :: Parser a -> Parser a
braces p = lexeme (char '{') *> p <* lexeme (char '}')

comma :: Parser ()
comma = void $ lexeme (char ',')

colon :: Parser ()
colon = void $ lexeme (char ':')

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` comma

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest <- many ((,) <$> op <*> p)
  pure $ foldl' (\acc (f, y) -> f acc y) x rest

dotChain :: Parser a -> (a -> a -> a) -> Parser a
dotChain p combine = chainl1 p (combine <$ symbol ".")
