{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Shared Megaparsec combinators for the DSL parsers.
module HQ.Parser where

import Relude hiding (many, some)
import Text.Megaparsec
import Text.Megaparsec.Char (char, spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser over 'Text' input with no custom error component.
type Parser = Parsec Void Text

-- | Whitespace between tokens.
sc :: Parser ()
sc = L.space (void spaceChar) empty empty

-- | Top-level entry point: consume leading whitespace, run @p@, require 'eof'.
parseTop :: String -> Parser a -> Text -> Either (ParseErrorBundle Text Void) a
parseTop name p = parse (sc *> p <* eof) name

-- | Symbol with trailing whitespace.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Lexeme with trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Keyword yielding a fixed value.
keyword :: Text -> a -> Parser a
keyword w v = symbol w $> v

-- | Parenthesized parser.
parens :: Parser a -> Parser a
parens p = lexeme (char '(') *> p <* lexeme (char ')')

-- | Bracketed parser.
brackets :: Parser a -> Parser a
brackets p = lexeme (char '[') *> p <* lexeme (char ']')

-- | Braced parser.
braces :: Parser a -> Parser a
braces p = lexeme (char '{') *> p <* lexeme (char '}')

-- | Comma separator with trailing whitespace.
comma :: Parser ()
comma = void $ lexeme (char ',')

-- | Colon separator with trailing whitespace.
colon :: Parser ()
colon = void $ lexeme (char ':')

-- | Comma-separated list.
commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` comma

-- | Left-associative chain: @p (op p)*@ folded strictly.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest <- many ((,) <$> op <*> p)
  pure $ foldl' (\acc (f, y) -> f acc y) x rest

-- | Dot-separated chain folded with the given combine function,
-- e.g. @dotChain opticAtomParser compose@.
dotChain :: Parser a -> (a -> a -> a) -> Parser a
dotChain p combine = chainl1 p (combine <$ symbol ".")
