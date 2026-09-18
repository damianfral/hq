{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Parser for the transformation DSL.
module HQ.Transformation.Parser where

import Data.Aeson (Value (..))
import Data.Scientific (Scientific)
import qualified Data.Vector as V
import HQ.JSON.Parser (Parser, jsonValueParser)
import HQ.Transformation
import Relude hiding (many, not, or, some, subtract)
import Text.Megaparsec
import Text.Megaparsec.Char

-- | Parse a single transformation expression, e.g. @+1 . == 3@.
parseTransformation :: Text -> Either (ParseErrorBundle Text Void) Transformation
parseTransformation =
  parse (spaceConsumer *> transformationParser <* eof) "transformation"

-- | Grammar of the DSL, loosest to tightest:
--
-- > or     := combine (('or' | '||') combine)*
-- > combine  := atom ('.' atom)*
-- > atom   := '+' number | '*' number | '-' number | '/' number
-- >        |  '++' string | 'concat' array | 'trim' | 'not'
-- >        |  'replace' string string | ('==' | '=') value
-- >        |  '(' or ')'
--
-- JSON literals (numbers, strings and arrays) are parsed with the aeson
-- value parser from "HQ.JSON.Parser", so numbers are 'Scientific'.
transformationParser :: Parser Transformation
transformationParser = orParser

orParser :: Parser Transformation
orParser = do
  t0 <- combineParser
  rest <- many ((symbol "or" <|> symbol "||") *> combineParser)
  pure $ foldl' or t0 rest

combineParser :: Parser Transformation
combineParser =
  foldl' combine <$> atomParser <*> many (symbol "." *> atomParser)

atomParser :: Parser Transformation
atomParser =
  parenParser
    <|> equalParser
    <|> try strConcatParser
    <|> addParser
    <|> multiplyParser
    <|> subtractParser
    <|> divideParser
    <|> arrayConcatParser
    <|> trimParser
    <|> notParser
    <|> replaceParser

parenParser :: Parser Transformation
parenParser = lexeme (char '(') *> transformationParser <* lexeme (char ')')

equalParser :: Parser Transformation
equalParser = do
  void $ try (symbol "==") <|> symbol "="
  equal <$> jsonValueParser

addParser :: Parser Transformation
addParser = add <$> (symbol "+" *> numberOperand)

multiplyParser :: Parser Transformation
multiplyParser = multiply <$> (symbol "*" *> numberOperand)

subtractParser :: Parser Transformation
subtractParser = subtract <$> (symbol "-" *> numberOperand)

divideParser :: Parser Transformation
divideParser = divide <$> (symbol "/" *> numberOperand)

strConcatParser :: Parser Transformation
strConcatParser = concatString <$> (symbol "++" *> textOperand)

arrayConcatParser :: Parser Transformation
arrayConcatParser = symbol "concat" *> (concatArray <$> arrayOperand)

trimParser :: Parser Transformation
trimParser = symbol "trim" $> trim

notParser :: Parser Transformation
notParser = symbol "not" $> not

replaceParser :: Parser Transformation
replaceParser = do
  void $ symbol "replace"
  replace <$> textOperand <*> textOperand

-- | A JSON number literal, e.g. @1@ or @0.5@.
numberOperand :: Parser Scientific
numberOperand = do
  value <- jsonValueParser
  case value of
    Number n -> pure n
    _ -> fail "expected a JSON number"

-- | A JSON string literal, e.g. @"hello"@.
textOperand :: Parser Text
textOperand = do
  value <- jsonValueParser
  case value of
    String t -> pure t
    _ -> fail "expected a JSON string"

-- | A JSON array literal, e.g. @[1, "two"]@.
arrayOperand :: Parser [Value]
arrayOperand = do
  value <- jsonValueParser
  case value of
    Array items -> pure (V.toList items)
    _ -> fail "expected a JSON array"

spaceConsumer :: Parser ()
spaceConsumer = skipMany spaceChar

symbol :: Text -> Parser Text
symbol = lexeme . string

lexeme :: Parser a -> Parser a
lexeme p = p <* spaceConsumer
