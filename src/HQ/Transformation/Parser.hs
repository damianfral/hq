{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Parser for the transformation DSL.
module HQ.Transformation.Parser where

import Data.Aeson (Value (..))
import Data.Scientific (Scientific)
import HQ.JSON.Parser (jsonArray, jsonNumber, jsonText, jsonValueParser)
import HQ.Parser (Parser, chainl1, dotChain, keyword, parens, parseTop, symbol)
import HQ.Transformation
import Relude hiding (many, not, or, some, subtract)
import Text.Megaparsec

-- | Parse a single transformation expression, e.g. @+1 . == 3@.
parseTransformation :: Text -> Either (ParseErrorBundle Text Void) Transformation
parseTransformation = parseTop "transformation" transformationParser

-- | Grammar of the DSL, loosest to tightest:
--
-- > or     := combine (('or' | '||') combine)*
-- > combine  := atom ('.' atom)*
-- > atom   := '+' number | '*' number | '-' number | '/' number
-- >        |  '++' string | 'concat' array | 'trim' | 'not'
-- >        |  'replace' string string | ('==' | '=') value
-- >        |  'const' value | '(' or ')'
--
-- JSON literals (numbers, strings and arrays) are parsed with the aeson
-- value parser from "HQ.JSON.Parser", so numbers are 'Scientific'.
transformationParser :: Parser Transformation
transformationParser = orParser

orParser :: Parser Transformation
orParser = chainl1 combineParser (or <$ (symbol "or" <|> symbol "||"))

combineParser :: Parser Transformation
combineParser = dotChain atomParser combine

atomParser :: Parser Transformation
atomParser =
  parenParser
    <|> constParser
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
parenParser = parens transformationParser

constParser :: Parser Transformation
constParser = constValue <$> (symbol "const" *> jsonValueParser)

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
trimParser = keyword "trim" trim

notParser :: Parser Transformation
notParser = keyword "not" not

replaceParser :: Parser Transformation
replaceParser = do
  void $ symbol "replace"
  replace <$> textOperand <*> textOperand

-- | A JSON number literal, e.g. @1@ or @0.5@.
numberOperand :: Parser Scientific
numberOperand = jsonNumber

-- | A JSON string literal, e.g. @"hello"@.
textOperand :: Parser Text
textOperand = jsonText

-- | A JSON array literal, e.g. @[1, "two"]@.
arrayOperand :: Parser [Value]
arrayOperand = jsonArray
