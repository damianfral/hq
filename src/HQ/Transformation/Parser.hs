{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.Parser where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Aeson (Value (..))
import Data.Scientific (Scientific)
import HQ.JSON.Parser (jsonArray, jsonNumber, jsonText, jsonValueParser)
import HQ.Parser (Parser, keyword, parens, parseTop, symbol)
import HQ.Transformation
import Relude hiding (and, isPrefixOf, length, many, not, or, reverse, some, subtract, xor)
import Text.Megaparsec

-- | Parse a single transformation expression, e.g. @+1 . == 3@.
parseTransformation :: Text -> Either (ParseErrorBundle Text Void) Transformation
parseTransformation = parseTop "transformation" transformationParser

-- | Grammar of the DSL, loosest to tightest:
--
-- > or     := xor (('or' | '||') xor)*
-- > xor    := and (('xor' | '^^') and)*
-- > and    := compose (('and' | '&&') compose)*
-- > compose  := atom ('.' atom)*
-- > atom   := '+' number | '*' number | '-' number | '/' number
-- >        |  '++' string | 'concat' array | 'trim' | 'not'
-- >        |  'replace' string string | 'stripPrefix' string | 'stripSuffix' string
-- >        |  'isPrefixOf' string | 'isSuffixOf' string | 'isInfixOf' string
-- >        |  'isEmpty' | 'length' | 'reverse' | 'unique'
-- >        |  ('==' | '=') value
-- >        |  'const' value | '(' or ')'
--
-- JSON literals (numbers, strings and arrays) are parsed with the aeson
-- value parser from "HQ.JSON.Parser", so numbers are 'Scientific'.
-- Precedence, tightest to loosest: atoms, @.@, @and@, @xor@, @or@.
transformationParser :: Parser Transformation
transformationParser = makeExprParser atomParser table
  where
    table =
      [ [InfixL (compose <$ symbol ".")],
        [InfixL (and <$ (symbol "and" <|> symbol "&&"))],
        [InfixL (xor <$ (symbol "xor" <|> symbol "^^"))],
        [InfixL (or <$ (symbol "or" <|> symbol "||"))]
      ]

atomParser :: Parser Transformation
atomParser =
  choice
    [ parenParser,
      constParser,
      equalParser,
      try strConcatParser,
      addParser,
      multiplyParser,
      subtractParser,
      divideParser,
      arrayConcatParser,
      trimParser,
      notParser,
      replaceParser,
      stripPrefixParser,
      stripSuffixParser,
      isPrefixOfParser,
      isSuffixOfParser,
      isInfixOfParser,
      isEmptyParser,
      lengthParser,
      reverseParser,
      uniqueParser
    ]

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

stripPrefixParser :: Parser Transformation
stripPrefixParser = do
  void $ symbol "stripPrefix"
  stripPrefix <$> textOperand

stripSuffixParser :: Parser Transformation
stripSuffixParser = do
  void $ symbol "stripSuffix"
  stripSuffix <$> textOperand

isPrefixOfParser :: Parser Transformation
isPrefixOfParser = do
  void $ symbol "isPrefixOf"
  isPrefixOf <$> textOperand

isSuffixOfParser :: Parser Transformation
isSuffixOfParser = do
  void $ symbol "isSuffixOf"
  isSuffixOf <$> textOperand

isInfixOfParser :: Parser Transformation
isInfixOfParser = do
  void $ symbol "isInfixOf"
  isInfixOf <$> textOperand

isEmptyParser :: Parser Transformation
isEmptyParser = keyword "isEmpty" isEmpty

lengthParser :: Parser Transformation
lengthParser = keyword "length" length

reverseParser :: Parser Transformation
reverseParser = keyword "reverse" reverse

uniqueParser :: Parser Transformation
uniqueParser = keyword "unique" unique

-- | A JSON number literal, e.g. @1@ or @0.5@.
numberOperand :: Parser Scientific
numberOperand = jsonNumber

-- | A JSON string literal, e.g. @"hello"@.
textOperand :: Parser Text
textOperand = jsonText

-- | A JSON array literal, e.g. @[1, "two"]@.
arrayOperand :: Parser [Value]
arrayOperand = jsonArray
