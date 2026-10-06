{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.Parser where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import HQ.JSON.Parser (jsonArray, jsonNumber, jsonText, jsonValueParser)
import HQ.Parser (Parser, keyword, parens, parseTop, symbol)
import HQ.Transformation
import Relude hiding (Compose, Const, and, isPrefixOf, length, many, not, or, reverse, some, subtract, xor)
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
      [ [InfixL (Compose <$ symbol ".")],
        [InfixL (And <$ (symbol "and" <|> symbol "&&"))],
        [InfixL (Xor <$ (symbol "xor" <|> symbol "^^"))],
        [InfixL (Or <$ (symbol "or" <|> symbol "||"))]
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
constParser = Const <$> (symbol "const" *> jsonValueParser)

equalParser :: Parser Transformation
equalParser = do
  void $ try (symbol "==") <|> symbol "="
  Equal <$> jsonValueParser

addParser :: Parser Transformation
addParser = prefixOp "+" jsonNumber Add

multiplyParser :: Parser Transformation
multiplyParser = prefixOp "*" jsonNumber Multiply

subtractParser :: Parser Transformation
subtractParser = prefixOp "-" jsonNumber Subtract

divideParser :: Parser Transformation
divideParser = prefixOp "/" jsonNumber Divide

strConcatParser :: Parser Transformation
strConcatParser = prefixOp "++" jsonText ConcatString

arrayConcatParser :: Parser Transformation
arrayConcatParser = prefixOp "concat" jsonArray ConcatArray

trimParser :: Parser Transformation
trimParser = keyword "trim" Trim

notParser :: Parser Transformation
notParser = keyword "not" Not

replaceParser :: Parser Transformation
replaceParser = Replace <$> (symbol "replace" *> jsonText) <*> jsonText

stripPrefixParser :: Parser Transformation
stripPrefixParser = prefixOp "stripPrefix" jsonText StripPrefix

stripSuffixParser :: Parser Transformation
stripSuffixParser = prefixOp "stripSuffix" jsonText StripSuffix

isPrefixOfParser :: Parser Transformation
isPrefixOfParser = prefixOp "isPrefixOf" jsonText IsPrefixOf

isSuffixOfParser :: Parser Transformation
isSuffixOfParser = prefixOp "isSuffixOf" jsonText IsSuffixOf

isInfixOfParser :: Parser Transformation
isInfixOfParser = prefixOp "isInfixOf" jsonText IsInfixOf

isEmptyParser :: Parser Transformation
isEmptyParser = keyword "isEmpty" IsEmpty

lengthParser :: Parser Transformation
lengthParser = keyword "length" ArrayLength

reverseParser :: Parser Transformation
reverseParser = keyword "reverse" ArrayReverse

uniqueParser :: Parser Transformation
uniqueParser = keyword "unique" ArrayUnique

-- | Parse @OP operand@ and apply the constructor: shared shape of the
-- symbolic and single-operand parsers.
prefixOp :: Text -> Parser a -> (a -> Transformation) -> Parser Transformation
prefixOp op operand constr = constr <$> (symbol op *> operand)
