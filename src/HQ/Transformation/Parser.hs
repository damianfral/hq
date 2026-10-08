{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.Parser where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import HQ.JSON.Parser (jsonArray, jsonNumber, jsonObject, jsonText, jsonValueParser)
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
-- >        |  '<' number | '<=' number | '>' number | '>=' number
-- >        |  '++' string | 'concat' array | 'trim' | 'not'
-- >        |  'replace' string string | 'stripPrefix' string | 'stripSuffix' string
-- >        |  'isPrefixOf' string | 'isSuffixOf' string | 'isInfixOf' string
-- >        |  'isEmpty' | 'length' | 'reverse' | 'unique' | 'sort'
-- >        |  'merge' object | 'deepMerge' object
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
        [InfixL (keyword "and" And <|> (And <$ symbol "&&"))],
        [InfixL (keyword "xor" Xor <|> (Xor <$ symbol "^^"))],
        [InfixL (keyword "or" Or <|> (Or <$ symbol "||"))]
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
      try lteParser,
      ltParser,
      try gteParser,
      gtParser,
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
      uniqueParser,
      sortParser,
      mergeParser,
      deepMergeParser
    ]

parenParser :: Parser Transformation
parenParser = parens transformationParser

constParser :: Parser Transformation
constParser = Const <$> (keyword "const" () *> jsonValueParser)

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

ltParser :: Parser Transformation
ltParser = prefixOp "<" jsonNumber Lt

lteParser :: Parser Transformation
lteParser = prefixOp "<=" jsonNumber Lte

-- | @> v@ is sugar for @not . <= v@: strictness comes from the
-- primitive, negation from composition. No new constructor needed.
gtParser :: Parser Transformation
gtParser = prefixOp ">" jsonNumber (Compose Not . Lte)

-- | @>= v@ is sugar for @not . < v@.
gteParser :: Parser Transformation
gteParser = prefixOp ">=" jsonNumber (Compose Not . Lt)

strConcatParser :: Parser Transformation
strConcatParser = prefixOp "++" jsonText ConcatString

arrayConcatParser :: Parser Transformation
arrayConcatParser = prefixWordOp "concat" jsonArray ConcatArray

trimParser :: Parser Transformation
trimParser = keyword "trim" Trim

notParser :: Parser Transformation
notParser = keyword "not" Not

replaceParser :: Parser Transformation
replaceParser = Replace <$> (keyword "replace" () *> jsonText) <*> jsonText

stripPrefixParser :: Parser Transformation
stripPrefixParser = prefixWordOp "stripPrefix" jsonText StripPrefix

stripSuffixParser :: Parser Transformation
stripSuffixParser = prefixWordOp "stripSuffix" jsonText StripSuffix

isPrefixOfParser :: Parser Transformation
isPrefixOfParser = prefixWordOp "isPrefixOf" jsonText IsPrefixOf

isSuffixOfParser :: Parser Transformation
isSuffixOfParser = prefixWordOp "isSuffixOf" jsonText IsSuffixOf

isInfixOfParser :: Parser Transformation
isInfixOfParser = prefixWordOp "isInfixOf" jsonText IsInfixOf

isEmptyParser :: Parser Transformation
isEmptyParser = keyword "isEmpty" IsEmpty

lengthParser :: Parser Transformation
lengthParser = keyword "length" ArrayLength

reverseParser :: Parser Transformation
reverseParser = keyword "reverse" ArrayReverse

uniqueParser :: Parser Transformation
uniqueParser = keyword "unique" ArrayUnique

-- NOTE: 'sort' is a bare keyword with no prefix relations today, so it
-- needs no 'try'. If a longer 'sort...'-prefixed atom (e.g. @sortOn@)
-- is ever added, it must come first with 'try', like '++' vs '+'.
sortParser :: Parser Transformation
sortParser = keyword "sort" ArraySort

mergeParser :: Parser Transformation
mergeParser = prefixWordOp "merge" jsonObject Merge

deepMergeParser :: Parser Transformation
deepMergeParser = prefixWordOp "deepMerge" jsonObject DeepMerge

-- | Parse @OP operand@ and apply the constructor: shared shape of the
-- symbolic and single-operand parsers.
prefixOp :: Text -> Parser a -> (a -> Transformation) -> Parser Transformation
prefixOp op operand constr = constr <$> (symbol op *> operand)

-- | Word-operator variant of 'prefixOp': the operator must end on a
-- word boundary so @sortOn@/@concatenate@ don't lex as @sort@/@concat@
-- plus residue. Symbolic operators ('+', '++', '<='...) keep 'prefixOp'
-- since their operands ('+1', '"x"') may follow without a space.
prefixWordOp :: Text -> Parser a -> (a -> Transformation) -> Parser Transformation
prefixWordOp op operand constr = constr <$> (keyword op () *> operand)
