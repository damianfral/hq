{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HQ.Optic.Parser where

import HQ.Optic
import HQ.Parser (Parser, dotChain, keyword, lexeme, parens, parseTop, symbol)
import HQ.Transformation.Parser (atomParser, transformationParser)
import Relude hiding (Compose, filter, id, many, some)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude (Read (..))

parseOptic :: Text -> Either (ParseErrorBundle Text Void) Optic
parseOptic = parseTop "optic" opticParser

opticParser :: Parser Optic
opticParser = dotChain opticAtomParser compose

opticAtomParser :: Parser Optic
opticAtomParser =
  choice
    [ fieldParser,
      eachParser,
      keysParser,
      valuesParser,
      filterParser,
      idParser,
      prismParser,
      ixParser,
      groupedOptic
    ]

-- | Redundant parentheses are tolerated around any optic atom.
groupedOptic :: Parser Optic
groupedOptic = parens opticParser

fieldParser :: Parser Optic
fieldParser = char '@' >> field <$> identifier

identifier :: Parser Text
identifier = lexeme $ fromString <$> some (alphaNumChar <|> char '_' <|> char '-')

eachParser :: Parser Optic
eachParser = keyword "each" each

keysParser :: Parser Optic
keysParser = keyword "keys" keys

valuesParser :: Parser Optic
valuesParser = keyword "values" values

-- | @filter OPTIC TRANSFORMATION@: one group wrapping both sides, or
-- two bare atoms. A @.@ after a bare optic starts an outer composition,
-- so dotted optics need the group form.
filterParser :: Parser Optic
filterParser = symbol "filter" >> (try grouped <|> bare)
  where
    grouped = do
      (o, t) <- parens ((,) <$> opticParser <*> transformationParser)
      pure (filter o t)
    bare = filter <$> opticAtomParser <*> transArg
    transArg = atomParser

idParser :: Parser Optic
idParser = keyword "id" id

prismParser :: Parser Optic
prismParser = char '_' *> prismNameParser

ixParser :: Parser Optic
ixParser = symbol "ix" >> ix <$> lexeme decimal

prismNameParser :: Parser Optic
prismNameParser =
  choice
    [ keyword "String" _String,
      keyword "Number" _Number,
      keyword "Bool" _Bool,
      keyword "Null" _Null,
      keyword "Array" _Array,
      keyword "Object" _Object,
      keyword "Just" _Just
    ]

instance Read Optic where
  readsPrec _ str = case parseOptic $ fromString str of
    Left _ -> []
    Right v -> [(v, "")]
