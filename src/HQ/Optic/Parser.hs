{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Optic.Parser where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import HQ.Optic
import HQ.Parser (Parser, keyword, lexeme, parens, parseTop, symbol)
import HQ.Transformation.Parser (atomParser, transformationParser)
import Relude hiding (Compose, filter, id, many, some)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec.Char.Lexer (decimal)

parseOptic :: Text -> Either (ParseErrorBundle Text Void) Optic
parseOptic = parseTop "optic" opticParser

opticParser :: Parser Optic
opticParser = makeExprParser opticAtomParser [[InfixL (Compose <$ symbol ".")]]

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
fieldParser = char '@' >> Field <$> identifier

identifier :: Parser Text
identifier = lexeme $ fromString <$> some (alphaNumChar <|> char '_' <|> char '-')

eachParser :: Parser Optic
eachParser = keyword "each" Each

keysParser :: Parser Optic
keysParser = keyword "keys" Keys

valuesParser :: Parser Optic
valuesParser = keyword "values" Values

-- | @filter OPTIC TRANSFORMATION@: one group wrapping both sides, or
-- two bare atoms. A @.@ after a bare optic starts an outer composition,
-- so dotted optics need the group form.
filterParser :: Parser Optic
filterParser = symbol "filter" >> (try grouped <|> bare)
  where
    grouped = do
      (o, t) <- parens ((,) <$> opticParser <*> transformationParser)
      pure (Filter o t)
    bare = Filter <$> opticAtomParser <*> transArg
    transArg = atomParser

idParser :: Parser Optic
idParser = keyword "id" Id

prismParser :: Parser Optic
prismParser = char '_' *> prismNameParser

ixParser :: Parser Optic
ixParser = symbol "ix" >> Ix <$> lexeme decimal

prismNameParser :: Parser Optic
prismNameParser =
  choice
    [ keyword "String" (Prism PString),
      keyword "Number" (Prism PNumber),
      keyword "Bool" (Prism PBool),
      keyword "Null" (Prism PNull),
      keyword "Array" (Prism PArray),
      keyword "Object" (Prism PObject),
      keyword "Just" PrismJust
    ]
