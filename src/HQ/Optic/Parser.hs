{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HQ.Optic.Parser where

import HQ.JSON.Parser (Parser)
import HQ.Optic
import HQ.Transformation.Parser (atomParser, transformationParser)
import Relude hiding (Compose, filter, id, many, some)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude (Read (..))

parseOptic :: Text -> Either (ParseErrorBundle Text Void) Optic
parseOptic = parse (sc *> opticParser <* eof) "optic"

opticParser :: Parser Optic
opticParser = do
  initial <- opticAtomParser
  opts <- many (L.symbol sc "." *> opticAtomParser)
  pure $ foldl' compose initial opts

opticAtomParser :: Parser Optic
opticAtomParser =
  fieldParser <|> eachParser <|> keysParser <|> valuesParser <|> filterParser <|> idParser <|> prismParser <|> ixParser

fieldParser :: Parser Optic
fieldParser = char '#' >> field <$> identifier

identifier :: Parser Text
identifier = L.lexeme sc $ fromString <$> some (alphaNumChar <|> char '_' <|> char '-')

eachParser :: Parser Optic
eachParser = L.symbol sc "each" $> each

keysParser :: Parser Optic
keysParser = L.symbol sc "keys" $> keys

valuesParser :: Parser Optic
valuesParser = L.symbol sc "values" $> values

-- | @filter OPTIC TRANSFORMATION@: either a single group wrapping
-- both sides, or two bare single atoms, e.g. @filter (#age == 30)@,
-- @filter (each . #age == 30)@ or @filter #public not@. A @.@ after a
-- bare optic starts an outer composition (@filter #a ... . #b@), so
-- dotted optics need the group form.
filterParser :: Parser Optic
filterParser = L.symbol sc "filter" >> (grouped <|> bare)
  where
    grouped = do
      (o, t) <- paren ((,) <$> opticParser <*> transformationParser)
      pure (filter o t)
    bare = filter <$> opticAtomParser <*> transArg
    transArg = atomParser
    paren p = L.lexeme sc (char '(') *> p <* L.lexeme sc (char ')')

idParser :: Parser Optic
idParser = L.symbol sc "id" $> id

prismParser :: Parser Optic
prismParser = char '_' *> prismNameParser

ixParser :: Parser Optic
ixParser = L.symbol sc "ix" >> ix <$> L.lexeme sc decimal

prismNameParser :: Parser Optic
prismNameParser =
  choice
    [ L.symbol sc "String" $> _String,
      L.symbol sc "Number" $> _Number,
      L.symbol sc "Bool" $> _Bool,
      L.symbol sc "Null" $> _Null,
      L.symbol sc "Array" $> _Array,
      L.symbol sc "Object" $> _Object,
      L.symbol sc "Just" $> _Just
    ]

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

instance Read Optic where
  readsPrec _ str = case parseOptic $ fromString str of
    Left _ -> []
    Right v -> [(v, "")]
