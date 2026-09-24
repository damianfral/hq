{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HQ.Optic.Parser where

import HQ.JSON.Parser (Parser)
import HQ.Optic
import Relude hiding (Compose, id, many, some)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude (Read (..))

parseOptic :: Text -> Either (ParseErrorBundle Text Void) Optic
parseOptic = parse (spaceConsumer *> opticParser <* eof) "optic"

opticParser :: Parser Optic
opticParser = do
  initial <- opticAtomParser
  opts <- many (symbol "." *> opticAtomParser)
  pure $ foldl' compose initial opts

opticAtomParser :: Parser Optic
opticAtomParser =
  fieldParser <|> eachParser <|> keysParser <|> valuesParser <|> idParser <|> prismParser <|> ixParser

fieldParser :: Parser Optic
fieldParser = char '#' >> field <$> identifier

identifier :: Parser Text
identifier = lexeme $ fromString <$> some (alphaNumChar <|> char '_' <|> char '-')

eachParser :: Parser Optic
eachParser = symbol "each" $> each

keysParser :: Parser Optic
keysParser = symbol "keys" $> keys

valuesParser :: Parser Optic
valuesParser = symbol "values" $> values

idParser :: Parser Optic
idParser = symbol "id" $> id

prismParser :: Parser Optic
prismParser = char '_' *> prismNameParser

ixParser :: Parser Optic
ixParser = symbol "ix" >> ix <$> decimal

prismNameParser :: Parser Optic
prismNameParser =
  s_String <|> s_Number <|> s_Bool <|> s_Null <|> s_Array <|> s_Object <|> s_Just <|> s_1 <|> s_2
  where
    s_String = symbol "String" $> _String
    s_Number = symbol "Number" $> _Number
    s_Bool = symbol "Bool" $> _Bool
    s_Null = symbol "Null" $> _Null
    s_Array = symbol "Array" $> _Array
    s_Object = symbol "Object" $> _Object
    s_Just = symbol "Just" $> _Just
    s_1 = symbol "1" $> _1
    s_2 = symbol "2" $> _2

spaceConsumer :: Parser ()
spaceConsumer = skipMany spaceChar

symbol :: Text -> Parser Text
symbol = lexeme . string

lexeme :: Parser a -> Parser a
lexeme p = p <* spaceConsumer

instance Read Optic where
  readsPrec _ str = case parseOptic $ fromString str of
    Left _ -> []
    Right v -> [(v, "")]
