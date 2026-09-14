{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HQ.Optic.Parser where

import HQ.Optic
import Relude hiding (Compose, id, many, some)
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude (Read (..))

type Parser = Parsec Void Text

parseOptic :: Text -> Either (ParseErrorBundle Text Void) Optic
parseOptic = parse (spaceConsumer *> opticParser <* eof) "optic"

opticParser :: Parser Optic
opticParser = do
  initial <- opticAtomParser
  opts <- many (symbol "." *> opticAtomParser)
  pure $ foldl' compose initial opts

opticAtomParser :: Parser Optic
opticAtomParser = fieldParser <|> eachParser <|> idParser <|> prismParser

fieldParser :: Parser Optic
fieldParser = char '#' >> field <$> identifier

identifier :: Parser Text
identifier = lexeme $ fromString <$> some (alphaNumChar <|> char '_' <|> char '-')

eachParser :: Parser Optic
eachParser = symbol "each" $> each

idParser :: Parser Optic
idParser = symbol "id" $> id

prismParser :: Parser Optic
prismParser = char '_' *> prismNameParser

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
