{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Parser where

import Data.Aeson (Value (..))
import Data.Aeson.Key (fromText)
import Relude hiding (Compose, id, many, some)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

parseValue :: Text -> Either (ParseErrorBundle Text Void) Value
parseValue = parse (spaceConsumer *> jsonValueParser <* eof) "value"

jsonValueParser :: Parser Value
jsonValueParser = nullParser <|> boolParser <|> numberParser <|> stringParser <|> arrayParser <|> objectParser

nullParser :: Parser Value
nullParser = symbol "null" $> Null

boolParser :: Parser Value
boolParser = (symbol "true" $> Bool True) <|> (symbol "false" $> Bool False)

numberParser :: Parser Value
numberParser = lexeme $ do
  sign <- maybe "" (: []) <$> optional (char '-')
  digits <- some digitChar
  frac <- maybe "" ('.' :) <$> optional (some digitChar)
  let numStr = sign ++ digits ++ frac
  case readMaybe numStr of
    Just n -> pure (Number n)
    Nothing -> fail "invalid number"

stringParser :: Parser Value
stringParser = do
  void $ char '"'
  chars <- many (escapedChar <|> nonEscapeChar)
  void $ char '"'
  pure $ String (toText chars)
  where
    escapedChar = do
      void $ char '\\'
      c <- anySingle
      pure $ case c of
        '"' -> '"'
        '\\' -> '\\'
        'n' -> '\n'
        't' -> '\t'
        _ -> c
    nonEscapeChar = satisfy (\c -> c /= '"' && c /= '\\')

arrayParser :: Parser Value
arrayParser = do
  void $ lexeme (char '[')
  vals <- jsonValueParser `sepBy` lexeme (char ',')
  void $ lexeme (char ']')
  pure $ Array (fromList vals)

objectParser :: Parser Value
objectParser = do
  void $ lexeme $ char '{'
  pairs <- objectField `sepBy` lexeme (char ',')
  void $ lexeme $ char '}'
  pure $ Object $ fromList [(fromText k, v) | (k, v) <- pairs]

objectField :: Parser (Text, Value)
objectField = do
  key <- lexeme $ do
    void $ char '"'
    k <- many (satisfy (\c -> c /= '"' && c /= '\\'))
    void $ char '"'
    pure (toText k)
  void $ lexeme (char ':')
  val <- jsonValueParser
  pure (key, val)

spaceConsumer :: Parser ()
spaceConsumer = skipMany spaceChar

symbol :: Text -> Parser Text
symbol = lexeme . string

lexeme :: Parser a -> Parser a
lexeme p = p <* spaceConsumer
