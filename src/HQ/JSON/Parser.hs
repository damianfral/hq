{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Parser where

import Data.Aeson (Value (..))
import Data.Aeson.Key (fromText)
import qualified HQ.JSON.Decoder as Decoder
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some)
import Streaming (Of (..))
import qualified Streaming.Prelude as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (scientific, signed)

type Parser = Parsec Void Text

parseValue :: Text -> Either (ParseErrorBundle Text Void) Value
parseValue = parse (spaceConsumer *> jsonValueParser <* eof) "value"

-- | Parse a single JSON value into the streaming event grammar,
-- preserving object member order.
--
-- 'parseValue' builds an Aeson 'Value', whose 'Object' preserves no
-- reliable member order; this variant accepts the same text and
-- returns the ordered events the streaming decoder would produce.
-- Used for the @set@ command's replacement value.
parseValueEvents :: Text -> Either Text [JSONEvent]
parseValueEvents input = case parseValue input of
  Left err -> Left (fromString (errorBundlePretty err))
  Right _ -> case decodeValueEvents input of
    Left err -> Left (fromString (show err))
    Right events -> Right events

-- | Decode a complete text as an event stream, run in 'Identity'.
decodeValueEvents :: Text -> Either Decoder.ParseError [JSONEvent]
decodeValueEvents input = runIdentity $ do
  result <- S.toList (Decoder.decode (S.yield input))
  pure $ case result of
    _ :> Left err -> Left err
    events :> Right () -> Right events

jsonValueParser :: Parser Value
jsonValueParser = nullParser <|> boolParser <|> numberParser <|> stringParser <|> arrayParser <|> objectParser

nullParser :: Parser Value
nullParser = symbol "null" $> Null

boolParser :: Parser Value
boolParser = (symbol "true" $> Bool True) <|> (symbol "false" $> Bool False)

numberParser :: Parser Value
numberParser = lexeme $ Number <$> signed spaceConsumer scientific

stringParser :: Parser Value
stringParser = lexeme $ do
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
