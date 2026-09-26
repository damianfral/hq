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
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseValue :: Text -> Either (ParseErrorBundle Text Void) Value
parseValue = parse (sc *> jsonValueParser <* eof) "value"

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
decodeValueEvents :: Text -> Either Decoder.DecodeError [JSONEvent]
decodeValueEvents input = runIdentity $ do
  result <- S.toList (Decoder.decode (S.yield input))
  pure $ case result of
    _ :> Left err -> Left err
    events :> Right () -> Right events

jsonValueParser :: Parser Value
jsonValueParser = nullParser <|> boolParser <|> numberParser <|> stringParser <|> arrayParser <|> objectParser

nullParser :: Parser Value
nullParser = L.symbol sc "null" $> Null

boolParser :: Parser Value
boolParser = (L.symbol sc "true" $> Bool True) <|> (L.symbol sc "false" $> Bool False)

numberParser :: Parser Value
numberParser = L.lexeme sc $ Number <$> signed sc scientific

stringParser :: Parser Value
stringParser = L.lexeme sc $ do
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
  void $ L.lexeme sc (char '[')
  vals <- jsonValueParser `sepBy` L.lexeme sc (char ',')
  void $ L.lexeme sc (char ']')
  pure $ Array (fromList vals)

objectParser :: Parser Value
objectParser = do
  void $ L.lexeme sc $ char '{'
  pairs <- objectField `sepBy` L.lexeme sc (char ',')
  void $ L.lexeme sc $ char '}'
  pure $ Object $ fromList [(fromText k, v) | (k, v) <- pairs]

objectField :: Parser (Text, Value)
objectField = do
  key <- L.lexeme sc $ do
    void $ char '"'
    k <- many (satisfy (\c -> c /= '"' && c /= '\\'))
    void $ char '"'
    pure (toText k)
  void $ L.lexeme sc (char ':')
  val <- jsonValueParser
  pure (key, val)

sc :: Parser ()
sc = L.space (void spaceChar) empty empty
