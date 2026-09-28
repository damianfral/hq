{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Parser where

import Data.Aeson (Value (..))
import Data.Aeson.Key (fromText)
import Data.Scientific (Scientific)
import qualified Data.Vector as V
import qualified HQ.JSON.Decoder as Decoder
import HQ.JSON.Event (JSONEvent (..))
import HQ.Parser (Parser, braces, brackets, colon, commaSep, lexeme, parseTop, sc, symbol)
import Relude hiding (Compose, id, many, some)
import Streaming (Of (..))
import qualified Streaming.Prelude as S
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (scientific, signed)

parseValue :: Text -> Either (ParseErrorBundle Text Void) Value
parseValue = parseTop "value" jsonValueParser

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
nullParser = symbol "null" $> Null

boolParser :: Parser Value
boolParser = (symbol "true" $> Bool True) <|> (symbol "false" $> Bool False)

numberParser :: Parser Value
numberParser = lexeme $ Number <$> signed sc scientific

-- | Raw string contents without quotes, shared by values and object keys.
jsonStringContent :: Parser Text
jsonStringContent = toText <$> many (escapedChar <|> nonEscapeChar)
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

stringParser :: Parser Value
stringParser = lexeme $ do
  void $ char '"'
  chars <- jsonStringContent
  void $ char '"'
  pure $ String chars

arrayParser :: Parser Value
arrayParser = Array . fromList <$> brackets (commaSep jsonValueParser)

objectParser :: Parser Value
objectParser = do
  pairs <- braces (commaSep objectField)
  pure $ Object $ fromList [(fromText k, v) | (k, v) <- pairs]

objectField :: Parser (Text, Value)
objectField = do
  key <- lexeme $ char '"' *> jsonStringContent <* char '"'
  colon
  val <- jsonValueParser
  pure (key, val)

-- | Parse a JSON value and project it, failing with @msg@ on mismatch.
-- Shared by transformation operands so @+1@, @++ \"s\"@ and @concat@ stay total.
jsonTyped :: Text -> (Value -> Maybe a) -> Parser a
jsonTyped msg project = do
  value <- jsonValueParser
  case project value of
    Just a -> pure a
    Nothing -> fail (toString msg)

-- | A JSON number literal, e.g. @1@ or @0.5@.
jsonNumber :: Parser Scientific
jsonNumber = jsonTyped "expected a JSON number" $ \case
  Number n -> Just n
  _ -> Nothing

-- | A JSON string literal, e.g. @\"hello\"@.
jsonText :: Parser Text
jsonText = jsonTyped "expected a JSON string" $ \case
  String t -> Just t
  _ -> Nothing

-- | A JSON array literal, e.g. @[1, \"two\"]@.
jsonArray :: Parser [Value]
jsonArray = jsonTyped "expected a JSON array" $ \case
  Array items -> Just (V.toList items)
  _ -> Nothing
