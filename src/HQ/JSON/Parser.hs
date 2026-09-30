{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Parser where

import Data.Aeson (Value (..))
import Data.Aeson.Key (fromText)
import Data.Char (digitToInt)
import Data.Scientific (Scientific)
import qualified Data.Vector as V
import qualified HQ.JSON.Decoder as Decoder
import HQ.JSON.Event (JSONEvent (..))
import HQ.Parser (Parser, braces, brackets, colon, commaSep, lexeme, parseTop, sc, symbol)
import Relude hiding (Compose, id, many, some)
import Streaming (Of (..))
import qualified Streaming.Prelude as S
import Text.Megaparsec
import Text.Megaparsec.Char (char, hexDigitChar)
import Text.Megaparsec.Char.Lexer (scientific, signed)

parseValue :: Text -> Either (ParseErrorBundle Text Void) Value
parseValue = parseTop "value" jsonValueParser

-- | Parse a value into ordered events ('Value' forgets member order).
-- Single streaming pass (no megaparsec pre-validation).
parseValueEvents :: Text -> Either Text [JSONEvent]
parseValueEvents input = case decodeValueEvents input of
  Left err -> Left (fromString (show err))
  Right events -> Right events

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

-- | Raw string contents without quotes; full escape table, fails on
-- anything else.
jsonStringContent :: Parser Text
jsonStringContent = toText <$> many (escapedChar <|> nonEscapeChar)
  where
    escapedChar = char '\\' *> escapeCode
    escapeCode =
      choice
        [ char '"' $> '"',
          char '\\' $> '\\',
          char '/' $> '/',
          char 'b' $> '\b',
          char 'f' $> '\f',
          char 'n' $> '\n',
          char 'r' $> '\r',
          char 't' $> '\t',
          char 'u' *> unicodeEscape
        ]
    unicodeEscape = do
      high <- hex4
      if Decoder.isHighSurrogate high
        then do
          low <- chunk "\\u" *> hex4
          if Decoder.isLowSurrogate low
            then pure (chr (0x10000 + ((high - 0xD800) * 1024) + (low - 0xDC00)))
            else fail "invalid surrogate pair"
        else
          if Decoder.isLowSurrogate high
            then fail "invalid surrogate pair"
            else pure (chr high)
    hex4 = foldl' (\v c -> v * 16 + digitToInt c) 0 <$> count 4 hexDigitChar
    nonEscapeChar = satisfy (\c -> c /= '"' && c /= '\\' && c >= '\x20')

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

jsonTyped :: Text -> (Value -> Maybe a) -> Parser a
jsonTyped msg project = do
  value <- jsonValueParser
  case project value of
    Just a -> pure a
    Nothing -> fail (toString msg)

jsonNumber :: Parser Scientific
jsonNumber = jsonTyped "expected a JSON number" $ \case
  Number n -> Just n
  _ -> Nothing

jsonText :: Parser Text
jsonText = jsonTyped "expected a JSON string" $ \case
  String t -> Just t
  _ -> Nothing

jsonArray :: Parser [Value]
jsonArray = jsonTyped "expected a JSON array" $ \case
  Array items -> Just (V.toList items)
  _ -> Nothing
