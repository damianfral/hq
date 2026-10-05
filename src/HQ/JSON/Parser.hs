{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Parser where

import Data.Aeson (Value (..))
import Data.Aeson.Decoding (eitherDecodeStrictText, toEitherValue)
import Data.Aeson.Decoding.Text (textToTokens)
import Data.Scientific (Scientific)
import qualified HQ.JSON.Decoder as Decoder
import HQ.JSON.Event (JSONEvent (..))
import HQ.Parser (Parser, lexeme)
import Relude hiding (Compose, id, many, some)
import Streaming (Of (..))
import qualified Streaming.Prelude as S
import Text.Megaparsec (getInput, setInput)

-- | Parse a whole JSON value (CLI @VALUE@ arguments, tests).
-- Single validation via aeson.
parseValue :: Text -> Either String Value
parseValue = eitherDecodeStrictText

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

-- | Parse one JSON literal embedded in the DSL (e.g. after @const@,
-- @==@, @+ N@, @++ "s"@, @concat [...]@) directly with aeson.
-- 'textToTokens' lexes the prefix and 'toEitherValue' returns the
-- remainder, so no custom string/bracket scanner is needed.
jsonValueParser :: Parser Value
jsonValueParser = lexeme $ do
  input <- getInput
  case toEitherValue (textToTokens input) of
    Left err -> fail ("invalid JSON value: " <> err)
    Right (v, rest) -> setInput rest >> pure v

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
  String s -> Just s
  _ -> Nothing

jsonArray :: Parser [Value]
jsonArray = jsonTyped "expected a JSON array" $ \case
  Array items -> Just (toList items)
  _ -> Nothing
