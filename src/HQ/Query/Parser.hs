{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query.Parser where

import HQ.JSON.Parser (Parser, jsonValueParser, parseValueEvents)
import HQ.Optic
import HQ.Optic.Parser (opticParser, spaceConsumer, symbol)
import HQ.Query
import Relude
import Text.Megaparsec

parseQuery :: Text -> Either (ParseErrorBundle Text Void) Query
parseQuery = parse (spaceConsumer *> queryParser <* eof) "query"

queryParser :: Parser Query
queryParser = setParser <|> deleteParser <|> viewOrFoldParser

setParser :: Parser Query
setParser = do
  void $ symbol "set"
  optic <- opticParser
  (raw, _) <- match jsonValueParser
  events <- case parseValueEvents raw of
    Left err -> fail (toString err)
    Right events -> pure events
  pure $ Set optic events

deleteParser :: Parser Query
deleteParser = symbol "delete" >> Delete <$> opticParser

viewOrFoldParser :: Parser Query
viewOrFoldParser = operationParser <*> opticParser

operationParser :: Parser (Optic -> Query)
operationParser = (symbol "fold" $> Fold) <|> (symbol "view" $> Preview)
