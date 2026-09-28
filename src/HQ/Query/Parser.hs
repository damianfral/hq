{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query.Parser where

import HQ.JSON.Parser (jsonValueParser)
import HQ.Optic.Parser (opticParser)
import HQ.Parser (Parser, parseTop, symbol)
import HQ.Query
import HQ.Transformation (constValue)
import HQ.Transformation.Parser (transformationParser)
import Relude
import Text.Megaparsec

parseQuery :: Text -> Either (ParseErrorBundle Text Void) Query
parseQuery = parseTop "query" queryParser

queryParser :: Parser Query
queryParser =
  foldParser <|> previewParser <|> overParser <|> deleteParser <|> setParser

-- | @set optic value@ is sugar for @over optic (const value)@.
setParser :: Parser Query
setParser = do
  symbol "set" >> Over <$> opticParser <*> (constValue <$> jsonValueParser)

deleteParser :: Parser Query
deleteParser = symbol "delete" >> Delete <$> opticParser

overParser :: Parser Query
overParser = symbol "over" >> Over <$> opticParser <*> transformationParser

foldParser :: Parser Query
foldParser = symbol "fold" >> Fold <$> opticParser

previewParser :: Parser Query
previewParser = symbol "preview" >> Preview <$> opticParser
