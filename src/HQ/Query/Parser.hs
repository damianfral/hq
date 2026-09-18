{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query.Parser where

import HQ.JSON.Parser (Parser, jsonValueParser)
import HQ.Optic
import HQ.Optic.Parser (opticParser, spaceConsumer, symbol)
import HQ.Query
import HQ.Transformation (constValue)
import HQ.Transformation.Parser (transformationParser)
import Relude
import Text.Megaparsec

parseQuery :: Text -> Either (ParseErrorBundle Text Void) Query
parseQuery = parse (spaceConsumer *> queryParser <* eof) "query"

queryParser :: Parser Query
queryParser = setParser <|> deleteParser <|> overParser <|> viewOrFoldParser

-- | @set optic value@ is sugar for @over optic (const value)@.
setParser :: Parser Query
setParser = do
  symbol "set" >> Over <$> opticParser <*> (constValue <$> jsonValueParser)

deleteParser :: Parser Query
deleteParser = symbol "delete" >> Delete <$> opticParser

overParser :: Parser Query
overParser = symbol "over" >> Over <$> opticParser <*> transformationParser

viewOrFoldParser :: Parser Query
viewOrFoldParser = operationParser <*> opticParser

operationParser :: Parser (Optic -> Query)
operationParser = symbol "fold" $> Fold
