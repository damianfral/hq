{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query.Parser where

import HQ.JSON.Parser (jsonValueParser)
import HQ.Optic
import HQ.Optic.Parser (Parser, opticParser, spaceConsumer, symbol)
import HQ.Query
import Relude
import Text.Megaparsec

parseQuery :: Text -> Either (ParseErrorBundle Text Void) Query
parseQuery = parse (spaceConsumer *> queryParser <* eof) "query"

queryParser :: Parser Query
queryParser = setParser <|> deleteParser <|> viewOrFoldParser

setParser :: Parser Query
setParser = symbol "set" >> Set <$> opticParser <*> jsonValueParser

deleteParser :: Parser Query
deleteParser = symbol "delete" >> Delete <$> opticParser

viewOrFoldParser :: Parser Query
viewOrFoldParser = operationParser <*> opticParser

operationParser :: Parser (Optic -> Query)
operationParser = (symbol "fold" $> Fold) <|> (symbol "view" $> Preview)
