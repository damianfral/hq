{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query.Parser where

import HQ.JSON.Parser (jsonValueParser)
import HQ.Optic.Parser (opticParser)
import HQ.Parser (Parser, keyword, parseTop)
import HQ.Query
import HQ.Transformation (Transformation (..))
import HQ.Transformation.Parser (transformationParser)
import Relude hiding (Const)
import Text.Megaparsec

parseQuery :: Text -> Either (ParseErrorBundle Text Void) Query
parseQuery = parseTop "query" queryParser

queryParser :: Parser Query
queryParser =
  foldParser <|> previewParser <|> overParser <|> deleteParser <|> setParser

-- | @set@ is @over@ with a constant.
setParser :: Parser Query
setParser = do
  keyword "set" () >> Over <$> opticParser <*> (Const <$> jsonValueParser)

deleteParser :: Parser Query
deleteParser = keyword "delete" () >> Delete <$> opticParser

overParser :: Parser Query
overParser = keyword "over" () >> Over <$> opticParser <*> transformationParser

foldParser :: Parser Query
foldParser = keyword "fold" () >> Fold <$> opticParser

previewParser :: Parser Query
previewParser = keyword "preview" () >> Preview <$> opticParser
