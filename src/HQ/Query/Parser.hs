{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query.Parser where

import HQ.JSON.Parser (Parser, jsonValueParser)
import HQ.Optic.Parser (opticParser)
import HQ.Query
import HQ.Transformation (constValue)
import HQ.Transformation.Parser (transformationParser)
import Relude
import Text.Megaparsec
import Text.Megaparsec.Char (spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L

parseQuery :: Text -> Either (ParseErrorBundle Text Void) Query
parseQuery = parse (sc *> queryParser <* eof) "query"

queryParser :: Parser Query
queryParser =
  foldParser <|> previewParser <|> overParser <|> deleteParser <|> setParser

-- | Whitespace between tokens.
sc :: Parser ()
sc = L.space (void spaceChar) empty empty

-- | @set optic value@ is sugar for @over optic (const value)@.
setParser :: Parser Query
setParser = do
  L.symbol sc "set" >> Over <$> opticParser <*> (constValue <$> jsonValueParser)

deleteParser :: Parser Query
deleteParser = L.symbol sc "delete" >> Delete <$> opticParser

overParser :: Parser Query
overParser = L.symbol sc "over" >> Over <$> opticParser <*> transformationParser

foldParser :: Parser Query
foldParser = do
  void $ L.symbol sc "fold" <|> L.symbol sc "view"
  Fold <$> opticParser

previewParser :: Parser Query
previewParser = L.symbol sc "preview" >> Preview <$> opticParser
