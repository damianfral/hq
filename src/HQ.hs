{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ where

import Data.Scientific (Scientific)
import qualified Data.Text as Text
import Data.Vector (Vector)
import Relude hiding (Compose, many, some)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, spaceChar, string)

data Value
  = Null
  | Bool Bool
  | Number Scientific
  | String Text
  | Array (Vector Value)
  | Object (Map Text Value)
  deriving (Eq, Ord, Show)

data Literal
  = LNull
  | LBool Bool
  | LNumber Scientific
  | LString Text
  | LList [Literal]
  | LObject (Map Text Literal)
  deriving (Eq, Ord, Show)

data Optic
  = Field Text
  | Each
  | Compose Optic Optic
  deriving (Eq, Ord, Show)

data Query
  = Preview Optic
  | Fold Optic
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseQuery :: Text -> Either (ParseErrorBundle Text Void) Query
parseQuery = parse (spaceConsumer *> query <* eof) "query"

query :: Parser Query
query = operation <*> optic

operation :: Parser (Optic -> Query)
operation =
  (symbol "^.." $> Fold)
    <|> (symbol "^." $> Preview)
    <|> (symbol "fold" $> Fold)
    <|> (symbol "view" $> Preview)

optic :: Parser Optic
optic = do
  opt <- opticAtom
  opts <- many (symbol "." *> opticAtom)
  pure $ foldl' Compose opt opts

opticAtom :: Parser Optic
opticAtom = field <|> each

field :: Parser Optic
field = char '#' >> Field <$> identifier

identifier :: Parser Text
identifier = lexeme $ Text.pack <$> some (alphaNumChar <|> char '_')

each :: Parser Optic
each = symbol "each" $> Each

spaceConsumer :: Parser ()
spaceConsumer = skipMany spaceChar

symbol :: Text -> Parser Text
symbol = lexeme . string

lexeme :: Parser a -> Parser a
lexeme p = p <* spaceConsumer
