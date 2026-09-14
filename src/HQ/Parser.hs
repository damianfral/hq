{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Parser where

import HQ.Optic
import HQ.Query
import Relude hiding (Compose, id, many, some)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, spaceChar, string)

parseValue :: Text -> Either (ParseErrorBundle Text Void) Value
parseValue = parse (spaceConsumer *> jsonValueParser <* eof) "value"

parseQuery :: Text -> Either (ParseErrorBundle Text Void) Query
parseQuery = parse (spaceConsumer *> queryParser <* eof) "query"

type Parser = Parsec Void Text

queryParser :: Parser Query
queryParser = setParser <|> deleteParser <|> viewOrFoldParser

setParser :: Parser Query
setParser = symbol "set" >> Set <$> opticParser <*> jsonValueParser

deleteParser :: Parser Query
deleteParser = symbol "delete" >> Delete <$> opticParser

viewOrFoldParser :: Parser Query
viewOrFoldParser = operationParser <*> opticParser

parseOptic :: Text -> Either (ParseErrorBundle Text Void) Optic
parseOptic = parse (spaceConsumer *> opticParser <* eof) "optic"

operationParser :: Parser (Optic -> Query)
operationParser = (symbol "fold" $> Fold) <|> (symbol "view" $> Preview)

jsonValueParser :: Parser Value
jsonValueParser =
  nullParser
    <|> boolParser
    <|> numberParser
    <|> stringParser
    <|> arrayParser
    <|> objectParser

nullParser :: Parser Value
nullParser = symbol "null" $> Null

boolParser :: Parser Value
boolParser = (symbol "true" $> Bool True) <|> (symbol "false" $> Bool False)

numberParser :: Parser Value
numberParser = lexeme $ do
  sign <- maybe "" (: []) <$> optional (char '-')
  digits <- some digitChar
  frac <- maybe "" ('.' :) <$> optional (some digitChar)
  let numStr = sign ++ digits ++ frac
  case readMaybe numStr of
    Just n -> pure (Number n)
    Nothing -> fail "invalid number"

stringParser :: Parser Value
stringParser = do
  void $ char '"'
  chars <- many (escapedChar <|> nonEscapeChar)
  void $ char '"'
  pure $ String (toText chars)
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

arrayParser :: Parser Value
arrayParser = do
  void $ lexeme (char '[')
  vals <- jsonValueParser `sepBy` lexeme (char ',')
  void $ lexeme (char ']')
  pure $ Array (fromList vals)

objectParser :: Parser Value
objectParser = do
  void $ lexeme $ char '{'
  pairs <- objectField `sepBy` lexeme (char ',')
  void $ lexeme $ char '}'
  pure $ Object $ fromList pairs

objectField :: Parser (Text, Value)
objectField = do
  key <- lexeme $ do
    void $ char '"'
    k <- many (satisfy (\c -> c /= '"' && c /= '\\'))
    void $ char '"'
    pure (toText k)
  void $ lexeme (char ':')
  val <- jsonValueParser
  pure (key, val)

-- | Parse an optic path.
--
-- Optic atoms are composed left-to-right with @.@:
--
-- @#a.#b.#c@ = @(a . b) . c@
--
-- The @each@ atom iterates over container elements. When composed
-- with another optic, it distributes that optic:
--
-- @#users.each.#name@ focuses on the @name@ field of each array element.
opticParser :: Parser Optic
opticParser = do
  initial <- opticAtomParser
  opts <- many (symbol "." *> opticAtomParser)
  pure $ foldl' compose initial opts

opticAtomParser :: Parser Optic
opticAtomParser = fieldParser <|> eachParser <|> idParser <|> prismParser

fieldParser :: Parser Optic
fieldParser = char '#' >> field <$> identifier

identifier :: Parser Text
identifier = lexeme $ fromString <$> some (alphaNumChar <|> char '_' <|> char '-')

eachParser :: Parser Optic
eachParser = symbol "each" $> each

idParser :: Parser Optic
idParser = symbol "id" $> id

prismParser :: Parser Optic
prismParser = char '_' *> prismNameParser

prismNameParser :: Parser Optic
prismNameParser =
  symbol "String"
    $> _String
    <|> symbol "Number"
    $> _Number
    <|> symbol "Bool"
    $> _Bool
    <|> symbol "Null"
    $> _Null
    <|> symbol "Array"
    $> _Array
    <|> symbol "Object"
    $> _Object
    <|> symbol "Just"
    $> _Just
    <|> symbol "1"
    $> _1
    <|> symbol "2"
    $> _2

spaceConsumer :: Parser ()
spaceConsumer = skipMany spaceChar

symbol :: Text -> Parser Text
symbol = lexeme . string

lexeme :: Parser a -> Parser a
lexeme p = p <* spaceConsumer
