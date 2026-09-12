{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ where

import Control.Comonad.Cofree
import Data.Fix
import Data.Scientific (Scientific)
import qualified Data.Text as Text
import Data.Vector (Vector)
import GHC.Show (appPrec)
import Relude hiding (Compose, many, some)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, spaceChar, string)
import Prelude (Show (showsPrec), showParen, showString)

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

data OpticF a = Field Text | Each | Compose a a
  deriving (Eq, Ord, Show)

newtype Optic = Optic (Fix OpticF)

instance Eq Optic where
  Optic (Fix (Field a)) == Optic (Fix (Field b)) = a == b
  Optic (Fix Each) == Optic (Fix Each) = True
  Optic (Fix (Compose a b)) == Optic (Fix (Compose c d)) =
    Optic a == Optic c && Optic b == Optic d
  _ == _ = False

instance Show Optic where
  showsPrec d (Optic (Fix (Field name))) =
    showParen (d > appPrec) $ showString "#" . showsPrec (appPrec + 1) name
  showsPrec _ (Optic (Fix Each)) = showString "each"
  showsPrec d (Optic (Fix (Compose a b))) =
    showParen (d > composePrec)
      $ showsPrec (composePrec + 1) (Optic a)
      . showString " . "
      . showsPrec (composePrec + 1) (Optic b)
    where
      composePrec = 5

field :: Text -> Optic
field = Optic . Fix . Field

each :: Optic
each = Optic (Fix Each)

compose :: Optic -> Optic -> Optic
compose (Optic a) (Optic b) = Optic (Fix (Compose a b))

data Query = Preview Optic | Fold Optic deriving (Eq, Show)

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseQuery :: Text -> Either (ParseErrorBundle Text Void) Query
parseQuery = parse (spaceConsumer *> queryParser <* eof) "query"

queryParser :: Parser Query
queryParser = operationParser <*> opticParser

operationParser :: Parser (Optic -> Query)
operationParser =
  (symbol "^.." $> Fold)
    <|> (symbol "^." $> Preview)
    <|> (symbol "fold" $> Fold)
    <|> (symbol "view" $> Preview)

opticParser :: Parser Optic
opticParser = do
  opt <- opticAtomParser
  opts <- many (symbol "." *> opticAtomParser)
  pure $ foldl' compose opt opts

opticAtomParser :: Parser Optic
opticAtomParser = fieldParser <|> eachParser

fieldParser :: Parser Optic
fieldParser = char '#' >> field <$> identifier

identifier :: Parser Text
identifier = lexeme $ Text.pack <$> some (alphaNumChar <|> char '_')

eachParser :: Parser Optic
eachParser = symbol "each" $> each

spaceConsumer :: Parser ()
spaceConsumer = skipMany spaceChar

symbol :: Text -> Parser Text
symbol = lexeme . string

lexeme :: Parser a -> Parser a
lexeme p = p <* spaceConsumer

--------------------------------------------------------------------------------

data Cardinality = One | Many

newtype OpticAnn = OpticAnn Cardinality

newtype AST = AST {unAST :: Cofree OpticF OpticAnn}
