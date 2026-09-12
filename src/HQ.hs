{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ where

import Control.Comonad.Cofree
import Data.Aeson (FromJSON (parseJSON))
import qualified Data.Aeson as JS
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Fix
import Data.JsonStream.Parser (arrayOf, (.:))
import qualified Data.JsonStream.Parser as JS
import qualified Data.Map.Lazy as Map
import Data.Scientific (Scientific)
import qualified Data.Text as Text
import Data.Vector (Vector)
import GHC.Show (appPrec)
import Relude hiding (Compose, many, some)
import Relude.Extra (view)
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

instance FromJSON Value where
  parseJSON JS.Null = pure Null
  parseJSON (JS.Bool b) = pure $ Bool b
  parseJSON (JS.Number n) = pure $ Number n
  parseJSON (JS.String s) = pure $ String s
  parseJSON (JS.Array xs) = Array <$> traverse parseJSON xs
  parseJSON (JS.Object obj) = Object <$> traverse parseJSON obj'
    where
      obj' = Map.fromList [(Key.toText k, v) | (k, v) <- KM.toList obj]

data OpticF a = Field Text | Each | Compose a a
  deriving (Eq, Ord, Show, Functor)

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
identifier = lexeme $ Text.pack <$> some (alphaNumChar <|> char '_' <|> char '-')

eachParser :: Parser Optic
eachParser = symbol "each" $> each

spaceConsumer :: Parser ()
spaceConsumer = skipMany spaceChar

symbol :: Text -> Parser Text
symbol = lexeme . string

lexeme :: Parser a -> Parser a
lexeme p = p <* spaceConsumer

--------------------------------------------------------------------------------

data Cardinality = One | Many deriving (Eq, Ord, Show)

newtype AST = AST {unAST :: Cofree OpticF Cardinality}

data TypeError = InvalidCardinality Cardinality Cardinality deriving (Eq, Show)

typecheck :: AST -> Either TypeError Optic
typecheck (AST ast) = go ast
  where
    go :: Cofree OpticF Cardinality -> Either TypeError Optic
    go (One :< Field name) = pure $ field name
    go (Many :< Each) = pure each
    go (_ :< Field _) = Left $ InvalidCardinality One Many
    go (_ :< Each) = Left $ InvalidCardinality Many One
    go (result :< Compose left right) = do
      l <- go left
      r <- go right
      let lc = view _extract left
          rc = view _extract right
          expected = composeCardinality lc rc
      if result == expected
        then pure $ compose l r
        else Left $ InvalidCardinality expected result

composeCardinality :: Cardinality -> Cardinality -> Cardinality
composeCardinality One One = One
composeCardinality One Many = Many
composeCardinality Many One = Many
composeCardinality Many Many = Many

run :: Optic -> Value -> [Value]
run (Optic optic) = foldFix algebra optic
  where
    algebra :: OpticF (Value -> [Value]) -> Value -> [Value]
    algebra (Field name) (Object obj) = maybe [] pure $ Map.lookup name obj
    algebra Each (Array values) = toList values
    algebra (Compose left right) value = concatMap right $ left value
    algebra _ _ = []

--------------------------------------------------------------------------------

data JSONStep = FieldStep Text | EachStep deriving (Eq, Show)

data JSONPlan = Root | FieldPlan Text JSONPlan | EachPlan JSONPlan
  deriving (Eq, Show)

instance Semigroup JSONPlan where
  Root <> b = b
  FieldPlan name rest <> b = FieldPlan name (rest <> b)
  EachPlan rest <> b = EachPlan (rest <> b)

instance Monoid JSONPlan where mempty = Root

type JSONStreamParser = JS.Parser JS.Value

compile :: Optic -> JSONStreamParser
compile (Optic optic) = jsonParser $ foldFix algebra optic
  where
    algebra :: OpticF JSONPlan -> JSONPlan
    algebra (Field name) = FieldPlan name Root
    algebra Each = EachPlan Root
    algebra (Compose a b) = a <> b

    jsonParser :: JSONPlan -> JS.Parser JS.Value
    jsonParser Root = JS.value
    jsonParser (FieldPlan name rest) = name .: jsonParser rest
    jsonParser (EachPlan rest) = arrayOf $ jsonParser rest

runJSON :: Optic -> BL.ByteString -> [JS.Value]
runJSON optic = JS.parseLazyByteString $ compile optic
