{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ
  ( AST (..),
    Cardinality (..),
    compose,
    each,
    executeQuery,
    field,
    Optic (..),
    OpticF (..),
    parseOptic,
    parseQuery,
    parseValue,
    Query (..),
    runDelete,
    runOver,
    runTraversal,
    typecheck,
    TypeError (..),
    Value (..),
  )
where

import Control.Comonad.Cofree
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as JS
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
-- import qualified Data.ByteString.Lazy as BL
import Data.Fix
-- import Data.JsonStream.Parser (arrayOf, (.:))
-- import qualified Data.JsonStream.Parser as JS
import qualified Data.Map.Lazy as Map
import Data.Scientific (Scientific)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Show (appPrec)
import Relude hiding (Compose, many, some)
import Relude.Extra (view)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, spaceChar, string)
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

instance ToJSON Value where
  toJSON Null = JS.Null
  toJSON (Bool b) = JS.Bool b
  toJSON (Number n) = JS.Number n
  toJSON (String s) = JS.String s
  toJSON (Array xs) = JS.Array (fmap toJSON xs)
  toJSON (Object obj) =
    JS.Object $ KM.fromList $ bimap Key.fromText toJSON <$> Map.toList obj

parseValue :: Text -> Either (ParseErrorBundle Text Void) Value
parseValue = parse (spaceConsumer *> jsonValueParser <* eof) "value"

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
  showsPrec d (Optic (Fix (Compose a b))) = showParen (d > composePrec) $ do
    showsPrec prec (Optic a) . showString " . " . showsPrec prec (Optic b)
    where
      composePrec = 5
      prec = composePrec + 1

field :: Text -> Optic
field = Optic . Fix . Field

each :: Optic
each = Optic (Fix Each)

compose :: Optic -> Optic -> Optic
compose (Optic a) (Optic b) = Optic (Fix (Compose a b))

--------------------------------------------------------------------------------

data Query
  = Fold Optic
  | Preview Optic
  | Set Optic Value
  | Delete Optic
  deriving (Show, Eq)

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

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
identifier = lexeme $ fromString <$> some (alphaNumChar <|> char '_' <|> char '-')

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

instance Semigroup Cardinality where
  One <> One = One
  _ <> _ = Many

newtype AST = AST {unAST :: Cofree OpticF Cardinality}

data TypeError = InvalidCardinality Cardinality Cardinality deriving (Eq, Show)

typecheck :: AST -> Either TypeError Optic
typecheck = go . unAST
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
          expected = lc <> rc
      if result == expected
        then pure $ compose l r
        else Left $ InvalidCardinality expected result

runTraversal :: Optic -> Value -> [Value]
runTraversal (Optic optic) = foldFix algebra optic
  where
    algebra :: OpticF (Value -> [Value]) -> Value -> [Value]
    algebra (Field name) (Object obj) = maybe [] pure $ Map.lookup name obj
    algebra (Field name) (Array v) = concatMap (algebra (Field name)) (toList v)
    algebra Each (Array v) = toList v
    algebra Each (Object obj) = Map.elems obj
    algebra (Compose left right) value = concatMap right $ left value
    algebra _ _ = []

-- | Modify the focused values with the given function.
runOver :: Fix OpticF -> (Value -> Value) -> Value -> Value
runOver (Fix (Field name)) f (Object m) = Object $ case Map.lookup name m of
  Just v -> Map.insert name (f v) m
  Nothing -> m
runOver (Fix (Field name)) f (Array xs) =
  Array $ runOver (Fix (Field name)) f <$> xs
runOver (Fix (Field _)) _ val = val
runOver (Fix Each) f (Array xs) = Array $ f <$> xs
runOver (Fix Each) _ val = val
runOver (Fix (Compose l r)) f val = runOver l (runOver r f) val

-- | Replace all values matched by the optic with the given value.
runSet :: Optic -> Value -> Value -> Value
runSet (Optic o) newVal = runOver o $ const newVal

-- | Remove all values matched by the optic.
runDelete :: Optic -> Value -> Value
runDelete (Optic (Fix (Field name))) (Object m) = Object $ Map.delete name m
runDelete (Optic (Fix Each)) _ = Array V.empty
runDelete (Optic (Fix (Compose l r))) val = runOver l (runDelete (Optic r)) val
runDelete _ val = val

--------------------------------------------------------------------------------

executeQuery :: Query -> [Value] -> [Value]
executeQuery (Preview optic) vals = concatMap (runTraversal optic) vals
executeQuery (Fold optic) vals = concatMap (runTraversal optic) vals
executeQuery (Set optic newVal) vals = map (runSet optic newVal) vals
executeQuery (Delete optic) vals = map (runDelete optic) vals
