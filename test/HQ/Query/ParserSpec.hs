{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Query.ParserSpec (spec) where

import Data.Aeson (Value (..))
import HQ.Optic
import HQ.Query (Query (..))
import HQ.Query.Parser (parseQuery)
import HQ.Transformation hiding (Compose)
import HQ.Transformation qualified as T
import Relude hiding (Compose, Const)
import Test.Syd

spec :: Spec
spec = describe "HQ.Query.Parser" $ do
  it "parses queries" $ forM_ happyCases $ \(input, expected) ->
    parseQuery input `shouldBe` Right expected

  it "rejects invalid queries" $ forM_ rejectCases $ \input ->
    parseQuery input `shouldSatisfy` isLeft

happyCases :: [(Text, Query)]
happyCases =
  [ ("fold @foo", Fold (Field "foo")),
    ("fold each", Fold Each),
    ("fold @foo.each.@bar", Fold (Compose (Compose (Field "foo") Each) (Field "bar"))),
    ("preview @foo.each", Preview (Compose (Field "foo") Each)),
    ("preview each.@foo", Preview (Compose Each (Field "foo"))),
    ("preview @name", Preview (Field "name")),
    ("fold @my_field", Fold (Field "my_field")),
    ("preview @field123", Preview (Field "field123")),
    ("preview @foo_bar_1", Preview (Field "foo_bar_1")),
    ("fold id", Fold Id),
    ("preview id.@foo", Preview (Compose Id (Field "foo"))),
    ("fold keys", Fold Keys),
    ("fold values", Fold Values),
    ("preview @obj.keys", Preview (Compose (Field "obj") Keys)),
    ("preview keys._String", Preview (Compose Keys (Prism PString))),
    ("fold values.@x", Fold (Compose Values (Field "x"))),
    ("fold ix 10", Fold (Ix 10)),
    ("fold filter @age == 30", Fold (Filter (Field "age") (Equal (Number 30)))),
    ("fold @users.filter (each . @age) (== 30)", Fold (Compose (Field "users") (Filter (Compose Each (Field "age")) (Equal (Number 30))))),
    ("over each.filter @age == 30 +1", Over (Compose Each (Filter (Field "age") (Equal (Number 30)))) (Add 1)),
    ("fold (@foo)", Fold (Field "foo")),
    ("fold ((@foo))", Fold (Field "foo")),
    ("fold (@foo . @bar)", Fold (Compose (Field "foo") (Field "bar"))),
    ("fold filter (@age) (== 30)", Fold (Filter (Field "age") (Equal (Number 30)))),
    ("over keys trim", Over Keys Trim),
    ("delete values", Delete Values),
    ("  preview @foo  ", Preview (Field "foo")),
    ("preview @foo . @bar", Preview (Compose (Field "foo") (Field "bar"))),
    ("preview@foo", Preview (Field "foo")),
    ("\tpreview\t@foo\t", Preview (Field "foo")),
    ("fold _String", Fold (Prism PString)),
    ("fold _Number", Fold (Prism PNumber)),
    ("fold _Bool", Fold (Prism PBool)),
    ("fold _Null", Fold (Prism PNull)),
    ("fold _Array", Fold (Prism PArray)),
    ("fold _Object", Fold (Prism PObject)),
    ("fold _Just", Fold PrismJust),
    ("fold each._String", Fold (Compose Each (Prism PString))),
    ("fold @data._Number", Fold (Compose (Field "data") (Prism PNumber))),
    ("fold _Array.ix 0", Fold (Compose (Prism PArray) (Ix 0))),
    ("over @foo +1", Over (Field "foo") (Add 1)),
    ("over each trim", Over Each Trim),
    ("over @foo.each +1", Over (Compose (Field "foo") Each) (Add 1)),
    ("over @n +1 . == 3", Over (Field "n") (T.Compose (Add 1) (Equal (Number 3)))),
    ("over @title ++\"!\"", Over (Field "title") (ConcatString "!")),
    ("over@foo+1", Over (Field "foo") (Add 1)),
    ("set @name \"bob\"", Over (Field "name") (Const (String "bob"))),
    ("set each 0", Over Each (Const (Number 0))),
    ("set @users.each.@name \"anon\"", Over (Compose (Compose (Field "users") Each) (Field "name")) (Const (String "anon")))
  ]

rejectCases :: [Text]
rejectCases =
  [ "fold @users.filter (each . @age == 30)",
    "fold @users.filter (each . @age, == 30)",
    "fold ()",
    "",
    "unknown @foo",
    "fold",
    "fold foo",
    "fold @",
    "folder @foo",
    "fold eachx",
    "fold ix0",
    "over @foo"
  ]
