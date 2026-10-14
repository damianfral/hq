{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.ParserSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import HQ.Transformation
import HQ.Transformation.Parser (parseTransformation)
import Relude hiding (Compose, Const, many, some)
import Test.Syd

spec :: Spec
spec = describe "HQ.Transformation.Parser" $ do
  it "parses arithmetic" $ forM_ arithmeticCases $ \(input, expected) ->
    parseTransformation input `shouldBe` Right expected

  it "parses strings and arrays" $ forM_ stringArrayCases $ \(input, expected) ->
    parseTransformation input `shouldBe` Right expected

  it "parses affixes and predicates" $ forM_ affixCases $ \(input, expected) ->
    parseTransformation input `shouldBe` Right expected

  it "parses constants" $ forM_ constantCases $ \(input, expected) ->
    parseTransformation input `shouldBe` Right expected

  it "parses booleans" $ forM_ booleanCases $ \(input, expected) ->
    parseTransformation input `shouldBe` Right expected

  it "parses composition and precedence" $ forM_ compositionCases $ \(input, expected) ->
    parseTransformation input `shouldBe` Right expected

  it "rejects invalid expressions" $ forM_ errorCases $ \input ->
    parseTransformation input `shouldSatisfy` isLeft

arithmeticCases :: [(Text, Transformation)]
arithmeticCases =
  [ ("+1", Add 1),
    ("*2", Multiply 2),
    ("-5", Subtract 5),
    ("/4", Divide 4),
    ("+0.5", Add 0.5),
    ("<5", Lt 5),
    ("<=5", Lte 5),
    (">5", Compose Not (Lte 5)),
    (">=5", Compose Not (Lt 5)),
    ("*1e10", Multiply 1e10)
  ]

stringArrayCases :: [(Text, Transformation)]
stringArrayCases =
  [ ("++ \" world\"", ConcatString " world"),
    ("concat [1,2]", ConcatArray [Number 1, Number 2]),
    ("trim", Trim),
    ("replace \"ab\" \"bc\"", Replace "ab" "bc"),
    ("concat[1,2]", ConcatArray [Number 1, Number 2])
  ]

affixCases :: [(Text, Transformation)]
affixCases =
  [ ("stripPrefix \"pre\"", StripPrefix "pre"),
    ("stripSuffix \"suf\"", StripSuffix "suf"),
    ("isPrefixOf \"pre\"", IsPrefixOf "pre"),
    ("isSuffixOf \"suf\"", IsSuffixOf "suf"),
    ("isInfixOf \"fix\"", IsInfixOf "fix"),
    ("isEmpty", IsEmpty),
    ("length", ArrayLength),
    ("reverse", ArrayReverse),
    ("unique", ArrayUnique),
    ("sort", ArraySort),
    ("merge {\"b\": 1}", Merge (KeyMap.fromList [("b", Number 1)])),
    ("deepMerge {\"a\": {\"y\": 2}}", DeepMerge (KeyMap.fromList [("a", Object (KeyMap.fromList [("y", Number 2)]))])),
    ("stripPrefix \"a\" . trim", Compose (StripPrefix "a") Trim)
  ]

constantCases :: [(Text, Transformation)]
constantCases =
  [ ("const 3", Const (Number 3)),
    ("const \"x\"", Const (String "x")),
    ("const 3 . +1", Compose (Const (Number 3)) (Add 1))
  ]

booleanCases :: [(Text, Transformation)]
booleanCases =
  [ ("== 3", Equal (Number 3)),
    ("= \"x\"", Equal (String "x")),
    ("not", Not),
    ("== 1 . not", Compose (Equal (Number 1)) Not),
    ("not . == 1", Compose Not (Equal (Number 1))),
    ("== 1 or == 2", Or (Equal (Number 1)) (Equal (Number 2))),
    ("== 1 || == 2", Or (Equal (Number 1)) (Equal (Number 2))),
    ("== 1 and == 1", And (Equal (Number 1)) (Equal (Number 1))),
    ("== 1 && == 1", And (Equal (Number 1)) (Equal (Number 1))),
    ("== 1 xor == 2", Xor (Equal (Number 1)) (Equal (Number 2))),
    ("== 1 ^^ == 2", Xor (Equal (Number 1)) (Equal (Number 2))),
    ("== 1 and == 1 or == 3", Or (And (Equal (Number 1)) (Equal (Number 1))) (Equal (Number 3))),
    ("== 1 or == 2 xor == 2", Or (Equal (Number 1)) (Xor (Equal (Number 2)) (Equal (Number 2)))),
    ("== 1 xor == 2 and == 2", Xor (Equal (Number 1)) (And (Equal (Number 2)) (Equal (Number 2))))
  ]

compositionCases :: [(Text, Transformation)]
compositionCases =
  [ ("+1 . trim", Compose (Add 1) Trim),
    ("+1 . == 3", Compose (Add 1) (Equal (Number 3))),
    ("+1 . == 3 or == 4", Or (Compose (Add 1) (Equal (Number 3))) (Equal (Number 4))),
    ("not . == 1 . == 3", Compose (Compose Not (Equal (Number 1))) (Equal (Number 3))),
    ("(== 1 or == 2) . == 3", Compose (Or (Equal (Number 1)) (Equal (Number 2))) (Equal (Number 3))),
    ("not . (== 1 or == 2)", Compose Not (Or (Equal (Number 1)) (Equal (Number 2))))
  ]

errorCases :: [Text]
errorCases =
  [ "merge [1]",
    "",
    "5",
    "+",
    "blah",
    "+ \"x\"",
    "replace 1 2",
    "stripPrefix 1",
    "isEmpty \"x\"",
    "concat 3",
    "trim xyz",
    "not == 1",
    "!",
    "const1",
    "sortOn",
    "android",
    "== 1 orange == 2"
  ]
