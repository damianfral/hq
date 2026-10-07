{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.ParserSpec (spec) where

import Data.Aeson (Value (..))
import HQ.Transformation
import HQ.Transformation.Parser (parseTransformation)
import Relude hiding (Compose, Const, many, some)
import Test.Syd

spec :: Spec
spec = describe "HQ.Transformation.Parser" $ do
  describe "arithmetic" $ do
    it "parses addition" $ do
      parseTransformation "+1" `shouldBe` Right (Add 1)
    it "parses multiplication" $ do
      parseTransformation "*2" `shouldBe` Right (Multiply 2)
    it "parses subtraction" $ do
      parseTransformation "-5" `shouldBe` Right (Subtract 5)
    it "parses division" $ do
      parseTransformation "/4" `shouldBe` Right (Divide 4)
    it "parses fractional numbers" $ do
      parseTransformation "+0.5" `shouldBe` Right (Add 0.5)
    it "parses less-than" $ do
      parseTransformation "<5" `shouldBe` Right (Lt 5)
    it "parses less-than-or-equal" $ do
      parseTransformation "<=5" `shouldBe` Right (Lte 5)
    it "desugars greater-than to not . <=" $ do
      parseTransformation ">5" `shouldBe` Right (Compose Not (Lte 5))
    it "desugars greater-than-or-equal to not . <" $ do
      parseTransformation ">=5" `shouldBe` Right (Compose Not (Lt 5))
    it "parses exponent notation" $ do
      parseTransformation "*1e10" `shouldBe` Right (Multiply 1e10)

  describe "strings and arrays" $ do
    it "parses string concatenation" $ do
      parseTransformation "++ \" world\""
        `shouldBe` Right (ConcatString " world")
    it "parses array concatenation" $ do
      parseTransformation "concat [1,2]"
        `shouldBe` Right (ConcatArray [Number 1, Number 2])
    it "parses trim" $ do parseTransformation "trim" `shouldBe` Right Trim
    it "parses replace" $ do
      parseTransformation "replace \"ab\" \"bc\""
        `shouldBe` Right (Replace "ab" "bc")

  describe "affixes and predicates" $ do
    it "parses stripPrefix" $ do
      parseTransformation "stripPrefix \"pre\""
        `shouldBe` Right (StripPrefix "pre")
    it "parses stripSuffix" $ do
      parseTransformation "stripSuffix \"suf\""
        `shouldBe` Right (StripSuffix "suf")
    it "parses isPrefixOf" $ do
      parseTransformation "isPrefixOf \"pre\""
        `shouldBe` Right (IsPrefixOf "pre")
    it "parses isSuffixOf" $ do
      parseTransformation "isSuffixOf \"suf\""
        `shouldBe` Right (IsSuffixOf "suf")
    it "parses isInfixOf" $ do
      parseTransformation "isInfixOf \"fix\""
        `shouldBe` Right (IsInfixOf "fix")
    it "parses isEmpty" $ do
      parseTransformation "isEmpty" `shouldBe` Right IsEmpty
    it "parses length" $ do
      parseTransformation "length" `shouldBe` Right ArrayLength
    it "parses reverse" $ do
      parseTransformation "reverse" `shouldBe` Right ArrayReverse
    it "parses unique" $ do
      parseTransformation "unique" `shouldBe` Right ArrayUnique
    it "parses sort" $ do
      parseTransformation "sort" `shouldBe` Right ArraySort
    it "composes stripPrefix after trim" $ do
      parseTransformation "stripPrefix \"a\" . trim"
        `shouldBe` Right (Compose (StripPrefix "a") Trim)

  describe "constants" $ do
    it "parses a constant number" $ do
      parseTransformation "const 3" `shouldBe` Right (Const (Number 3))

    it "parses a constant string" $ do
      parseTransformation "const \"x\"" `shouldBe` Right (Const (String "x"))

    it "composes a constant with arithmetic" $ do
      parseTransformation "const 3 . +1"
        `shouldBe` Right (Compose (Const (Number 3)) (Add 1))

  describe "booleans" $ do
    it "parses equality with ==" $ do
      parseTransformation "== 3" `shouldBe` Right (Equal (Number 3))
    it "parses equality with =" $ do
      parseTransformation "= \"x\"" `shouldBe` Right (Equal (String "x"))
    it "parses not" $ do
      parseTransformation "not" `shouldBe` Right Not
    it "composes not after equality" $ do
      parseTransformation "== 1 . not"
        `shouldBe` Right (Compose (Equal (Number 1)) Not)
    it "composes not before equality" $ do
      parseTransformation "not . == 1"
        `shouldBe` Right (Compose Not (Equal (Number 1)))
    it "parses or" $ do
      parseTransformation "== 1 or == 2"
        `shouldBe` Right (Or (Equal (Number 1)) (Equal (Number 2)))
    it "parses double bar" $ do
      parseTransformation "== 1 || == 2"
        `shouldBe` Right (Or (Equal (Number 1)) (Equal (Number 2)))
    it "parses and" $ do
      parseTransformation "== 1 and == 1"
        `shouldBe` Right (And (Equal (Number 1)) (Equal (Number 1)))
    it "parses double ampersand" $ do
      parseTransformation "== 1 && == 1"
        `shouldBe` Right (And (Equal (Number 1)) (Equal (Number 1)))
    it "parses xor" $ do
      parseTransformation "== 1 xor == 2"
        `shouldBe` Right (Xor (Equal (Number 1)) (Equal (Number 2)))
    it "parses double caret" $ do
      parseTransformation "== 1 ^^ == 2"
        `shouldBe` Right (Xor (Equal (Number 1)) (Equal (Number 2)))
    it "binds and tighter than or" $ do
      parseTransformation "== 1 and == 1 or == 3"
        `shouldBe` Right
          (Or (And (Equal (Number 1)) (Equal (Number 1))) (Equal (Number 3)))
    it "binds xor tighter than or" $ do
      parseTransformation "== 1 or == 2 xor == 2"
        `shouldBe` Right
          (Or (Equal (Number 1)) (Xor (Equal (Number 2)) (Equal (Number 2))))
    it "binds and tighter than xor" $ do
      parseTransformation "== 1 xor == 2 and == 2"
        `shouldBe` Right
          (Xor (Equal (Number 1)) (And (Equal (Number 2)) (Equal (Number 2))))

  describe "composition and precedence" $ do
    it "composes with dot" $ do
      parseTransformation "+1 . trim" `shouldBe` Right (Compose (Add 1) Trim)
    it "composes equality after arithmetic" $ do
      parseTransformation "+1 . == 3"
        `shouldBe` Right (Compose (Add 1) (Equal (Number 3)))
    it "binds or looser than dot" $ do
      parseTransformation "+1 . == 3 or == 4"
        `shouldBe` Right
          (Or (Compose (Add 1) (Equal (Number 3))) (Equal (Number 4)))
    it "chains not before equality" $ do
      parseTransformation "not . == 1 . == 3"
        `shouldBe` Right (Compose (Compose Not (Equal (Number 1))) (Equal (Number 3)))
    it "groups with parentheses" $ do
      parseTransformation "(== 1 or == 2) . == 3"
        `shouldBe` Right
          (Compose (Or (Equal (Number 1)) (Equal (Number 2))) (Equal (Number 3)))
    it "composes not around parentheses" $ do
      parseTransformation "not . (== 1 or == 2)"
        `shouldBe` Right (Compose Not (Or (Equal (Number 1)) (Equal (Number 2))))

  describe "errors" $ do
    it "rejects an empty expression" $ do
      parseTransformation "" `shouldSatisfy` isLeft
    it "rejects a bare number" $ do
      parseTransformation "5" `shouldSatisfy` isLeft
    it "rejects a missing operand" $ do
      parseTransformation "+" `shouldSatisfy` isLeft
    it "rejects an unknown operation" $ do
      parseTransformation "blah" `shouldSatisfy` isLeft
    it "rejects a string where a number is expected" $ do
      parseTransformation "+ \"x\"" `shouldSatisfy` isLeft
    it "rejects numbers where strings are expected" $ do
      parseTransformation "replace 1 2" `shouldSatisfy` isLeft
    it "rejects a number where an affix is expected" $ do
      parseTransformation "stripPrefix 1" `shouldSatisfy` isLeft
    it "rejects an operand for a bare keyword" $ do
      parseTransformation "isEmpty \"x\"" `shouldSatisfy` isLeft
    it "rejects a non-array concat operand" $ do
      parseTransformation "concat 3" `shouldSatisfy` isLeft
    it "rejects trailing garbage" $ do
      parseTransformation "trim xyz" `shouldSatisfy` isLeft
    it "rejects not without composition" $ do
      parseTransformation "not == 1" `shouldSatisfy` isLeft
    it "rejects bang" $ do
      parseTransformation "!" `shouldSatisfy` isLeft
