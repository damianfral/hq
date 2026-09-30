{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Transformation.ParserSpec (spec) where

import Data.Aeson (Value (..))
import HQ.Transformation
import HQ.Transformation.Parser (parseTransformation)
import Relude hiding (and, isPrefixOf, length, not, or, reverse, subtract, xor)
import Test.Syd

spec :: Spec
spec = describe "HQ.Transformation.Parser" $ do
  describe "arithmetic" $ do
    it "parses addition" $ do
      parseTransformation "+1" `shouldBe` Right (add 1)
    it "parses multiplication" $ do
      parseTransformation "*2" `shouldBe` Right (multiply 2)
    it "parses subtraction" $ do
      parseTransformation "-5" `shouldBe` Right (subtract 5)
    it "parses division" $ do
      parseTransformation "/4" `shouldBe` Right (divide 4)
    it "parses fractional numbers" $ do
      parseTransformation "+0.5" `shouldBe` Right (add 0.5)
    it "parses exponent notation" $ do
      parseTransformation "*1e10" `shouldBe` Right (multiply 1e10)

  describe "strings and arrays" $ do
    it "parses string concatenation" $ do
      parseTransformation "++ \" world\""
        `shouldBe` Right (concatString " world")
    it "parses array concatenation" $ do
      parseTransformation "concat [1,2]"
        `shouldBe` Right (concatArray [Number 1, Number 2])
    it "parses trim" $ do parseTransformation "trim" `shouldBe` Right trim
    it "parses replace" $ do
      parseTransformation "replace \"ab\" \"bc\""
        `shouldBe` Right (replace "ab" "bc")

  describe "affixes and predicates" $ do
    it "parses stripPrefix" $ do
      parseTransformation "stripPrefix \"pre\""
        `shouldBe` Right (stripPrefix "pre")
    it "parses stripSuffix" $ do
      parseTransformation "stripSuffix \"suf\""
        `shouldBe` Right (stripSuffix "suf")
    it "parses isPrefixOf" $ do
      parseTransformation "isPrefixOf \"pre\""
        `shouldBe` Right (isPrefixOf "pre")
    it "parses isSuffixOf" $ do
      parseTransformation "isSuffixOf \"suf\""
        `shouldBe` Right (isSuffixOf "suf")
    it "parses isInfixOf" $ do
      parseTransformation "isInfixOf \"fix\""
        `shouldBe` Right (isInfixOf "fix")
    it "parses isEmpty" $ do
      parseTransformation "isEmpty" `shouldBe` Right isEmpty
    it "parses length" $ do
      parseTransformation "length" `shouldBe` Right length
    it "parses reverse" $ do
      parseTransformation "reverse" `shouldBe` Right reverse
    it "parses unique" $ do
      parseTransformation "unique" `shouldBe` Right unique
    it "composes stripPrefix after trim" $ do
      parseTransformation "stripPrefix \"a\" . trim"
        `shouldBe` Right (combine (stripPrefix "a") trim)

  describe "constants" $ do
    it "parses a constant number" $ do
      parseTransformation "const 3" `shouldBe` Right (constValue (Number 3))

    it "parses a constant string" $ do
      parseTransformation "const \"x\"" `shouldBe` Right (constValue (String "x"))

    it "composes a constant with arithmetic" $ do
      parseTransformation "const 3 . +1"
        `shouldBe` Right (combine (constValue (Number 3)) (add 1))

  describe "booleans" $ do
    it "parses equality with ==" $ do
      parseTransformation "== 3" `shouldBe` Right (equal (Number 3))
    it "parses equality with =" $ do
      parseTransformation "= \"x\"" `shouldBe` Right (equal (String "x"))
    it "parses not" $ do
      parseTransformation "not" `shouldBe` Right not
    it "composes not after equality" $ do
      parseTransformation "== 1 . not"
        `shouldBe` Right (combine (equal (Number 1)) not)
    it "composes not before equality" $ do
      parseTransformation "not . == 1"
        `shouldBe` Right (combine not (equal (Number 1)))
    it "parses or" $ do
      parseTransformation "== 1 or == 2"
        `shouldBe` Right (or (equal (Number 1)) (equal (Number 2)))
    it "parses double bar" $ do
      parseTransformation "== 1 || == 2"
        `shouldBe` Right (or (equal (Number 1)) (equal (Number 2)))
    it "parses and" $ do
      parseTransformation "== 1 and == 1"
        `shouldBe` Right (and (equal (Number 1)) (equal (Number 1)))
    it "parses double ampersand" $ do
      parseTransformation "== 1 && == 1"
        `shouldBe` Right (and (equal (Number 1)) (equal (Number 1)))
    it "parses xor" $ do
      parseTransformation "== 1 xor == 2"
        `shouldBe` Right (xor (equal (Number 1)) (equal (Number 2)))
    it "parses double caret" $ do
      parseTransformation "== 1 ^^ == 2"
        `shouldBe` Right (xor (equal (Number 1)) (equal (Number 2)))
    it "binds and tighter than or" $ do
      parseTransformation "== 1 and == 1 or == 3"
        `shouldBe` Right
          (or (and (equal (Number 1)) (equal (Number 1))) (equal (Number 3)))
    it "binds xor tighter than or" $ do
      parseTransformation "== 1 or == 2 xor == 2"
        `shouldBe` Right
          (or (equal (Number 1)) (xor (equal (Number 2)) (equal (Number 2))))
    it "binds and tighter than xor" $ do
      parseTransformation "== 1 xor == 2 and == 2"
        `shouldBe` Right
          (xor (equal (Number 1)) (and (equal (Number 2)) (equal (Number 2))))

  describe "composition and precedence" $ do
    it "composes with dot" $ do
      parseTransformation "+1 . trim" `shouldBe` Right (combine (add 1) trim)
    it "composes equality after arithmetic" $ do
      parseTransformation "+1 . == 3"
        `shouldBe` Right (combine (add 1) (equal (Number 3)))
    it "binds or looser than dot" $ do
      parseTransformation "+1 . == 3 or == 4"
        `shouldBe` Right
          (or (combine (add 1) (equal (Number 3))) (equal (Number 4)))
    it "chains not before equality" $ do
      parseTransformation "not . == 1 . == 3"
        `shouldBe` Right (combine (combine not (equal (Number 1))) (equal (Number 3)))
    it "groups with parentheses" $ do
      parseTransformation "(== 1 or == 2) . == 3"
        `shouldBe` Right
          (combine (or (equal (Number 1)) (equal (Number 2))) (equal (Number 3)))
    it "composes not around parentheses" $ do
      parseTransformation "not . (== 1 or == 2)"
        `shouldBe` Right (combine not (or (equal (Number 1)) (equal (Number 2))))

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
