{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Number parsing for the streaming JSON decoder: the number state
-- machine plus stepping and finalization over a 'Decoder'.
module HQ.JSON.Decoder.Number where

import Data.Char (digitToInt, isDigit)
import Data.Scientific (Scientific, scientific)
import qualified Data.Text as T
import HQ.JSON.Decoder.Core
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)

-- | Is this phase valid at end-of-input?
isValidNumberFinal :: NumberPhase -> Bool
isValidNumberFinal NumberZero = True
isValidNumberFinal NumberNonZero = True
isValidNumberFinal NumberFraction = True
isValidNumberFinal NumberExpDigit = True
isValidNumberFinal _ = False

-- | Advance the number state machine for one character.
advanceNumber :: NumberPhase -> Char -> NumberStep
advanceNumber phase c
  | isJsonDelimiter c = NumberEnd
  | otherwise = case phase of
      NumberSign
        | c == '0' -> NumberStep NumberZero
        | isDigit c -> NumberStep NumberNonZero
        | otherwise -> NumberError
      NumberZero
        | c == '.' -> NumberStep NumberDot
        | c == 'e' || c == 'E' -> NumberStep NumberExpSign
        | otherwise -> NumberError
      NumberNonZero
        | isDigit c -> NumberStep NumberNonZero
        | c == '.' -> NumberStep NumberDot
        | c == 'e' || c == 'E' -> NumberStep NumberExpSign
        | otherwise -> NumberError
      NumberDot
        | isDigit c -> NumberStep NumberFraction
        | otherwise -> NumberError
      NumberFraction
        | isDigit c -> NumberStep NumberFraction
        | c == 'e' || c == 'E' -> NumberStep NumberExpSign
        | otherwise -> NumberError
      NumberExpSign
        | isDigit c -> NumberStep NumberExpDigit
        | c == '+' || c == '-' -> NumberStep NumberExpAfterSign
        | otherwise -> NumberError
      NumberExpAfterSign
        | isDigit c -> NumberStep NumberExpDigit
        | otherwise -> NumberError
      NumberExpDigit
        | isDigit c -> NumberStep NumberExpDigit
        | otherwise -> NumberError

-- | Determine the starting number phase for the first character of a number.
numberPhaseFromFirstChar :: Char -> Maybe NumberPhase
numberPhaseFromFirstChar '-' = Just NumberSign
numberPhaseFromFirstChar '0' = Just NumberZero
numberPhaseFromFirstChar c
  | isDigit c = Just NumberNonZero
  | otherwise = Nothing

-- | Create an initial NumberState from the first character.
startNumberState :: Char -> NumberState
startNumberState c =
  NumberState
    (ReversedString $ one c)
    (fromMaybe NumberSign (numberPhaseFromFirstChar c))

-- | Continue parsing a number from the saved state.
--
-- The common case (the rest of the number sits in the current chunk)
-- is handled by a tight index loop: the chunk length is measured
-- once and no per-character 'T.uncons'.State is written back exactly once,
-- when the number ends, fails, or runs out of input.
stepNumber :: Decoder -> NumberState -> Either DecodeError DecoderResult
stepNumber decoder numState = loop 0 rev0 phase0
  where
    input = decoderInput decoder
    len = T.length input
    ReversedString rev0 = numberBuffer numState
    phase0 = numberPhase numState
    loop :: Int -> String -> NumberPhase -> Either DecodeError DecoderResult
    loop !pos !rev !phase
      | pos >= len =
          Right
            $ NeedInput
              decoder
                { decoderInput = mempty,
                  decoderState = DecoderStateNumber (NumberState (ReversedString rev) phase)
                }
      | otherwise =
          let c = T.index input pos
           in case advanceNumber phase c of
                NumberEnd ->
                  finalizeNumber
                    decoder
                      { decoderInput = T.drop pos input,
                        decoderState = DecoderStateNumber (NumberState (ReversedString rev) phase)
                      }
                NumberError -> Left (InvalidNumber (reversedStringToText (ReversedString rev) <> one c))
                NumberStep nextPhase -> loop (pos + 1) (c : rev) nextPhase

finalizeNumber :: Decoder -> Either DecodeError DecoderResult
finalizeNumber decoder = case decoderState decoder of
  DecoderStateNumber numState
    | isValidNumberFinal (numberPhase numState) ->
        let value = parseNumberBuffer (reversedStringToText $ numberBuffer numState)
         in emitScalar
              (JSONNumber value)
              (decoderInput decoder)
              decoder
    | otherwise -> Left (InvalidNumber (reversedStringToText $ numberBuffer numState))
  _ -> Left (InvalidNumber mempty)

-- | Parse the accumulated number buffer into a Scientific value.
-- We build it manually to stay within the JSON grammar.
--
-- The buffer is scanned once: every digit accumulates into a single
-- coefficient while post-dot digits are counted, so no @10 ^ n@
-- bignum power is needed. A leading @-@ sets only the overall sign;
-- @-@/@+@ later in the buffer is an exponent sign.
parseNumberBuffer :: Text -> Scientific
parseNumberBuffer buf = go (0 :: Integer) (0 :: Integer) (0 :: Int) pos0 False False False
  where
    len = T.length buf
    (negative, pos0)
      | not (T.null buf) && T.index buf 0 == '-' = (True, 1)
      | otherwise = (False, 0)
    go :: Integer -> Integer -> Int -> Int -> Bool -> Bool -> Bool -> Scientific
    go !coeff !expP !nFrac !pos !hasDot !hasExp !expNeg
      | pos >= len =
          let expVal = (if expNeg then negate else identity) expP
              finalExp = fromInteger (expVal - fromIntegral nFrac :: Integer) :: Int
           in (if negative then negate else identity) coeff `scientific` finalExp
      | otherwise =
          let c = T.index buf pos
           in case c of
                '-' -> go coeff expP nFrac (pos + 1) hasDot hasExp True
                '+' -> go coeff expP nFrac (pos + 1) hasDot hasExp False
                '.' -> go coeff expP nFrac (pos + 1) True hasExp expNeg
                'e' -> go coeff expP nFrac (pos + 1) hasDot True expNeg
                'E' -> go coeff expP nFrac (pos + 1) hasDot True expNeg
                _
                  | isDigit c ->
                      let d = fromIntegral (digitToInt c) :: Integer
                       in if hasExp
                            then go coeff (expP * 10 + d) nFrac (pos + 1) hasDot True expNeg
                            else
                              if hasDot
                                then go (coeff * 10 + d) expP (nFrac + 1) (pos + 1) True False expNeg
                                else go (coeff * 10 + d) expP nFrac (pos + 1) False False expNeg
                  | otherwise -> go coeff expP nFrac (pos + 1) hasDot hasExp expNeg
