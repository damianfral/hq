{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Number parsing: state machine plus stepping and finalization.
module HQ.JSON.Decoder.Number where

import Data.Char (digitToInt, isDigit)
import Data.Scientific (Scientific, scientific)
import Data.Text qualified as T
import HQ.JSON.Decoder.Core
import HQ.JSON.Decoder.Error (DecodeError (..))
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)

-- | Is this phase valid at end-of-input?
isValidNumberFinal :: NumberPhase -> Bool
isValidNumberFinal NumberZero = True
isValidNumberFinal NumberNonZero = True
isValidNumberFinal NumberFraction = True
isValidNumberFinal NumberExpDigit = True
isValidNumberFinal _ = False

-- | Advance the number state machine for one character. Digits are
-- tested before delimiters: digits are the common case and never
-- delimit, so leading with 'isDigit' skips two list scans per digit.
-- (Order is behaviour-preserving: no character is both.)
advanceNumber :: NumberPhase -> Char -> NumberStep
advanceNumber phase c
  | isDigit c = case phase of
      NumberSign
        | c == '0' -> NumberStep NumberZero
        | otherwise -> NumberStep NumberNonZero
      NumberZero -> NumberError
      NumberNonZero -> NumberStep NumberNonZero
      NumberDot -> NumberStep NumberFraction
      NumberFraction -> NumberStep NumberFraction
      NumberExpSign -> NumberStep NumberExpDigit
      NumberExpAfterSign -> NumberStep NumberExpDigit
      NumberExpDigit -> NumberStep NumberExpDigit
  | isJsonDelimiter c = NumberEnd
  | otherwise = case phase of
      NumberSign -> NumberError
      NumberZero
        | c == '.' -> NumberStep NumberDot
        | c == 'e' || c == 'E' -> NumberStep NumberExpSign
        | otherwise -> NumberError
      NumberNonZero
        | c == '.' -> NumberStep NumberDot
        | c == 'e' || c == 'E' -> NumberStep NumberExpSign
        | otherwise -> NumberError
      NumberDot -> NumberError
      NumberFraction
        | c == 'e' || c == 'E' -> NumberStep NumberExpSign
        | otherwise -> NumberError
      NumberExpSign
        | c == '+' || c == '-' -> NumberStep NumberExpAfterSign
        | otherwise -> NumberError
      NumberExpAfterSign -> NumberError
      NumberExpDigit -> NumberError

-- | Phases where digits self-loop: a maximal digit run can be consumed
-- without per-character dispatch, leaving the phase unchanged.
acceptsDigitRun :: NumberPhase -> Bool
acceptsDigitRun NumberNonZero = True
acceptsDigitRun NumberFraction = True
acceptsDigitRun NumberExpDigit = True
acceptsDigitRun _ = False

-- | Determine the starting number phase for the first character of a number.
numberPhaseFromFirstChar :: Char -> Maybe NumberPhase
numberPhaseFromFirstChar '-' = Just NumberSign
numberPhaseFromFirstChar '0' = Just NumberZero
numberPhaseFromFirstChar c
  | isDigit c = Just NumberNonZero
  | otherwise = Nothing

startNumberState :: Char -> NumberState
startNumberState c =
  NumberState
    { numberNegative = c == '-',
      numberCoeff = if isDigit c then fromIntegral (digitToInt c) else 0,
      numberExp = 0,
      numberFracDigits = 0,
      numberExpNeg = False,
      numberPhase = fromMaybe NumberSign (numberPhaseFromFirstChar c),
      -- The first character belongs to the error payload, like every
      -- consumed chunk below; one small 'Text' per number, freed on
      -- completion.
      numberFrags = [T.singleton c]
    }

-- | Text consumed for the current number: prior chunks (newest
-- first) plus a slice of the current input. Forced on the error path
-- only.
numberText :: NumberState -> Text -> Int -> Text
numberText ns cur pos = T.concat (reverse (numberFrags ns)) <> T.take pos cur

-- | The error payload for a number rejected at end-of-input or at a
-- delimiter: exactly the consumed text. The scan pushes each consumed
-- slice (including the final one, see 'stepNumber') before
-- finalizing, so prior chunks plus the current slice are all present.
numberErrorText :: NumberState -> Text
numberErrorText ns = T.concat (reverse (numberFrags ns))

-- | Render the accumulated components as a 'Scientific', folding
-- fraction digits into the coefficient and offsetting the exponent.
finishNumber :: NumberState -> Scientific
finishNumber ns =
  (if numberNegative ns then negate else identity) (numberCoeff ns)
    `scientific` fromInteger (expVal - fromIntegral (numberFracDigits ns))
  where
    expVal = (if numberExpNeg ns then negate else identity) (numberExp ns)

-- | Continue parsing a number: tight index loop with the numeric
-- components accumulated inline, so completion needs no intermediate
-- buffer and no re-scan. State is written back once at end, failure,
-- or input exhaustion.
stepNumber :: DecoderState -> NumberState -> Either DecodeError DecoderResult
stepNumber decoder ns0 = scan 0 (numberCoeff ns0) (numberExp ns0) (numberFracDigits ns0) (numberExpNeg ns0) (numberPhase ns0)
  where
    input = decoderInput decoder
    len = T.length input
    scan :: Int -> Integer -> Integer -> Int -> Bool -> NumberPhase -> Either DecodeError DecoderResult
    scan !pos !coeff !expP !nFrac !expNeg !phase
      | pos >= len =
          Right
            $ NeedInput
              decoder
                { decoderInput = mempty,
                  decoderPhase =
                    DecoderPhaseNumber
                      NumberState
                        { numberNegative = numberNegative ns0,
                          numberCoeff = coeff,
                          numberExp = expP,
                          numberFracDigits = nFrac,
                          numberExpNeg = expNeg,
                          numberPhase = phase,
                          numberFrags = if T.null input then numberFrags ns0 else input : numberFrags ns0
                        }
                }
      | otherwise =
          let c = T.index input pos
           in case advanceNumber phase c of
                NumberEnd ->
                  finalizeNumber
                    decoder
                      { decoderInput = T.drop pos input,
                        decoderPhase =
                          DecoderPhaseNumber
                            (pushSlice ns0 input pos)
                              { numberCoeff = coeff,
                                numberExp = expP,
                                numberFracDigits = nFrac,
                                numberExpNeg = expNeg,
                                numberPhase = phase
                              }
                      }
                NumberError -> Left (InvalidNumber (numberText ns0 input pos <> T.singleton c))
                NumberStep phase' -> scan (pos + 1) coeff' expP' nFrac' expNeg' phase'
                  where
                    (coeff', expP', nFrac', expNeg')
                      | isDigit c =
                          let d = fromIntegral (digitToInt c) :: Integer
                           in if isExpPhase phase
                                then (coeff, expP * 10 + d, nFrac, expNeg)
                                else
                                  if phase == NumberDot || phase == NumberFraction
                                    then (coeff * 10 + d, expP, nFrac + 1, expNeg)
                                    else (coeff * 10 + d, expP, nFrac, expNeg)
                      | c == '-' = (coeff, expP, nFrac, True)
                      | otherwise = (coeff, expP, nFrac, expNeg)

-- | Record the consumed slice of the current chunk, so a later
-- rejection ('finalizeNumber', 'finish') reports the full consumed
-- text. One slice header per number; freed on completion.
pushSlice :: NumberState -> Text -> Int -> NumberState
pushSlice ns cur pos
  | pos <= 0 = ns
  | otherwise = ns {numberFrags = T.take pos cur : numberFrags ns}

-- | Digit runs accumulate into the exponent once one started.
isExpPhase :: NumberPhase -> Bool
isExpPhase NumberExpSign = True
isExpPhase NumberExpAfterSign = True
isExpPhase NumberExpDigit = True
isExpPhase _ = False

finalizeNumber :: DecoderState -> Either DecodeError DecoderResult
finalizeNumber decoder = case decoderPhase decoder of
  DecoderPhaseNumber numState
    | isValidNumberFinal (numberPhase numState) ->
        emitEvent
          (JSONNumber (finishNumber numState))
          (decoderInput decoder)
          decoder
    | otherwise -> Left (InvalidNumber (numberErrorText numState))
  _ -> Left (InvalidNumber mempty)
