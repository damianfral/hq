{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Streaming JSON decoder over text chunks.
module HQ.JSON.Decoder
  ( module HQ.Error,
    module HQ.JSON.Decoder.Core,
    module HQ.JSON.Decoder.Error,
    initialDecoder,
    feed,
    finish,
    step,
    decodeTexts,
    pullEvent,
    Terminal (..),
    classifyTerminal,
  )
where

import Data.Char (isDigit)
import Data.Text qualified as T
import HQ.Early (Early, leave)
import HQ.Error
import HQ.JSON.Decoder.Core
import HQ.JSON.Decoder.Error
import HQ.JSON.Decoder.Keyword
import HQ.JSON.Decoder.Number
import HQ.JSON.Decoder.String
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming.Prelude qualified as S

initialDecoder :: DecoderState
initialDecoder =
  DecoderState
    { decoderInput = mempty,
      decoderStack = [],
      decoderNestDepth = initialDepth,
      decoderPhase = DecoderPhaseValue
    }

feed :: Text -> DecoderState -> Either DecodeError DecoderResult
feed input decoder = step decoder {decoderInput = decoderInput decoder <> input}

finish :: DecoderState -> Either DecodeError DecoderResult
finish decoder = case decoderPhase decoder of
  DecoderPhaseString (AfterHighSurrogate _ _) -> Left InvalidSurrogatePair
  DecoderPhaseString (AfterLowBackslash _ _) -> Left InvalidSurrogatePair
  DecoderPhaseString (InLowSurrogateEscape {}) -> Left InvalidSurrogatePair
  DecoderPhaseString _ -> Left UnexpectedEnd
  DecoderPhaseNumber numState
    | isValidNumberFinal (numberPhase numState) -> finalizeNumber decoder
    | otherwise ->
        Left $ InvalidNumber (reversedStringToText $ numberBuffer numState)
  DecoderPhaseKeyword _ -> finalizeKeyword decoder
  DecoderPhaseFinished -> case decoderStack decoder of
    [] ->
      let isEmpty = T.null (decoderInput decoder)
       in if isEmpty then Right (Done decoder) else Left TrailingInput
    _ -> Left UnexpectedEnd
  _ -> Left UnexpectedEnd

step :: DecoderState -> Either DecodeError DecoderResult
step !decoder = case decoderPhase decoder of
  DecoderPhaseString state -> stepString decoder state
  DecoderPhaseNumber numState -> stepNumber decoder numState
  DecoderPhaseKeyword state -> stepKeyword decoder state
  _ -> stepStructural decoder

stepStructural :: DecoderState -> Either DecodeError DecoderResult
stepStructural !decoder = case T.uncons input of
  Nothing -> case decoderPhase decoder of
    DecoderPhaseFinished ->
      let newDecoder = decoder {decoderInput = mempty}
       in Right
            $ if null (decoderStack decoder)
              then Done newDecoder
              else NeedInput newDecoder
    _ -> Right (NeedInput decoder {decoderInput = mempty})
  Just (c, rest) -> parseStructuralChar c rest decoder
  where
    input = T.dropWhile isWhitespace (decoderInput decoder)

parseStructuralChar ::
  Char -> Text -> DecoderState -> Either DecodeError DecoderResult
parseStructuralChar !c !rest !decoder = case decoderPhase decoder of
  DecoderPhaseValue -> parseValueChar c rest decoder
  DecoderPhaseObjectKey
    | c == '}' -> case decoderStack decoder of
        (_ : contexts) ->
          let newDecoder = decoder {decoderStack = contexts}
           in emitEvent JSONEndObject rest newDecoder
        [] -> Left ExpectedObjectKey
    | c == '"' -> startString StringKey rest decoder
    | otherwise -> Left ExpectedObjectKey
  DecoderPhaseObjectColon
    | c == ':' ->
        step decoder {decoderInput = rest, decoderPhase = DecoderPhaseValue}
    | otherwise -> Left ExpectedColon
  DecoderPhaseObjectComma
    | c == ',' ->
        step
          decoder {decoderInput = rest, decoderPhase = DecoderPhaseObjectKey}
    | c == '}' -> case decoderStack decoder of
        (_ : contexts) ->
          let newDecoder = decoder {decoderStack = contexts}
           in emitEvent JSONEndObject rest newDecoder
        [] -> Left ExpectedCommaOrEnd
    | otherwise -> Left ExpectedCommaOrEnd
  DecoderPhaseArrayComma
    | c == ',' ->
        step decoder {decoderInput = rest, decoderPhase = DecoderPhaseValue}
    | c == ']' -> case decoderStack decoder of
        (_ : contexts) ->
          emitEvent JSONEndArray rest decoder {decoderStack = contexts}
        [] -> Left ExpectedCommaOrEnd
    | otherwise -> Left ExpectedCommaOrEnd
  DecoderPhaseFinished -> Left TrailingInput
  DecoderPhaseString _ -> Left (UnexpectedChar c)
  DecoderPhaseNumber _ -> Left (UnexpectedChar c)
  DecoderPhaseKeyword _ -> Left (UnexpectedChar c)

parseValueChar :: Char -> Text -> DecoderState -> Either DecodeError DecoderResult
parseValueChar !c !rest !decoder = case decoderStack decoder of
  DecodeArray : contexts
    | c == ']' ->
        emitEvent JSONEndArray rest decoder {decoderStack = contexts}
  _ -> startValue c rest decoder

startValue :: Char -> Text -> DecoderState -> Either DecodeError DecoderResult
startValue !c !rest !decoder = case c of
  '{' ->
    Right
      $ Emit
        JSONBeginObject
        decoder
          { decoderInput = rest,
            decoderStack = DecodeObject : decoderStack decoder,
            decoderPhase = DecoderPhaseObjectKey
          }
  '[' ->
    Right
      $ Emit
        JSONBeginArray
        decoder
          { decoderInput = rest,
            decoderStack = DecodeArray : decoderStack decoder,
            decoderPhase = DecoderPhaseValue
          }
  '"' -> startString StringValue rest decoder
  'n' -> startKeyword decoder rest "null" 1
  't' -> startKeyword decoder rest "true" 1
  'f' -> startKeyword decoder rest "false" 1
  _
    | isNumberStart c ->
        let numState = startNumberState c
         in stepNumber
              decoder {decoderInput = rest, decoderPhase = DecoderPhaseNumber numState}
              numState
    | otherwise -> Left (UnexpectedChar c)

isNumberStart :: Char -> Bool
isNumberStart c = c == '-' || isDigit c

-- | The root value is complete: empty stack + finished state.
isRootDone :: DecoderState -> Bool
isRootDone decoder =
  decoderPhase decoder == DecoderPhaseFinished && null (decoderStack decoder)

-- | The decoder consumed nothing yet: empty input with initial stack,
-- depth and phase. Cheaper and more precise than '(== initialDecoder)',
-- which compares the whole state (including the input text).
isEmptyDecoder :: DecoderState -> Bool
isEmptyDecoder dec = getAll $ foldMap All predicates
  where
    predicates =
      [ T.null (decoderInput dec),
        null (decoderStack dec),
        decoderNestDepth dec == initialDepth,
        decoderPhase dec == DecoderPhaseValue
      ]

-- | Terminal reading of a finished decoder: the single policy behind
-- 'decodeTexts' (@atEnd@), 'pullEvent' (@finishEnd@) and Take's
-- @finishTakeEvent@. All three must agree on which 'finish' outcomes
-- end cleanly, fail, or yield one more event — that table lives here,
-- each driver only embeds it in its own monad (top-level exhaustion
-- means clean end for the first two, truncated take for the third).
-- NOTE: lives here rather than in Core because it runs 'finish',
-- which Core cannot see without an import cycle (via Number/Keyword).
data Terminal
  = TerminalEnd
  | TerminalError DecodeError
  | TerminalEmit JSONEvent DecoderState
  deriving (Eq, Show)

classifyTerminal :: DecoderState -> Terminal
classifyTerminal dec = case finish dec of
  Left UnexpectedEnd
    | decoderPhase dec == DecoderPhaseValue && null (decoderStack dec) -> TerminalEnd
  Left err -> TerminalError err
  Right (Done _) -> TerminalEnd
  Right (NeedInput _) -> TerminalError UnexpectedEnd
  Right (Emit event dec') -> TerminalEmit event dec'

-- | Decode complete text chunks to events (single-shot list driver for
-- embedded literals and tests). Steps 'step' directly and finalizes
-- with 'finish'; terminal outcomes go through 'classifyTerminal', the
-- policy shared with 'pullEvent': pending input always takes
-- precedence over pulling more chunks.
decodeTexts :: [Text] -> Either DecodeError [JSONEvent]
decodeTexts = go initialDecoder
  where
    go dec [] = atEnd dec
    go dec (chunk : rest) = case feed chunk dec of
      Left err -> Left err
      Right res -> drain res rest
    drain (Emit event dec') rest = (event :) <$> stepped dec' rest
    drain (NeedInput dec') rest
      | not (T.null (decoderInput dec')) = stepped dec' rest
      | isRootDone dec' = trailing dec' rest
      | otherwise = go dec' rest
    drain (Done dec') rest = trailing dec' rest
    stepped dec' rest = case step dec' of
      Left err -> Left err
      Right res -> drain res rest
    trailing dec' [] = atEnd dec'
    trailing dec' (chunk : rest)
      | isRootDone dec' =
          if T.all isWhitespace chunk
            then trailing dec' rest
            else Left TrailingInput
      | otherwise = Left TrailingInput
    atEnd dec' = case classifyTerminal dec' of
      TerminalEnd -> Right []
      TerminalError err -> Left err
      TerminalEmit event dec'' -> (event :) <$> atEnd dec''

-- | Pull a single event; bulk loops drive 'step' directly.
pullEvent :: Early HQError -> DecoderState -> StreamIO Text () -> IO Next
pullEvent early decoder txtStream = case step decoder of
  Left err -> leave early (HQDecodeError err)
  Right (Emit event dec') -> pure $ NextEvent event dec' txtStream
  Right (NeedInput dec') -> pullMore dec' txtStream
  Right (Done dec') -> endCheck dec' txtStream
  where
    pullMore :: DecoderState -> StreamIO Text () -> IO Next
    pullMore dec txt
      -- Pending input takes precedence over pulling more text (same
      -- precedence as 'drain'): only a truly drained decoder may end.
      | not (T.null (decoderInput dec)) = case step dec of
          Left err -> leave early (HQDecodeError err)
          Right (Emit event dec') -> pure (NextEvent event dec' txt)
          Right (NeedInput dec') -> pullMore dec' txt
          Right (Done dec') -> endCheck dec' txt
      | otherwise = do
          result <- S.next txt
          case result of
            Left ()
              | isRootDone dec || isEmptyDecoder dec -> pure EndOfInput
              | otherwise -> finishEnd dec
            Right (chunk, rest) -> case feed chunk dec of
              Left err -> leave early (HQDecodeError err)
              Right (Emit event dec') -> pure (NextEvent event dec' rest)
              Right (NeedInput dec') -> pullMore dec' rest
              Right (Done dec') -> endCheck dec' rest
    endCheck :: DecoderState -> StreamIO Text () -> IO Next
    endCheck dec txt = do
      result <- S.next txt
      case result of
        Left () -> finishEnd dec
        Right (chunk, rest)
          | isRootDone dec ->
              if T.all isWhitespace chunk
                then endCheck dec rest
                else leave early (HQDecodeError TrailingInput)
          | otherwise -> leave early (HQDecodeError TrailingInput)
    finishEnd :: DecoderState -> IO Next
    finishEnd dec = case classifyTerminal dec of
      TerminalEnd -> pure EndOfInput
      TerminalError err -> leave early (HQDecodeError err)
      TerminalEmit event dec' -> pure $ NextEvent event dec' $ pure ()
