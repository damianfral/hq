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
      let isNull = T.null (decoderInput decoder)
       in if isNull then Right (Done decoder) else Left TrailingInput
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
           in emitContainerEnd JSONEndObject rest newDecoder
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
           in emitContainerEnd JSONEndObject rest newDecoder
        [] -> Left ExpectedCommaOrEnd
    | otherwise -> Left ExpectedCommaOrEnd
  DecoderPhaseArrayComma
    | c == ',' ->
        step decoder {decoderInput = rest, decoderPhase = DecoderPhaseValue}
    | c == ']' -> case decoderStack decoder of
        (_ : contexts) ->
          emitContainerEnd JSONEndArray rest decoder {decoderStack = contexts}
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
        emitContainerEnd JSONEndArray rest decoder {decoderStack = contexts}
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

-- | Decode complete text chunks to events (single-shot list driver for
-- embedded literals and tests). Steps 'step' directly and finalizes
-- with 'finish', mirroring 'pullEvent' terminal policy: pending input
-- always takes precedence over pulling more chunks.
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
    atEnd dec' = case finish dec' of
      Left UnexpectedEnd
        | decoderPhase dec' == DecoderPhaseValue && null (decoderStack dec') -> Right []
      Left err -> Left err
      Right (Done _) -> Right []
      Right (NeedInput _) -> Left UnexpectedEnd
      Right (Emit event dec'') -> (event :) <$> atEnd dec''

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
      -- Pending input takes precedence over pulling more text,
      -- mirroring drain: only a truly drained decoder may end.
      | not (T.null (decoderInput dec)) = case step dec of
          Left err -> leave early (HQDecodeError err)
          Right (Emit event dec') -> pure (NextEvent event dec' txt)
          Right (NeedInput dec') -> pullMore dec' txt
          Right (Done dec') -> endCheck dec' txt
      | otherwise = do
          result <- S.next txt
          case result of
            Left ()
              | isRootDone dec || dec == initialDecoder -> pure EndOfInput
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
    finishEnd dec = case finish dec of
      Left UnexpectedEnd
        | decoderPhase dec == DecoderPhaseValue && null (decoderStack dec) ->
            pure EndOfInput
      Left err -> leave early (HQDecodeError err)
      Right (Done _) -> pure EndOfInput
      Right (NeedInput _) -> leave early (HQDecodeError UnexpectedEnd)
      Right (Emit event dec') -> pure $ NextEvent event dec' $ pure ()
