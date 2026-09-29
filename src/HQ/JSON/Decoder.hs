{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Streaming JSON decoder: incremental event parsing over text chunks.
--
-- The machine vocabulary lives in "HQ.JSON.Decoder.Core", the value
-- steppers in "HQ.JSON.Decoder.Number", "HQ.JSON.Decoder.String" and
-- "HQ.JSON.Decoder.Keyword". This module wires them together: stepping,
-- structural dispatch, the decode pipeline and event pulling.
module HQ.JSON.Decoder
  ( module HQ.Error,
    module HQ.JSON.Decoder.Core,
    module HQ.JSON.Decoder.Error,
    initialDecoder,
    feed,
    finish,
    step,
    decode,
    pullEvent,
  )
where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Char (isDigit)
import qualified Data.Text as T
import HQ.Error
import HQ.JSON.Decoder.Core
import HQ.JSON.Decoder.Error
import HQ.JSON.Decoder.Keyword
import HQ.JSON.Decoder.Number
import HQ.JSON.Decoder.String
import HQ.JSON.Depth (initialDepth)
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of, Stream)
import qualified Streaming.Prelude as S

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

decode ::
  (Monad m) =>
  Stream (Of Text) m r -> Stream (Of JSONEvent) m (Either DecodeError r)
decode = runDecoder initialDecoder

runDecoder ::
  (Monad m) =>
  DecoderState ->
  Stream (Of Text) m r ->
  Stream (Of JSONEvent) m (Either DecodeError r)
runDecoder decoder input = do
  result <- lift $ S.next input
  case result of
    Left r
      -- Pending input takes precedence over end of stream: step it
      -- first (mirrors drainCollect's rule). Only a truly drained
      -- decoder may end or finalize.
      | not (T.null (decoderInput decoder)) -> drainStep (step decoder) (pure r)
      | isRootDone decoder || decoder == initialDecoder ->
          pure (Right r)
      | otherwise -> drainAtEnd decoder r
    Right (chunk, rest) -> case feed chunk decoder of
      Left err -> pure (Left err)
      Right dr -> drain dr rest

drain ::
  (Monad m) =>
  DecoderResult ->
  Stream (Of Text) m r ->
  Stream (Of JSONEvent) m (Either DecodeError r)
drain (Emit event nextDecoder) rest =
  S.yield event >> drainStep (step nextDecoder) rest
drain (NeedInput nextDecoder) rest
  -- Pending input takes precedence over pulling more text.
  | not (T.null (decoderInput nextDecoder)) = drainStep (step nextDecoder) rest
  | isRootDone nextDecoder =
      drainTrailing nextDecoder rest
  | otherwise = runDecoder nextDecoder rest
drain (Done nextDecoder) rest = drainTrailing nextDecoder rest

drainStep ::
  (Monad m) =>
  Either DecodeError DecoderResult ->
  Stream (Of Text) m r ->
  Stream (Of JSONEvent) m (Either DecodeError r)
drainStep (Left err) _ = pure (Left err)
drainStep (Right result) rest = drain result rest

drainTrailing ::
  (Monad m) =>
  DecoderState ->
  Stream (Of Text) m r ->
  Stream (Of JSONEvent) m (Either DecodeError r)
drainTrailing nextDecoder rest = do
  more <- lift $ S.next rest
  case more of
    Left r -> drainAtEnd nextDecoder r
    Right (chunk, rest')
      | isRootDone nextDecoder ->
          if T.all isWhitespace chunk
            then drainTrailing nextDecoder rest'
            else pure (Left TrailingInput)
      | otherwise -> pure (Left TrailingInput)

drainAtEnd ::
  (Monad m) => DecoderState -> r -> Stream (Of JSONEvent) m (Either DecodeError r)
drainAtEnd decoder r = case finish decoder of
  Left UnexpectedEnd
    | getAll
        $ foldMap
          All
          [ decoderPhase decoder == DecoderPhaseValue,
            null $ decoderStack decoder
          ] ->
        pure (Right r)
  Left err -> pure (Left err)
  Right (Done _) -> pure (Right r)
  Right (NeedInput _) -> pure (Left UnexpectedEnd)
  Right (Emit event nextDecoder) -> S.yield event >> drainAtEnd nextDecoder r

-- | Pull a single event from a decoder cursor.
--
-- This drives the same machine as 'decode' ('step'/'feed' with the
-- same end-of-input handling as 'runDecoder'/'drainTrailing'/'drainAtEnd'),
-- but returns one event at a time with the advanced cursor instead of
-- an event stream. Navigation peeks at events through this; bulk
-- take/skip loops drive 'step' directly.
pullEvent :: DecoderState -> StreamIO Text () -> ExceptT HQError IO Next
pullEvent decoder txtStream = case step decoder of
  Left err -> throwError (HQDecodeError err)
  Right (Emit event dec') -> pure $ NextEvent event dec' txtStream
  Right (NeedInput dec') -> pullMore dec' txtStream
  Right (Done dec') -> endCheck dec' txtStream
  where
    pullMore :: DecoderState -> StreamIO Text () -> ExceptT HQError IO Next
    pullMore dec txt
      -- Pending input takes precedence over pulling more text,
      -- mirroring drain: only a truly drained decoder may end.
      | not (T.null (decoderInput dec)) = case step dec of
          Left err -> throwError (HQDecodeError err)
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
              Left err -> throwError (HQDecodeError err)
              Right (Emit event dec') -> pure (NextEvent event dec' rest)
              Right (NeedInput dec') -> pullMore dec' rest
              Right (Done dec') -> endCheck dec' rest
    endCheck :: DecoderState -> StreamIO Text () -> ExceptT HQError IO Next
    endCheck dec txt = do
      result <- S.next txt
      case result of
        Left () -> finishEnd dec
        Right (chunk, rest)
          | isRootDone dec ->
              if T.all isWhitespace chunk
                then endCheck dec rest
                else throwError (HQDecodeError TrailingInput)
          | otherwise -> throwError (HQDecodeError TrailingInput)
    finishEnd :: DecoderState -> ExceptT HQError IO Next
    finishEnd dec = case finish dec of
      Left UnexpectedEnd
        | decoderPhase dec == DecoderPhaseValue && null (decoderStack dec) ->
            pure EndOfInput
      Left err -> throwError (HQDecodeError err)
      Right (Done _) -> pure EndOfInput
      Right (NeedInput _) -> throwError (HQDecodeError UnexpectedEnd)
      Right (Emit event dec') -> pure $ NextEvent event dec' $ pure ()
