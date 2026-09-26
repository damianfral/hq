{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Shared vocabulary of the streaming JSON decoder: machine state,
-- errors and step outcomes, plus the emit helpers every sub-machine
-- uses. Imported by "HQ.JSON.Decoder.Number",
-- "HQ.JSON.Decoder.String", "HQ.JSON.Decoder.Keyword" and the
-- orchestrator in "HQ.JSON.Decoder".
module HQ.JSON.Decoder.Core where

import qualified Data.Text as T
import HQ.JSON.Decoder.StringBuffer
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of, Stream)

data Decoder = Decoder
  { decoderInput :: Text,
    decoderStack :: [Context],
    decoderState :: DecoderState
  }
  deriving (Show, Eq)

data DecoderState
  = DecoderStateValue
  | DecoderStateObjectKey
  | DecoderStateObjectColon
  | DecoderStateObjectComma
  | DecoderStateArrayComma
  | DecoderStateString StringState
  | DecoderStateNumber NumberState
  | DecoderStateKeyword KeywordState
  | DecoderStateFinished
  deriving (Eq, Show)

data Context = ContextArray | ContextObject
  deriving (Eq, Show)

newtype ReversedString = ReversedString {unReversedString :: String}
  deriving (Eq, Show)

reversedStringToText :: ReversedString -> Text
reversedStringToText (ReversedString str) = T.reverse $ T.pack str

-- | JSON number state machine.
data NumberState = NumberState
  { numberBuffer :: ReversedString,
    numberPhase :: NumberPhase
  }
  deriving (Eq, Show)

-- | Tracks how much of a JSON number has been consumed.
-- -? (0 | [1-9][0-9]*) (\.[0-9]+)? ([eE][+-]?[0-9]+)?
data NumberPhase
  = -- | Saw leading '-'
    NumberSign
  | -- | Saw '0' (must not be followed by more digits)
    NumberZero
  | -- | Saw [1-9] and possibly more digits
    NumberNonZero
  | -- | Saw '.' after integer part (need fractional digits)
    NumberDot
  | -- | Saw '.' + at least one digit
    NumberFraction
  | -- | Saw 'e'/'E' (waiting for optional sign or digit)
    NumberExpSign
  | -- | Saw 'e'/'E' then '+'/'-' (need digits)
    NumberExpAfterSign
  | -- | Saw exponent digits
    NumberExpDigit
  deriving (Eq, Show)

data NumberStep
  = -- | Valid continuation, advance to this phase
    NumberStep NumberPhase
  | -- | Delimiter, number is complete
    NumberEnd
  | -- | Invalid character in this context
    NumberError

data StringTarget = StringKey | StringValue
  deriving (Eq, Show)

data BufferedStringTarget = BufferedStringTarget StringTarget StringBuffer
  deriving (Eq, Show)

data Unicode = Unicode {unicodeValue :: Int, unicodeDigits :: Int}
  deriving (Eq, Show)

data StringState
  = InString BufferedStringTarget
  | AfterEscape BufferedStringTarget
  | InUnicodeEscape BufferedStringTarget Unicode
  | AfterHighSurrogate BufferedStringTarget Int
  deriving (Eq, Show)

data KeywordState
  = KeywordNull Int
  | KeywordTrue Int
  | KeywordFalse Int
  deriving (Eq, Show)

data DecoderResult
  = Emit JSONEvent Decoder
  | NeedInput Decoder
  | Done Decoder
  deriving (Eq, Show)

data DecodeError
  = UnexpectedEnd
  | UnexpectedChar Char
  | UnexpectedToken Text
  | ExpectedColon
  | ExpectedCommaOrEnd
  | ExpectedObjectKey
  | ExpectedValue
  | InvalidEscape Char
  | InvalidUnicodeEscape
  | InvalidSurrogatePair
  | InvalidNumber Text
  | InvalidKeyword Text
  | TrailingInput
  deriving (Eq, Show)

emitScalar :: JSONEvent -> Text -> Decoder -> Either DecodeError DecoderResult
emitScalar event remaining decoder =
  let newDec = decoder {decoderInput = remaining}
   in Right $ Emit event $ finishValue newDec

-- | A JSON value has just been completed.
-- Determine what the enclosing context expects next.
finishValue :: Decoder -> Decoder
finishValue decoder = case decoderStack decoder of
  [] -> decoder {decoderState = DecoderStateFinished}
  ContextArray : _ ->
    decoder {decoderState = DecoderStateArrayComma}
  ContextObject : _ ->
    decoder {decoderState = DecoderStateObjectComma}

emitContainerEnd :: JSONEvent -> Text -> Decoder -> Either DecodeError DecoderResult
emitContainerEnd event remaining decoder =
  let decoder' = decoder {decoderInput = remaining}
   in Right $ Emit event (finishValue decoder')

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\t', '\n', '\r']

isJsonDelimiter :: Char -> Bool
isJsonDelimiter c = isWhitespace c || c `elem` [',', ']', '}']

isHighSurrogate :: Int -> Bool
isHighSurrogate x = x >= 0xD800 && x <= 0xDBFF

isLowSurrogate :: Int -> Bool
isLowSurrogate x = x >= 0xDC00 && x <= 0xDFFF

-- | The result of pulling a single event: clean end of input, or one
-- event with the decoder and text positioned immediately after it.
data Next = EndOfInput | NextEvent JSONEvent Decoder (StreamIO Text ())

-- | The house stream: 'Stream' over 'ExceptT Text IO', i.e. a stream
-- that can fail with a 'Text' error. All streaming pipelines in hq
-- run in this stack; see also 'EventStream' in "HQ.Runner.Cursor" for
-- the 'JSONEvent' instantiation used by folding.
type StreamIO s = Stream (Of s) (ExceptT Text IO)
