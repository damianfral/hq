{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Shared vocabulary of the streaming JSON decoder: machine state,
-- errors and step outcomes, plus the emit helpers every sub-machine
-- uses. Imported by "HQ.JSON.Decoder.Number",
-- "HQ.JSON.Decoder.String", "HQ.JSON.Decoder.Keyword" and the
-- orchestrator in "HQ.JSON.Decoder".
module HQ.JSON.Decoder.Core where

import qualified Data.Text as T
import HQ.Error (HQError)
import HQ.JSON.Decoder.Error (DecodeError)
import HQ.JSON.Decoder.StringBuffer
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of, Stream)

data DecoderState = DecoderState
  { decoderInput :: Text,
    decoderStack :: [DecodeContext],
    decoderPhase :: DecoderPhase
  }
  deriving (Show, Eq)

data DecoderPhase
  = DecoderPhaseValue
  | DecoderPhaseObjectKey
  | DecoderPhaseObjectColon
  | DecoderPhaseObjectComma
  | DecoderPhaseArrayComma
  | DecoderPhaseString StringState
  | DecoderPhaseNumber NumberState
  | DecoderPhaseKeyword KeywordState
  | DecoderPhaseFinished
  deriving (Eq, Show)

data DecodeContext = DecodeArray | DecodeObject
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
  | -- | Mid low-surrogate digits: high half plus partial value and
    -- digit count. Separate from 'InUnicodeEscape' so split escapes
    -- resume in the low half instead of restarting as a fresh codepoint.
    InLowSurrogateEscape BufferedStringTarget Int Unicode
  | -- | Low escape's backslash arrived at a chunk end; the next chunk
    -- must start with @u@. Separate from 'AfterHighSurrogate' (which
    -- requires the backslash too) so split @\\u@ resumes correctly.
    AfterLowBackslash BufferedStringTarget Int
  deriving (Eq, Show)

data KeywordState
  = KeywordNull Int
  | KeywordTrue Int
  | KeywordFalse Int
  deriving (Eq, Show)

data DecoderResult
  = Emit JSONEvent DecoderState
  | NeedInput DecoderState
  | Done DecoderState
  deriving (Eq, Show)

emitScalar :: JSONEvent -> Text -> DecoderState -> Either DecodeError DecoderResult
emitScalar event remaining decoder =
  let newDec = decoder {decoderInput = remaining}
   in Right $ Emit event $ finishValue newDec

-- | A JSON value has just been completed.
-- Determine what the enclosing context expects next.
finishValue :: DecoderState -> DecoderState
finishValue decoder = case decoderStack decoder of
  [] -> decoder {decoderPhase = DecoderPhaseFinished}
  DecodeArray : _ ->
    decoder {decoderPhase = DecoderPhaseArrayComma}
  DecodeObject : _ ->
    decoder {decoderPhase = DecoderPhaseObjectComma}

emitContainerEnd :: JSONEvent -> Text -> DecoderState -> Either DecodeError DecoderResult
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
data Next = EndOfInput | NextEvent JSONEvent DecoderState (StreamIO Text ())

-- | The house stream: 'Stream' over 'ExceptT HQError IO', i.e. a
-- stream that can fail with an 'HQError'. All streaming pipelines in
-- hq run in this stack; see also 'EventStream' in "HQ.Runner.Cursor"
-- for the 'JSONEvent' instantiation used by folding.
type StreamIO s = Stream (Of s) (ExceptT HQError IO)
