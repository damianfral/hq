{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Shared vocabulary of the streaming JSON decoder.
--
-- Canonical home for 'StringBuffer', 'NestDepth' plus the shared
-- string/escape tables.
module HQ.JSON.Decoder.Core where

import Data.Char (digitToInt, isHexDigit)
import qualified Data.Text as T
import HQ.Error (HQError)
import HQ.JSON.Decoder.Error (DecodeError)
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of, Stream)

-- | Buffered string fragments (reversed for O(1) prepend).
newtype StringBuffer = StringBuffer [Text] deriving (Eq, Show)

emptyStringBuffer :: StringBuffer
emptyStringBuffer = StringBuffer []

appendStringBuffer :: Text -> StringBuffer -> StringBuffer
appendStringBuffer t (StringBuffer ts)
  | T.null t = StringBuffer ts
  | otherwise = StringBuffer (t : ts)

appendCharStringBuffer :: Char -> StringBuffer -> StringBuffer
appendCharStringBuffer c = appendStringBuffer (T.singleton c)

finishStringBuffer :: StringBuffer -> Text
finishStringBuffer (StringBuffer ts) = T.concat (reverse ts)

-- | Nesting depth: the number of enclosing containers.
newtype NestDepth = NestDepth Int deriving (Eq, Ord, Show)

initialDepth :: NestDepth
initialDepth = NestDepth 0

deeper :: NestDepth -> NestDepth
deeper (NestDepth n) = NestDepth (n + 1)

shallower :: NestDepth -> NestDepth
shallower (NestDepth n) = NestDepth (n - 1)

data DecoderState = DecoderState
  { decoderInput :: Text,
    decoderStack :: [DecodeContext],
    decoderNestDepth :: NestDepth,
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

--------------------------------------------------------------------------------
-- Shared string tables (single source for decoder + skip/collect)
--------------------------------------------------------------------------------

-- | Verbatim string bytes; shared so decode, skip and collect split identically.
isStringChar :: Char -> Bool
isStringChar c = c /= '"' && c /= '\\' && ord c >= 0x20

-- | Simple (non-\u) escapes in the JSON string table.
isSimpleEscape :: Char -> Bool
isSimpleEscape c = c == '"' || c == '\\' || c == '/' || c == 'b' || c == 'f' || c == 'n' || c == 'r' || c == 't'

-- | Decode a simple escape to its character. Caller must have checked
-- 'isSimpleEscape' (except 'u', handled separately).
decodeSimpleEscape :: Char -> Char
decodeSimpleEscape '"' = '"'
decodeSimpleEscape '\\' = '\\'
decodeSimpleEscape '/' = '/'
decodeSimpleEscape 'b' = '\b'
decodeSimpleEscape 'f' = '\f'
decodeSimpleEscape 'n' = '\n'
decodeSimpleEscape 'r' = '\r'
decodeSimpleEscape 't' = '\t'
decodeSimpleEscape c = c

-- | Accumulate up to @needed@ hex digits from the head of @input@.
-- Returns @(value', digits', rest)@ where @value'@ folds the new digits
-- into @value@ and @rest@ is the unconsumed remainder.
accumulateHex :: Int -> Int -> Text -> (Int, Int, Text)
accumulateHex value digits input =
  let needed = 4 - digits
      hex = T.take needed (T.takeWhile isHexDigit input)
      rest = T.drop (T.length hex) input
      value' = T.foldl' (\v c -> v * 16 + digitToInt c) value hex
      digits' = digits + T.length hex
   in (value', digits', rest)

data Next = EndOfInput | NextEvent JSONEvent DecoderState (StreamIO Text ())

-- | Failable stream over 'ExceptT HQError IO'.
type StreamIO s = Stream (Of s) (ExceptT HQError IO)
