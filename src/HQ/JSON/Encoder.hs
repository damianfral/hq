{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

--
module HQ.JSON.Encoder
  ( EncodeStyle (..),
    ValueOptions (..),
    encode,
    encodeChunks,
    formatEvent,
    Builder,
    Chunk (..),
    ChunkStream,
    BSStream,
    EncodeCtx (..),
    NestDepth,
    initialDepth,
    EncoderState,
    initialEncoderState,
    Raw (..),
    Join (..),
    EncoderConfig (..),
  )
where

import Data.Bits (Bits (..))
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, byteString, char7, charUtf8, string7, stringUtf8, toLazyByteString)
import Data.Scientific (Scientific, base10Exponent, coefficient)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8Builder)
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of, Stream)
import Streaming.Internal (Stream (..))
import qualified Streaming.Prelude as S

data EncoderConfig = EncoderConfig
  { outputStyle :: EncodeStyle,
    outputValueOptions :: ValueOptions
  }
  deriving (Show, Eq)

-- | Output formatting style.
data EncodeStyle
  = -- | Compressed output with no whitespace.
    Compact
  | -- | Human-readable output using @n@ spaces per nesting
    -- level.
    Pretty Int
  deriving (Eq, Show)

-- | Render complete top-level string values without quotes or
-- escaping.  Strings nested inside containers, and object keys,
-- are unaffected.
data Raw = NoRaw | Raw deriving (Show, Eq)

-- | 'NoJoin' separates complete top-level values with a newline (like
-- jq); 'Join' concatenates them without separators (like @jq -j@).
data Join = NoJoin | Join deriving (Show, Eq)

-- | Options controlling how complete top-level values are rendered, on
-- top of the base 'EncodeStyle'.
--
-- The encoder's event stream may carry several top-level JSON values
-- back to back (this is what a streaming query produces).  These
-- options control how such a stream is rendered: whether each value is
-- terminated with a newline (like @jq@) and whether top-level strings
-- are emitted without surrounding quotes (like @jq -r@).
data ValueOptions = ValueOptions Raw Join deriving (Eq, Show)

-- | The encoder's container stack; the head is the innermost container.
--
-- The 'Bool' distinguishes an empty container (which stays on one line
-- even in 'Pretty' mode) from one that has already emitted a value, and
-- 'EncodeObjectAfterKey' remembers that a key still needs its colon and
-- value.
data EncodeCtx
  = -- | Inside an array; 'True' once at least one element was emitted.
    EncodeArray !Bool
  | -- | Inside an object; 'True' once at least one pair was emitted.
    EncodeObject !Bool
  | -- | A key was just emitted; the next event is its value.
    EncodeObjectAfterKey
  deriving (Eq, Show)

-- | Nesting depth: the number of enclosing containers, tracking
-- 'length' of the '[EncodeCtx]' stack without traversing it.
-- Constructed once at 'initialDepth'; pushed with 'deeper', popped
-- with 'shallower'.
newtype NestDepth = NestDepth Int deriving (Eq, Ord, Show)

-- | The depth outside all containers.
initialDepth :: NestDepth
initialDepth = NestDepth 0

-- | Descend into a container.
deeper :: NestDepth -> NestDepth
deeper (NestDepth n) = NestDepth (n + 1)

-- | Ascend out of a container.
shallower :: NestDepth -> NestDepth
shallower (NestDepth n) = NestDepth (n - 1)

-- | Encoder state threaded through transcription: container contexts
-- plus their depth, kept in sync by construction (only 'formatEvent'
-- and 'closeContainer' reshape it).
data EncoderState = EncoderState [EncodeCtx] NestDepth
  deriving (Eq, Show)

-- | The state outside all containers.
initialEncoderState :: EncoderState
initialEncoderState = EncoderState [] initialDepth

-- | Show a 'Scientific' in compact JSON-compatible form.
--
-- GHC's 'show' instance for 'Scientific' produces "42.0" for integer
-- values and "1.0e10" for exponent notation, neither of which is valid
-- JSON.  This function formats numbers correctly.
showScientific :: Scientific -> Text
showScientific n
  | expo >= 0 = show (coefficient n * 10 ^ expo)
  | otherwise = formatNonInteger (coefficient n) expo
  where
    expo = base10Exponent n

-- | Format a non-integer number with a negative exponent.
-- E.g. coefficient=1, expo=-2 → "0.01"; coefficient=15, expo=-2 → "0.15".
-- Negative coefficients produce a leading "-".
formatNonInteger :: Integer -> Int -> Text
formatNonInteger c e
  | e >= 0 = show (c * 10 ^ e)
  | len <= na = sign <> "0." <> Text.replicate (na - len) "0" <> show (abs c)
  | otherwise =
      let (prefix, suffix) = Text.splitAt (len - na) (show (abs c))
       in sign <> prefix <> "." <> suffix
  where
    sign = if c < 0 then "-" else ""
    len = Text.length (show (abs c))
    na = abs e

-- | Encode a stream of 'JSONEvent's into a stream of 'Chunk's.
--
-- A small formatting state machine walks the event stream, emitting
-- structural separators (commas, colons, and in 'Pretty' mode newlines
-- and indentation) around the raw event encodings produced by
-- 'encodeEvent'.  It never materializes the document: each event is
-- turned into output as soon as it arrives.
--
-- 'ValueOptions' additionally controls how back-to-back top-level
-- values are rendered: a newline may separate them, and top-level
-- strings may be emitted bare.
encodeToChunks :: (Monad m) => EncoderConfig -> JSONStream m r -> ChunkStream m r
encodeToChunks config = go initialEncoderState
  where
    go st events = do
      result <- lift $ S.next events
      case result of
        Left r -> pure r
        Right (event, rest) ->
          let (chunk, st') = formatEvent config st event
           in S.yield chunk >> go st' rest

-- | Format a single event to a 'Chunk', threading the container
-- context. This is one step of 'encodeToChunks', exposed so fused
-- pipelines can format events without an intermediate event stream.
formatEvent :: EncoderConfig -> EncoderState -> JSONEvent -> (Chunk, EncoderState)
formatEvent (EncoderConfig style (ValueOptions rawOpt joinOpt)) st event =
  case event of
    JSONEndArray -> closeContainerAndFinish
    JSONEndObject -> closeContainerAndFinish
    JSONBeginArray ->
      let (sep, st') = beforeValue style st
       in (sep <> encodeEvent event, pushCtx (EncodeArray False) st')
    JSONBeginObject ->
      let (sep, st') = beforeValue style st
       in (sep <> encodeEvent event, pushCtx (EncodeObject False) st')
    JSONObjectKey _ ->
      let (sep, st') = beforeKey style st
       in (sep <> encodeEvent event, st')
    _ ->
      let (sep, st') = beforeValue style st
          st'' = afterValue st'
       in (sep <> valueChunk event st <> finishValue st'', st'')
  where
    closeContainerAndFinish =
      let (sep, st') = closeContainer style st
       in (sep <> finishValue st', st')

    -- Push one container context, tracking depth alongside.
    pushCtx :: EncodeCtx -> EncoderState -> EncoderState
    pushCtx ctx (EncoderState ctxs depth) = EncoderState (ctx : ctxs) (deeper depth)

    -- Render one value event, honoring raw top-level string output.
    valueChunk :: JSONEvent -> EncoderState -> Chunk
    valueChunk (JSONString text) (EncoderState valueCtxs _)
      | rawOpt == Raw && null valueCtxs = encodeRawString text
    valueChunk ev _ = encodeEvent ev

    -- Separator emitted after a complete top-level value.  A top-level
    -- value is one that leaves the context stack empty.
    finishValue :: EncoderState -> Chunk
    finishValue (EncoderState ctxs' _)
      | joinOpt == NoJoin && null ctxs' = Chunk newline 1
      | otherwise = mempty

-- | The structural pieces to emit before an object key: a separator for
-- the first or any following key, and the switch to
-- 'EncodeObjectAfterKey' so the upcoming value gets its colon.
beforeKey :: EncodeStyle -> EncoderState -> (Chunk, EncoderState)
beforeKey style st@(EncoderState ctxs depth) = case ctxs of
  EncodeObject seen : rest ->
    ( elementSeparator style seen depth,
      EncoderState (EncodeObjectAfterKey : rest) depth
    )
  -- A key outside a container; emit it bare.
  _ -> (mempty, st)

-- | The structural pieces to emit before a value (a scalar or a
-- container opening).  The context is left unchanged; 'afterValue'
-- marks the parent non-empty once the value has arrived, or when a
-- nested container closes.
beforeValue :: EncodeStyle -> EncoderState -> (Chunk, EncoderState)
beforeValue style st@(EncoderState ctxs depth) = case ctxs of
  EncodeObjectAfterKey : _ -> (colonSeparator style, st)
  EncodeArray seen : _ ->
    (elementSeparator style seen depth, st)
  -- A value where a key was expected (malformed input); recover by
  -- treating it as an additional array-like member.
  EncodeObject seen : _ ->
    (elementSeparator style seen depth, st)
  -- A top-level value; no separator.
  [] -> (mempty, st)

-- | After a value was emitted, mark the innermost container non-empty.
afterValue :: EncoderState -> EncoderState
afterValue (EncoderState ctxs depth) = EncoderState (mark ctxs) depth
  where
    mark = \case
      [] -> []
      EncodeArray _ : rest -> EncodeArray True : rest
      EncodeObject _ : rest -> EncodeObject True : rest
      EncodeObjectAfterKey : rest -> EncodeObject True : rest

-- | Emit the closing delimiter for a container, then pop it and mark
-- the parent container as having received a value.
closeContainer :: EncodeStyle -> EncoderState -> (Chunk, EncoderState)
closeContainer _ st@(EncoderState [] _) = (mempty, st)
closeContainer style (EncoderState (ctx : rest) depth) =
  let depth' = shallower depth
   in (closingDelimiter style depth' ctx, afterValue (EncoderState rest depth'))

-- | The closing delimiter of the innermost container.  An empty
-- container stays on one line; a non-empty one is closed on its own
-- line, indented to the depth of the container itself (the parent
-- depth).
closingDelimiter :: EncodeStyle -> NestDepth -> EncodeCtx -> Chunk
closingDelimiter style (NestDepth depth) ctx = case ctx of
  EncodeArray seen -> close seen ']'
  EncodeObject seen -> close seen '}'
  EncodeObjectAfterKey -> close True '}'
  where
    close seen delim = case style of
      Pretty width
        | seen ->
            let size = width * depth
             in Chunk (newline <> indentSpaces size <> char7 delim) (size + 2)
      _ -> Chunk (char7 delim) 1

-- | The chunk emitted before an array element or an object key.
--
-- In 'Compact' mode this is just a comma (or nothing for the first
-- item); in 'Pretty' mode a newline (preceded by a comma except for the
-- first item) and indentation to @depth@.
elementSeparator :: EncodeStyle -> Bool -> NestDepth -> Chunk
elementSeparator Compact seen _ =
  if seen then Chunk comma 1 else mempty
elementSeparator (Pretty width) seen (NestDepth depth) =
  let size = width * depth
      prefix = if seen then commaNewline else newline
   in Chunk (prefix <> indentSpaces size) (size + 1 + fromEnum seen)

-- | Indentation body of 'size' spaces as a builder. 'BS.replicate'
-- is a memset, avoiding the two-words-per-space 'Char'-list that
-- 'replicate' would allocate (and that 'string7' would then
-- traverse again).
indentSpaces :: Int -> Builder
indentSpaces size = byteString (BS.replicate size 0x20)

-- | Constant output fragments as shared builders. 'string7'/'char7'
-- on a literal re-unpacks it and rebuilds the builder on every
-- event; these are built once and reused across the whole document.
commaNewline, newline, colonSpace, colon :: Builder
commaNewline = string7 ",\n"
newline = char7 '\n'
colonSpace = string7 ": "
colon = char7 ':'

comma :: Builder
comma = char7 ','

openBrace, closeBrace, openBracket, closeBracket :: Builder
openBrace = char7 '{'
closeBrace = char7 '}'
openBracket = char7 '['
closeBracket = char7 ']'

jsonNull, jsonTrue, jsonFalse :: Builder
jsonNull = string7 "null"
jsonTrue = string7 "true"
jsonFalse = string7 "false"

-- | The chunk emitted between an object key and its value.
colonSeparator :: EncodeStyle -> Chunk
colonSeparator Compact = Chunk colon 1
colonSeparator (Pretty _) = Chunk colonSpace 2

-- | An output fragment with its size. The size is a flush heuristic
-- (character count, not exact bytes for non-ASCII), so chunk
-- boundaries may shift with content; output bytes are unaffected.
data Chunk = Chunk {chunkBuilder :: !Builder, chunkSize :: !Int}

instance Semigroup Chunk where
  Chunk b1 s1 <> Chunk b2 s2 = let !s = s1 + s2 in Chunk (b1 <> b2) s

instance Monoid Chunk where mempty = Chunk mempty 0

encodeEvent :: JSONEvent -> Chunk
encodeEvent = \case
  JSONBeginObject -> Chunk openBrace 1
  JSONEndObject -> Chunk closeBrace 1
  JSONBeginArray -> Chunk openBracket 1
  JSONEndArray -> Chunk closeBracket 1
  JSONNull -> Chunk jsonNull 4
  JSONBool True -> Chunk jsonTrue 4
  JSONBool False -> Chunk jsonFalse 5
  JSONNumber n ->
    let text = toString (showScientific n)
     in Chunk (string7 text) (length text)
  JSONObjectKey text -> encodeString text
  JSONString text -> encodeString text

encodeString :: Text -> Chunk
encodeString text =
  let Chunk body bodySize = encodeStringBody text
   in Chunk (char7 '"' <> body <> char7 '"') (bodySize + 2)

-- | Render a string value as raw UTF-8, without surrounding quotes or
-- escaping.  Used for raw top-level string output (like @jq -r@).
encodeRawString :: Text -> Chunk
encodeRawString text =
  Chunk (stringUtf8 (toString text)) (Text.length text)

encodeStringBody :: Text -> Chunk
encodeStringBody text = let Chunk b s = go text in Chunk b s
  where
    go t
      | Text.null t = mempty
      | otherwise =
          let (safe, rest) = Text.span isSafe t
              -- Estimated size (chars, not bytes): exact enough to drive
              -- flush decisions, and free, unlike a byte-counting pass.
              safeChunk = Chunk (encodeUtf8Builder safe) (Text.length safe)
           in case Text.uncons rest of
                Nothing -> safeChunk
                Just (c, rest') ->
                  mconcat [safeChunk, encodeEscapedChar c, go rest']

isSafe :: Char -> Bool
isSafe c = c >= '\x20' && c /= '"' && c /= '\\'

encodeEscapedChar :: Char -> Chunk
encodeEscapedChar = \case
  '"' -> Chunk (string7 "\\\"") 2
  '\\' -> Chunk (string7 "\\\\") 2
  '\b' -> Chunk (string7 "\\b") 2
  '\f' -> Chunk (string7 "\\f") 2
  '\n' -> Chunk (string7 "\\n") 2
  '\r' -> Chunk (string7 "\\r") 2
  '\t' -> Chunk (string7 "\\t") 2
  c
    | c < '\x20' -> Chunk (encodeUnicodeEscape $ fromEnum c) 6
    | otherwise ->
        let size = utf8CharSize c
         in Chunk (charUtf8 c) size

encodeUnicodeEscape :: Int -> Builder
encodeUnicodeEscape n =
  string7 "\\u"
    <> char7 (hexDigit ((n `shiftR` 12) .&. 0xf))
    <> char7 (hexDigit ((n `shiftR` 8) .&. 0xf))
    <> char7 (hexDigit ((n `shiftR` 4) .&. 0xf))
    <> char7 (hexDigit (n .&. 0xf))

hexDigit :: Int -> Char
hexDigit x
  | x < 10 = toEnum (fromEnum '0' + x)
  | otherwise = toEnum (fromEnum 'a' + x - 10)

utf8CharSize :: Char -> Int
utf8CharSize c
  | n <= 0x7f = 1
  | n <= 0x7ff = 2
  | n <= 0xffff = 3
  | otherwise = 4
  where
    n = fromEnum c

type JSONStream m r = Stream (Of JSONEvent) m r

type BSStream m r = Stream (Of LByteString) m r

type ChunkStream m r = Stream (Of Chunk) m r

-- | Encode a stream of 'JSONEvent's into a stream of 'ByteString'
-- chunks, applying 'style' and 'ValueOptions' and flushing output once
-- roughly @cSize@ bytes have accumulated in the buffer.
--
-- An individual output chunk may be larger than @cSize@ when a single
-- encoded event exceeds the target size; the stream contents are
-- unaffected by the chunk size.
encode :: (Monad m) => EncoderConfig -> Int -> JSONStream m r -> BSStream m r
encode config size = encodeChunks size . encodeToChunks config

-- | Buffer a stream of 'Chunk's into 'ByteString' output, flushing
-- once roughly @cSize@ bytes have accumulated. Fused pipelines feed
-- this directly without an intermediate event stream.
encodeChunks :: (Monad m) => Int -> ChunkStream m r -> BSStream m r
encodeChunks size = go mempty 0
  where
    go :: (Monad m) => Builder -> Int -> ChunkStream m r -> BSStream m r
    go !builder !size' stream = do
      nextResult <- lift $ S.next stream
      case nextResult of
        Left r ->
          if size' == 0
            then Return r
            else S.yield (flush builder) >> Return r
        Right (chunk, rest) ->
          let eventChunk = chunk
              newSize = size' + chunkSize eventChunk
              newBuilder = builder <> chunkBuilder eventChunk
           in if size' > 0 && newSize >= size
                then S.yield (flush newBuilder) >> go mempty 0 rest
                else go newBuilder newSize rest

    flush :: Builder -> LByteString
    flush = toLazyByteString
