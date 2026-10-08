{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
    transcribeRawBytes,
    transcribeRawString,
    Builder,
    Chunk (..),
    ChunkStream,
    BSStream,
    EncodeContext (..),
    EncoderState,
    initialEncoderState,
    Raw (..),
    Join (..),
    EncoderConfig (..),
  )
where

import Data.Bits (Bits (..))
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, char7, charUtf8, integerDec, string7, stringUtf8, toLazyByteString)
import Data.Scientific (Scientific, base10Exponent, coefficient)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8Builder)
import HQ.JSON.Decoder.Core (NestDepth (..), deeper, initialDepth, isStringChar, shallower)
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of, Stream)
import Streaming.Internal (Stream (..))
import Streaming.Prelude qualified as S

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

-- | How back-to-back top-level values are rendered: newlines like
-- @jq@, and bare strings like @jq -r@.
data ValueOptions = ValueOptions Raw Join deriving (Eq, Show)

-- | Container stack, head innermost. The 'Bool' marks a container that
-- already emitted a value; 'EncodeObjectAfterKey' awaits a key's value.
data EncodeContext
  = -- | Inside an array; 'True' once at least one element was emitted.
    EncodeArray !Bool
  | -- | Inside an object; 'True' once at least one pair was emitted.
    EncodeObject !Bool
  | -- | A key was just emitted; the next event is its value.
    EncodeObjectAfterKey
  deriving (Eq, Show)

-- | Encoder state threaded through transcription: container contexts
-- plus their depth, kept in sync by construction (only 'formatEvent'
-- and 'closeContainer' reshape it).
data EncoderState = EncoderState [EncodeContext] NestDepth
  deriving (Eq, Show)

initialEncoderState :: EncoderState
initialEncoderState = EncoderState [] initialDepth

-- | Encode a 'Scientific' in compact JSON-compatible form, directly to
-- a 'Chunk'.
--
-- GHC's 'show' instance for 'Scientific' produces "42.0" for integer
-- values and "1.0e10" for exponent notation, neither of which is valid
-- JSON.  This function formats numbers correctly without intermediate
-- 'Text'/'String' conversions and without @10 ^ expo@ bignum
-- multiplications: non-negative exponents append @expo@ ASCII zeros,
-- negative exponents splice the decimal point into the coefficient
-- digits. Output text is identical to the previous 'show'-based
-- implementation; only allocation is reduced.
encodeNumber :: Scientific -> Chunk
encodeNumber n
  | c == 0 = Chunk (char7 '0') 1
  | e >= 0 =
      let signB = if c < 0 then char7 '-' else mempty
          signLen = if c < 0 then 1 else 0
          mag = abs c
          digLen = integerDigits mag
          zerosB = zeroBytes e
       in Chunk (signB <> integerDec mag <> zerosB) (signLen + digLen + e)
  | otherwise =
      let signB = if c < 0 then char7 '-' else mempty
          signLen = if c < 0 then 1 else 0
          digits = show (abs c)
          len = length digits
          na = negate e
       in if len <= na
            then
              Chunk
                (signB <> string7 "0." <> zeroBytes (na - len) <> string7 digits)
                (signLen + 2 + na)
            else
              let (prefix, suffix) = splitAt (len - na) digits
               in Chunk
                    (signB <> string7 prefix <> char7 '.' <> string7 suffix)
                    (signLen + len + 1)
  where
    c = coefficient n
    e = base10Exponent n

-- | Replicated bytes as a builder.
replicateBytes :: Word8 -> Int -> Builder
replicateBytes w k
  | k <= 0 = mempty
  | otherwise = byteString (BS.replicate k w)

-- | ASCII @'0'@ bytes as a builder: a @memset@, like 'indentSpaces'.
zeroBytes :: Int -> Builder
zeroBytes = replicateBytes 0x30

-- | Decimal digit count of a non-zero magnitude. Small values (the
-- common case: ages, balances, ids) take 1-2 steps; large values loop
-- once per digit without allocating a digit string.
integerDigits :: Integer -> Int
integerDigits x = go x (0 :: Int)
  where
    go v !acc
      | v < 10 = acc + 1
      | otherwise = go (v `quot` 10) (acc + 1)

-- | Events to 'ByteString' chunks, flushing roughly every @cSize@ bytes.
-- Formats and buffers in one loop: unlike @encodeChunks . encodeToChunks@
-- there is no intermediate chunk stream (one fewer 'S.next'/'S.yield'
-- pair and no 'Chunk' box per event). The buffering policy mirrors
-- 'encodeChunks', which stays for pre-chunked streams.
encode :: (Monad m) => EncoderConfig -> Int -> JSONStream m r -> BSStream m r
encode config size = go initialEncoderState mempty 0
  where
    go :: (Monad m) => EncoderState -> Builder -> Int -> JSONStream m r -> BSStream m r
    go !st !builder !size' stream = do
      nextResult <- lift $ S.next stream
      case nextResult of
        Left r ->
          if size' == 0 then Return r else S.yield (flush builder) >> Return r
        Right (event, rest) ->
          let (Chunk eventChunk eventSize, st') = formatEvent config st event
              newSize = size' + eventSize
              newBuilder = builder <> eventChunk
           in if size' > 0 && newSize >= size
                then S.yield (flush newBuilder) >> go st' mempty 0 rest
                else go st' newBuilder newSize rest

    flush :: Builder -> LByteString
    flush = toLazyByteString

-- | Format a single event to a 'Chunk', threading the container
-- context. This is one step of 'encode', exposed so fused
-- pipelines can format events without an intermediate event stream.
formatEvent :: EncoderConfig -> EncoderState -> JSONEvent -> (Chunk, EncoderState)
formatEvent config@(EncoderConfig style (ValueOptions rawOpt joinOpt)) st event =
  case event of
    e | e == JSONEndArray || e == JSONEndObject -> closeContainerAndFinish
    JSONBeginArray -> openWith openBracket (EncodeArray False)
    JSONBeginObject -> openWith openBrace (EncodeObject False)
    JSONObjectKey _ -> case beforeKey style st of
      (Chunk sepB sepS, st') -> case encodeEvent event of
        Chunk evB evS -> (Chunk (sepB <> evB) (sepS + evS), st')
    _ -> case valueChunk event st of
      Chunk evB evS -> transcribeRawBytes config st evB evS
  where
    closeContainerAndFinish = case closeContainer style st of
      (Chunk sepB sepS, st') -> case finishTopValue joinOpt st' of
        Chunk finB finS -> (Chunk (sepB <> finB) (sepS + finS), st')

    -- Shared shape of the two container opens: separator, bracket,
    -- and a fresh empty context.
    openWith :: Builder -> EncodeContext -> (Chunk, EncoderState)
    openWith brack ctx = case beforeValue style st of
      (Chunk sepB sepS, st') ->
        (Chunk (sepB <> brack) (sepS + 1), pushCtx ctx st')

    -- Push one container context, tracking depth alongside.
    pushCtx :: EncodeContext -> EncoderState -> EncoderState
    pushCtx ctx (EncoderState ctxs depth) =
      EncoderState (ctx : ctxs) (deeper depth)

    -- Render one value event, honoring raw top-level string output.
    valueChunk :: JSONEvent -> EncoderState -> Chunk
    valueChunk (JSONString text) (EncoderState valueCtxs _)
      | rawOpt == Raw && null valueCtxs = encodeRawString text
    valueChunk ev _ = encodeEvent ev

-- | Separator after a complete top-level value: newline unless 'Join'.
finishTopValue :: Join -> EncoderState -> Chunk
finishTopValue joinOpt (EncoderState ctxs' _)
  | joinOpt == NoJoin && null ctxs' = Chunk newline 1
  | otherwise = Chunk mempty 0

-- | Transcribe a pre-encoded scalar body in one 'Chunk', exactly as
-- 'formatEvent' would for the equivalent scalar event.
transcribeRawBytes ::
  EncoderConfig -> EncoderState -> Builder -> Int -> (Chunk, EncoderState)
transcribeRawBytes (EncoderConfig style valueOpts) st bodyB bodyS =
  (Chunk (sepB <> bodyB <> finB) (sepS + bodyS + finS), st2)
  where
    (Chunk sepB sepS, st1) = beforeValue style st
    st2 = afterValue st1
    Chunk finB finS = finishTopValue joinOpt st2
    (ValueOptions _ joinOpt) = valueOpts

-- | Transcribe a raw string body; escapes stay verbatim.
transcribeRawString ::
  EncoderConfig -> EncoderState -> Builder -> Int -> (Chunk, EncoderState)
transcribeRawString config st rawB rawS =
  transcribeRawBytes config st (char7 '"' <> rawB <> char7 '"') (rawS + 2)

beforeKey :: EncodeStyle -> EncoderState -> (Chunk, EncoderState)
beforeKey style st@(EncoderState ctxs depth) = case ctxs of
  EncodeObject seen : rest ->
    ( elementSeparator style seen depth,
      EncoderState (EncodeObjectAfterKey : rest) depth
    )
  -- A key outside a container; emit it bare.
  _ -> (Chunk mempty 0, st)

-- | The structural pieces to emit before a value (a scalar or a
-- container opening).  The context is left unchanged; 'afterValue'
-- marks the parent non-empty once the value has arrived, or when a
-- nested container closes.
beforeValue :: EncodeStyle -> EncoderState -> (Chunk, EncoderState)
beforeValue style st@(EncoderState ctxs depth) = case ctxs of
  EncodeObjectAfterKey : _ -> (colonSeparator style, st)
  EncodeArray seen : _ -> member seen
  -- A value where a key was expected (malformed input); recover by
  -- treating it as an additional array-like member.
  EncodeObject seen : _ -> member seen
  -- A top-level value; no separator.
  [] -> (Chunk mempty 0, st)
  where
    member seen = (elementSeparator style seen depth, st)

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
closeContainer _ st@(EncoderState [] _) = (Chunk mempty 0, st)
closeContainer style (EncoderState (ctx : rest) depth) =
  let depth' = shallower depth
   in (closingDelimiter style depth' ctx, afterValue (EncoderState rest depth'))

-- | The closing delimiter of the innermost container.  An empty
-- container stays on one line; a non-empty one is closed on its own
-- line, indented to the depth of the container itself (the parent
-- depth).
closingDelimiter :: EncodeStyle -> NestDepth -> EncodeContext -> Chunk
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
elementSeparator Compact seen _ = if seen then Chunk comma 1 else Chunk mempty 0
elementSeparator (Pretty width) seen (NestDepth depth) =
  let size = width * depth
      prefix = if seen then commaNewline else newline
   in Chunk (prefix <> indentSpaces size) (size + 1 + fromEnum seen)

-- | Indentation via O(1) slices of a shared padding string.
indentPadding :: BS.ByteString
indentPadding = BS.replicate 256 0x20

indentSpaces :: Int -> Builder
indentSpaces size
  | size <= BS.length indentPadding = byteString (BS.take size indentPadding)
  | otherwise = replicateBytes 0x20 size

-- | Shared output fragments, built once and reused per event.
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
-- Strict fields plus strict 'case' deconstruction keep every component
-- thunk-free.
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
  JSONNumber n -> encodeNumber n
  JSONObjectKey text -> encodeString text
  JSONString text -> encodeString text

encodeString :: Text -> Chunk
encodeString text =
  case encodeStringBody text of
    Chunk body bodySize -> Chunk (char7 '"' <> body <> char7 '"') (bodySize + 2)

encodeRawString :: Text -> Chunk
encodeRawString text =
  Chunk (stringUtf8 (toString text)) (Text.length text)

-- | String body; safe runs share the input slice (no copy).
encodeStringBody :: Text -> Chunk
encodeStringBody text = let Chunk b s = go text in Chunk b s
  where
    go t
      | Text.null t = mempty
      | otherwise =
          let (safe, rest) = Text.span isStringChar t
              -- Estimated size (chars, not bytes): exact enough to drive
              -- flush decisions, and free, unlike a byte-counting pass.
              safeChunk = Chunk (encodeUtf8Builder safe) (Text.length safe)
           in case Text.uncons rest of
                Nothing -> safeChunk
                Just (c, rest') ->
                  safeChunk <> encodeEscapedChar c <> go rest'

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

-- | Buffer 'Chunk's into 'ByteString' output, flushing roughly every
-- @cSize@ bytes.
encodeChunks :: (Monad m) => Int -> ChunkStream m r -> BSStream m r
encodeChunks size = go mempty 0
  where
    go :: (Monad m) => Builder -> Int -> ChunkStream m r -> BSStream m r
    go !builder !size' stream = do
      nextResult <- lift $ S.next stream
      case nextResult of
        Left r ->
          if size' == 0 then Return r else S.yield (flush builder) >> Return r
        Right (chunk, rest) ->
          let eventChunk = chunk
              newSize = size' + chunkSize eventChunk
              newBuilder = builder <> chunkBuilder eventChunk
           in if size' > 0 && newSize >= size
                then S.yield (flush newBuilder) >> go mempty 0 rest
                else go newBuilder newSize rest

    flush :: Builder -> LByteString
    flush = toLazyByteString
