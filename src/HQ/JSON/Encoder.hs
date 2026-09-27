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
    Raw (..),
    Join (..),
    EncoderConfig (..),
  )
where

import Data.Bits (Bits (..))
import Data.ByteString.Builder (Builder, char7, charUtf8, string7, stringUtf8, toLazyByteString)
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
encodeToChunks config = go []
  where
    go ctxs events = do
      result <- lift $ S.next events
      case result of
        Left r -> pure r
        Right (event, rest) ->
          let (chunk, ctxs') = formatEvent config ctxs event
           in S.yield chunk >> go ctxs' rest

-- | Format a single event to a 'Chunk', threading the container
-- context. This is one step of 'encodeToChunks', exposed so fused
-- pipelines can format events without an intermediate event stream.
formatEvent :: EncoderConfig -> [EncodeCtx] -> JSONEvent -> (Chunk, [EncodeCtx])
formatEvent (EncoderConfig style (ValueOptions rawOption joinOption)) ctxs event
  | event == JSONEndArray || event == JSONEndObject =
      let (sep, ctxs') = closeContainer style ctxs
       in (sep <> finishValue ctxs', ctxs')
  | otherwise = case event of
      JSONBeginArray ->
        let (sep, ctxs') = beforeValue style ctxs
         in (sep <> encodeEvent event, EncodeArray False : ctxs')
      JSONBeginObject ->
        let (sep, ctxs') = beforeValue style ctxs
         in (sep <> encodeEvent event, EncodeObject False : ctxs')
      JSONObjectKey _ ->
        let (sep, ctxs') = beforeKey style ctxs
         in (sep <> encodeEvent event, ctxs')
      _ ->
        let (sep, ctxs') = beforeValue style ctxs
            ctxs'' = afterValue ctxs'
         in (sep <> valueChunk event ctxs <> finishValue ctxs'', ctxs'')
  where
    -- Render one value event, honoring raw top-level string output.
    valueChunk :: JSONEvent -> [EncodeCtx] -> Chunk
    valueChunk (JSONString text) valueCtxs
      | rawOption == Raw && null valueCtxs = encodeRawString text
    valueChunk ev _ = encodeEvent ev

    -- Separator emitted after a complete top-level value.  A top-level
    -- value is one that leaves the context stack empty.
    finishValue :: [EncodeCtx] -> Chunk
    finishValue ctxs'
      | joinOption == NoJoin && null ctxs' = Chunk (char7 '\n') 1
      | otherwise = mempty

-- | The structural pieces to emit before an object key: a separator for
-- the first or any following key, and the switch to
-- 'EncodeObjectAfterKey' so the upcoming value gets its colon.
beforeKey :: EncodeStyle -> [EncodeCtx] -> (Chunk, [EncodeCtx])
beforeKey style ctxs = case ctxs of
  EncodeObject seen : rest ->
    ( elementSeparator style seen (length rest + 1),
      EncodeObjectAfterKey : rest
    )
  -- A key outside a container; emit it bare.
  _ -> (mempty, ctxs)

-- | The structural pieces to emit before a value (a scalar or a
-- container opening).  The context is left unchanged; 'afterValue'
-- marks the parent non-empty once the value has arrived, or when a
-- nested container closes.
beforeValue :: EncodeStyle -> [EncodeCtx] -> (Chunk, [EncodeCtx])
beforeValue style ctxs = case ctxs of
  EncodeObjectAfterKey : _ -> (colonSeparator style, ctxs)
  EncodeArray seen : rest ->
    (elementSeparator style seen (length rest + 1), ctxs)
  -- A value where a key was expected (malformed input); recover by
  -- treating it as an additional array-like member.
  EncodeObject seen : rest ->
    (elementSeparator style seen (length rest + 1), ctxs)
  -- A top-level value; no separator.
  [] -> (mempty, ctxs)

-- | After a value was emitted, mark the innermost container non-empty.
afterValue :: [EncodeCtx] -> [EncodeCtx]
afterValue = \case
  [] -> []
  EncodeArray _ : rest -> EncodeArray True : rest
  EncodeObject _ : rest -> EncodeObject True : rest
  EncodeObjectAfterKey : rest -> EncodeObject True : rest

-- | Emit the closing delimiter for a container, then pop it and mark
-- the parent container as having received a value.
closeContainer :: EncodeStyle -> [EncodeCtx] -> (Chunk, [EncodeCtx])
closeContainer _ [] = (mempty, [])
closeContainer style (ctx : rest) =
  (closingDelimiter style (length rest) ctx, afterValue rest)

-- | The closing delimiter of the innermost container.  An empty
-- container stays on one line; a non-empty one is closed on its own
-- line, indented to the depth of the container itself (the parent
-- depth).
closingDelimiter :: EncodeStyle -> Int -> EncodeCtx -> Chunk
closingDelimiter style depth ctx = case ctx of
  EncodeArray seen -> close seen ']'
  EncodeObject seen -> close seen '}'
  EncodeObjectAfterKey -> close True '}'
  where
    close seen delim = case style of
      Pretty width
        | seen ->
            let size = width * depth
                s = '\n' : replicate size ' ' ++ [delim]
             in Chunk (string7 s) (size + 2)
      _ -> Chunk (char7 delim) 1

-- | The chunk emitted before an array element or an object key.
--
-- In 'Compact' mode this is just a comma (or nothing for the first
-- item); in 'Pretty' mode a newline (preceded by a comma except for the
-- first item) and indentation to @depth@.
elementSeparator :: EncodeStyle -> Bool -> Int -> Chunk
elementSeparator Compact seen _ =
  if seen then Chunk (char7 ',') 1 else mempty
elementSeparator (Pretty width) seen depth =
  let size = width * depth
      prefix = if seen then ",\n" else "\n"
      s = prefix ++ replicate size ' '
   in Chunk (string7 s) (size + 1 + fromEnum seen)

-- | The chunk emitted between an object key and its value.
colonSeparator :: EncodeStyle -> Chunk
colonSeparator Compact = Chunk (char7 ':') 1
colonSeparator (Pretty _) = Chunk (string7 ": ") 2

-- | An output fragment with its size. The size is a flush heuristic
-- (character count, not exact bytes for non-ASCII), so chunk
-- boundaries may shift with content; output bytes are unaffected.
data Chunk = Chunk {chunkBuilder :: !Builder, chunkSize :: !Int}

instance Semigroup Chunk where
  Chunk b1 s1 <> Chunk b2 s2 = let !s = s1 + s2 in Chunk (b1 <> b2) s

instance Monoid Chunk where mempty = Chunk mempty 0

encodeEvent :: JSONEvent -> Chunk
encodeEvent = \case
  JSONBeginObject -> Chunk (char7 '{') 1
  JSONEndObject -> Chunk (char7 '}') 1
  JSONBeginArray -> Chunk (char7 '[') 1
  JSONEndArray -> Chunk (char7 ']') 1
  JSONNull -> Chunk (string7 "null") 4
  JSONBool True -> Chunk (string7 "true") 4
  JSONBool False -> Chunk (string7 "false") 5
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
