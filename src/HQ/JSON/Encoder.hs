{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Encoder (encode) where

import Data.Bits (Bits (..))
import Data.ByteString.Builder (Builder, char7, charUtf8, string7, stringUtf8, toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Scientific (Scientific, base10Exponent, coefficient)
import qualified Data.Text as Text
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of, Stream)
import Streaming.Internal (Stream (..))
import qualified Streaming.Prelude as S

-- | Context entries for the encoder's separator tracking.
data EncodeCtx
  = -- | Inside an array, no elements yet; skip comma.
    EncodeCtxNeedCommaInitial
  | -- | Inside an array; need ',' before next element.
    EncodeCtxNeedComma
  | -- | Inside an object, just opened; no comma before first key.
    EncodeCtxObjectInitial
  | -- | Inside an object, after a value; need ',' before next key.
    EncodeCtxObject
  | -- | Just read a key; need ':' before value.
    EncodeCtxObjectAfterKey
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

-- | Encode a stream of 'JSONEvent's into a stream of 'Text' chunks.
--
-- The encoder inserts appropriate separators (commas, colons) and
-- produces compact (no whitespace) JSON output.  It does not perform
-- string escaping — it assumes the 'JSONString' and 'JSONObjectKey'
-- events already contain properly escaped text as produced by 'decode'.
encodeToChunks :: (Monad m) => Stream (Of JSONEvent) m r -> Stream (Of Chunk) m r
encodeToChunks = go []
  where
    go :: (Monad m) => [EncodeCtx] -> Stream (Of JSONEvent) m r -> Stream (Of Chunk) m r
    go ctxs events = do
      result <- lift $ S.next events
      case result of
        Left r -> pure r
        Right (event, rest) -> case event of
          JSONEndArray -> do
            let parent' = needCommaAfterClose ctxs
            S.yield (encodeEvent event) >> go parent' rest
          JSONEndObject -> do
            S.yield (encodeEvent event)
            let parent' = needCommaAfterClose ctxs
            go parent' rest
          _ -> do
            let (sep, ctxs') = encoderSeparator ctxs
            S.yield $ Chunk (stringUtf8 $ toString sep) 1
            S.yield $ encodeEvent event
            case event of
              JSONBeginArray -> go (EncodeCtxNeedCommaInitial : ctxs') rest
              JSONBeginObject -> go (EncodeCtxObjectInitial : ctxs') rest
              JSONObjectKey _ -> do go (EncodeCtxObjectAfterKey : ctxs') rest
              JSONNull -> go (afterEncodeValue ctxs') rest
              JSONBool True -> go (afterEncodeValue ctxs') rest
              JSONBool False -> go (afterEncodeValue ctxs') rest
              JSONNumber _ -> go (afterEncodeValue ctxs') rest
              JSONString _ -> do go (afterEncodeValue ctxs') rest

    encoderSeparator :: [EncodeCtx] -> (Text, [EncodeCtx])
    encoderSeparator [] = ("", [])
    encoderSeparator (EncodeCtxNeedCommaInitial : rest) = ("", EncodeCtxNeedComma : rest)
    encoderSeparator (EncodeCtxNeedComma : rest) = (",", EncodeCtxNeedComma : rest)
    encoderSeparator (EncodeCtxObjectInitial : rest) = ("", EncodeCtxObject : rest)
    encoderSeparator (EncodeCtxObject : rest) = (",", EncodeCtxObject : rest)
    encoderSeparator (EncodeCtxObjectAfterKey : rest) =
      (":", EncodeCtxObject : rest)

    afterEncodeValue :: [EncodeCtx] -> [EncodeCtx]
    afterEncodeValue [] = []
    afterEncodeValue (EncodeCtxObjectAfterKey : rest) =
      EncodeCtxObject : rest
    afterEncodeValue (EncodeCtxNeedCommaInitial : rest) =
      EncodeCtxNeedComma : rest
    afterEncodeValue (EncodeCtxNeedComma : rest) = EncodeCtxNeedComma : rest
    afterEncodeValue (EncodeCtxObjectInitial : rest) = EncodeCtxObject : rest
    afterEncodeValue (EncodeCtxObject : rest) = EncodeCtxObject : rest

    needCommaAfterClose :: [EncodeCtx] -> [EncodeCtx]
    needCommaAfterClose [] = []
    needCommaAfterClose (_ : EncodeCtxNeedCommaInitial : rest) =
      EncodeCtxNeedComma : rest
    needCommaAfterClose (_ : EncodeCtxNeedComma : rest) =
      EncodeCtxNeedComma : rest
    needCommaAfterClose (_ : EncodeCtxObjectInitial : rest) =
      EncodeCtxObject : rest
    needCommaAfterClose (_ : EncodeCtxObject : rest) = EncodeCtxObject : rest
    needCommaAfterClose (_ : rest) = rest

data Chunk = Chunk {chunkBuilder :: !Builder, chunkSize :: !Int}

instance Semigroup Chunk where
  c1 <> c2 = Chunk (((<>) `on` chunkBuilder) c1 c2) (((+) `on` chunkSize) c1 c2)

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

encodeStringBody :: Text -> Chunk
encodeStringBody = go
  where
    go :: Text -> Chunk
    go remaining
      | Text.null remaining = mempty
      | otherwise =
          let (safe, rest) = Text.span isSafe remaining
              safeSize = utf8Length safe
              safeBuilder = stringUtf8 $ toString safe
              safeChunk = Chunk safeBuilder safeSize
           in case Text.uncons rest of
                Nothing -> Chunk safeBuilder safeSize
                Just (c, rest') ->
                  let chunk1 = encodeEscapedChar c
                      chunk2 = go rest'
                   in mconcat [safeChunk, chunk1, chunk2]

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

utf8Length :: Text -> Int
utf8Length = Text.foldl' (\size c -> size + utf8CharSize c) 0

type JSONStream m r = Stream (Of JSONEvent) m r

type BSStream m r = Stream (Of ByteString) m r

type ChunkStream m r = Stream (Of Chunk) m r

encode :: (Monad m) => Int -> JSONStream m r -> BSStream m r
encode cSize = go mempty 0 . encodeToChunks
  where
    go :: (Monad m) => Builder -> Int -> ChunkStream m r -> BSStream m r
    go !builder !size stream = do
      nextResult <- lift $ S.next stream
      case nextResult of
        Left r ->
          if size == 0
            then Return r
            else S.yield (flush builder) >> Return r
        Right (chunk, rest) ->
          let eventChunk = chunk
              newSize = size + chunkSize eventChunk
              newBuilder = builder <> chunkBuilder eventChunk
           in if size > 0 && newSize >= cSize
                then S.yield (flush newBuilder) >> go mempty 0 rest
                else go newBuilder newSize rest

    flush :: Builder -> ByteString
    flush = LBS.toStrict . toLazyByteString
