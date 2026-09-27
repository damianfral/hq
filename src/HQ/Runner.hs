{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner
  ( module HQ.Runner.Cursor,
    module HQ.Runner.Take,
    module HQ.Runner.Fold,
    module HQ.Runner.Rewrite,
    RunnerEnv (..),
    RunnerF (..),
    Runner,
    runRunner,
    runRunnerIO,
    runRunnerIOWith,
    jsonRunner,
    streamHandle,
    decodeUtf8Stream,
  )
where

import Control.Monad.Error.Class (MonadError (throwError))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text.IO (hPutStrLn)
import HQ.Error (HQError (..), renderHQError)
import HQ.JSON.Decoder (StreamIO, initialDecoder)
import HQ.JSON.Encoder (BSStream, EncodeStyle (..), EncoderConfig (..), Join (..), Raw (..), ValueOptions (..), encode, encodeChunks, initialEncoderState)
import HQ.Query (Query (..))
import HQ.Runner.Cursor
import HQ.Runner.Error (RunnerError (..))
import HQ.Runner.Fold
import HQ.Runner.Rewrite
import HQ.Runner.Take
import Relude hiding (Compose, Const)
import qualified Streaming.Prelude as S
import System.IO (hSetBinaryMode)

data RunnerEnv = RunnerEnv
  { runnerEnvQuery :: Query,
    runnerEnvInput :: StreamIO ByteString (),
    runnerEnvConfig :: EncoderConfig
  }

newtype RunnerF a = Runner {unRunner :: ReaderT RunnerEnv (ExceptT HQError IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader RunnerEnv,
      MonadError HQError
    )

type Runner = RunnerF (BSStream (ExceptT HQError IO) ())

runRunner ::
  Runner ->
  Query ->
  EncoderConfig ->
  Handle ->
  ExceptT HQError IO (BSStream (ExceptT HQError IO) ())
runRunner (Runner runner) query config handle = runReaderT runner env
  where
    env = RunnerEnv query (streamHandle 256 handle) config

-- | Run a query, encoding the selected values to stdout with pretty
-- formatting and default value options.
runRunnerIO :: Runner -> Query -> Handle -> IO ()
runRunnerIO runner query = runRunnerIOWith runner query config
  where
    config = EncoderConfig (Pretty 2) (ValueOptions NoRaw NoJoin)

-- | Run a query, encoding the selected values to stdout with the given
-- style and value options.
--
-- The stream is written to stdout; errors are reported on stderr with
-- a failing exit status.
runRunnerIOWith :: Runner -> Query -> EncoderConfig -> Handle -> IO ()
runRunnerIOWith runner query encConfig handle = do
  hSetBuffering stdout $ BlockBuffering Nothing
  hSetBinaryMode stdout True
  r <- runExceptT $ do
    byteStream <- runRunner runner query encConfig handle
    S.mapM_ write byteStream
  case r of
    Left e -> hPutStrLn stderr (renderHQError e) >> exitFailure
    Right v -> pure v
  where
    write = liftIO . LBS.hPut stdout

-- | Read strict 'ByteString' chunks from a handle.
-- The input is never loaded into memory as a whole. Each chunk is pulled
-- only when the downstream parser needs more data.
--
-- NOTE: the 256-byte size is deliberate. Larger input chunks allocate
-- slightly less overall but run slower per byte (measured): the
-- decoder works better on small, cache-resident texts.
streamHandle :: Int -> Handle -> StreamIO ByteString ()
streamHandle size handle = do
  chunk <- liftIO $ BS.hGetSome handle size
  if BS.null chunk
    then pure ()
    else S.yield chunk >> streamHandle size handle

jsonRunner :: Runner
jsonRunner = do
  query <- asks runnerEnvQuery
  input <- asks runnerEnvInput
  config <- asks runnerEnvConfig
  let cursor = Cursor [] initialDecoder (decodeUtf8Stream input)
  case query of
    Preview optic -> pure (void (encode config 65536 (takeFirstValue (runFold optic cursor))))
    Fold optic -> pure (void (encode config 65536 (runFold optic cursor)))
    Over optic transformation ->
      pure (void (encodeChunks 65536 (runRewrite (RewriteTransform transformation) optic config cursor initialEncoderState)))
    Delete optic ->
      pure (void (encodeChunks 65536 (runRewrite RewriteDelete optic config cursor initialEncoderState)))

decodeUtf8Stream :: StreamIO ByteString () -> StreamIO Text ()
decodeUtf8Stream = go mempty
  where
    go :: ByteString -> StreamIO ByteString () -> StreamIO Text ()
    go leftover stream = do
      result <- lift $ S.next stream
      case result of
        Left () -> when (leftover /= mempty) $ decodeAndYield leftover
        Right (chunk, rest) -> do
          let combined = leftover <> chunk
              safeLen = safePrefixLen combined
              (safe, trailing) = BS.splitAt safeLen combined
          when (safe /= mempty) $ decodeAndYield safe
          go trailing rest

    decodeAndYield :: ByteString -> StreamIO Text ()
    decodeAndYield bs = case decodeUtf8' bs of
      Left _ -> throwError (HQRunnerError InvalidUtf8)
      Right text -> S.yield text

-- | Compute the length of the longest prefix of a strict ByteString that
-- contains only complete UTF-8 sequences.
--
-- Any trailing partial multi-byte sequence is excluded so that the
-- remaining bytes can be carried over and decoded together with the next
-- chunk. Without this, a multi-byte code point straddling a chunk
-- boundary would be decoded as two invalid fragments.
safePrefixLen :: ByteString -> Int
safePrefixLen bs = total - partialTail
  where
    total = BS.length bs
    -- Index of the lead byte of the final sequence, found by stepping
    -- back over at most three trailing continuation bytes.
    leadIdx = seekLead (total - 1) (0 :: Int)
    seekLead i continuations
      | i < 0 = -1
      | continuations > 3 = -1
      | isContinuation (BS.index bs i) = seekLead (i - 1) (continuations + 1)
      | otherwise = i

    -- Number of bytes of the final sequence that are present.
    partialTail
      | total == 0 = 0
      | leadIdx < 0 = 0
      | present >= expected = 0
      | otherwise = present
      where
        lead = BS.index bs leadIdx
        present = total - leadIdx
        expected
          | lead < 0x80 = 1
          | lead < 0xC0 = 1
          | lead < 0xE0 = 2
          | lead < 0xF0 = 3
          | otherwise = 4
    isContinuation b = b >= 0x80 && b < 0xC0
