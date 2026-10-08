{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.Runner
  ( executeRunnerAction,
    jsonRunner,
    foldDocuments,
    rewriteDocuments,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text.IO (hPutStrLn)
import HQ.Early (Early, leave, runEarly)
import HQ.Error (HQError (..), renderHQError)
import HQ.JSON.Decoder (StreamIO, initialDecoder)
import HQ.JSON.Encoder (BSStream, ChunkStream, EncoderConfig (..), EncoderState, encode, encodeChunks, initialEncoderState)
import HQ.Optic (Optic)
import HQ.Query (Query (..))
import HQ.Runner.Cursor
import HQ.Runner.Error (RunnerError (..))
import HQ.Runner.Fold
import HQ.Runner.Rewrite
import HQ.Runner.Take
import Relude hiding (Compose, Const)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S
import System.IO (hSetBinaryMode)

data RunnerEnv = RunnerEnv
  { runnerEnvEarly :: Early HQError,
    runnerEnvQuery :: Query,
    runnerEnvInput :: StreamIO ByteString (),
    runnerEnvConfig :: EncoderConfig
  }

newtype RunnerF a = Runner (ReaderT RunnerEnv IO a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader RunnerEnv
    )

type RunnerAction = RunnerF (BSStream IO ())

-- | Evaluate a runner action into its output stream, without
-- draining it. Contrast 'executeRunnerAction', which runs the stream
-- to stdout.
evalRunnerAction ::
  Early HQError ->
  RunnerAction ->
  Query ->
  EncoderConfig ->
  Handle ->
  IO (BSStream IO ())
evalRunnerAction early (Runner runner) query config handle = runReaderT runner env
  where
    env = RunnerEnv early query (streamHandle 256 handle) config

-- | Run a query to stdout; errors go to stderr with a failing exit.
executeRunnerAction :: RunnerAction -> Query -> EncoderConfig -> Handle -> IO ()
executeRunnerAction runner query encConfig handle = do
  hSetBuffering stdout $ BlockBuffering Nothing
  hSetBinaryMode stdout True
  r <- runEarly $ \early -> do
    byteStream <- evalRunnerAction early runner query encConfig handle
    S.mapM_ write byteStream
  case r of
    Left e -> hPutStrLn stderr (renderHQError e) >> exitFailure
    Right v -> pure v
  where
    write = liftIO . LBS.hPut stdout

-- | Read 'ByteString' chunks lazily from a handle. NOTE: 256 bytes is
-- deliberate; larger chunks allocate less but run slower per byte
-- (measured).
streamHandle :: Int -> Handle -> StreamIO ByteString ()
streamHandle size handle = do
  chunk <- liftIO $ BS.hGetSome handle size
  if BS.null chunk
    then pure ()
    else S.yield chunk >> streamHandle size handle

jsonRunner :: RunnerAction
jsonRunner = do
  RunnerEnv {..} <- ask
  let cursor = Cursor [] initialDecoder (decodeUtf8Stream runnerEnvEarly runnerEnvInput)
      foldWith docs = void $ encode runnerEnvConfig 65536 docs
      rewriteWith rewriter optic =
        void
          $ encodeChunks 65536
          $ rewriteDocuments
            runnerEnvEarly
            (runRewrite runnerEnvEarly rewriter optic runnerEnvConfig)
            cursor
            initialEncoderState
  pure $ case runnerEnvQuery of
    Preview optic -> foldWith (takeFirstValue (runFold runnerEnvEarly optic cursor))
    Fold optic -> foldWith (foldDocuments runnerEnvEarly optic cursor)
    Over optic transformation -> rewriteWith (RewriteTransform transformation) optic
    Delete optic -> rewriteWith RewriteDelete optic

-- | Run @act@ over every top-level document in turn, threading loop
-- state @s@ through; @project@ recovers the cursor to advance from.
-- Both 'foldDocuments' (state is the cursor) and 'rewriteDocuments'
-- (state is @(EncoderState, Cursor)@) are instances.
forDocuments ::
  (Cursor -> IO (Maybe Cursor)) ->
  (s -> Cursor) ->
  (Cursor -> s -> Stream (Of e) IO s) ->
  s ->
  Stream (Of e) IO s
forDocuments next project act st = do
  mcur <- lift (next (project st))
  case mcur of
    Nothing -> pure st
    Just cur' -> act cur' st >>= forDocuments next project act

-- | Run a fold query over every top-level document in turn, yielding
-- each document's focused values downstream.
foldDocuments :: Early HQError -> Optic -> Cursor -> EventStream Cursor
foldDocuments early optic = forDocuments (nextDocument early) id (\cur _ -> runFold early optic cur)

-- | Run a rewrite query over every top-level document in turn,
-- threading the encoder state (empty at document boundaries).
rewriteDocuments ::
  Early HQError ->
  RewriteContinuation ->
  Cursor ->
  EncoderState ->
  ChunkStream IO (EncoderState, Cursor)
rewriteDocuments early action cur st =
  forDocuments (nextDocument early) snd go (st, cur)
  where
    go cur' (st', _) = action cur' st'

decodeUtf8Stream :: Early HQError -> StreamIO ByteString () -> StreamIO Text ()
decodeUtf8Stream early = go mempty
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
      Left _ -> lift (leave early (HQRunnerError InvalidUtf8))
      Right text -> S.yield text

-- | Longest prefix of complete UTF-8 sequences; a trailing partial
-- sequence carries over to the next chunk.
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
