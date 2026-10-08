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

import Control.Exception (evaluate, try)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding (Decoding (..), streamDecodeUtf8)
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
decodeUtf8Stream early stream = case streamDecodeUtf8 mempty of
  Some _ _ cont -> go mempty cont stream
  where
    -- The continuation carries partial sequences internally: feed it
    -- new bytes only (re-feeding the carry would decode them twice).
    -- The returned carry is only inspected at end of input, where a
    -- non-empty remainder means truncation.
    go ::
      ByteString ->
      (ByteString -> Decoding) ->
      StreamIO ByteString () ->
      StreamIO Text ()
    go leftover cont str = do
      result <- lift $ S.next str
      case result of
        Left ()
          | BS.null leftover -> pure ()
          -- A non-empty carry is a truncated sequence: invalid, exactly
          -- as the old all-or-nothing decode reported it.
          | otherwise -> lift (leave early (HQRunnerError InvalidUtf8))
        Right (chunk, rest) -> do
          Some text leftover' cont' <- lift (decodeStep (cont chunk))
          when (not $ T.null text) $ S.yield text
          go leftover' cont' rest

    -- Step the incremental decoder strictly: it throws
    -- 'UnicodeException' on invalid input, mapped here to the
    -- runner's 'InvalidUtf8' (the same errors 'decodeUtf8'' reported).
    decodeStep :: Decoding -> IO Decoding
    decodeStep decoding = do
      result <- try (evaluate decoding) :: IO (Either UnicodeException Decoding)
      case result of
        Left _ -> leave early (HQRunnerError InvalidUtf8)
        Right dec' -> pure dec'
