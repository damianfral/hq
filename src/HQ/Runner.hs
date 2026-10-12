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
import HQ.JSON.Encoder (BSStream, EmitState, EncoderConfig (..), encode, initialEmitState)
import HQ.Optic (Optic)
import HQ.Query (Query (..))
import HQ.Runner.Cursor
import HQ.Runner.Error (RunnerError (..))
import HQ.Runner.Fold
import HQ.Runner.Rewrite
import HQ.Runner.Rewrite.Emit (RewriteContinuation, RewriteResult (..))
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

-- | Evaluate a runner action into an output byte‑stream without consuming it.
-- Useful for testing or when the caller wants to further process the
-- stream (e.g., piping to another command). It returns a lazy 'BSStream' that
-- can be consumed later. Errors are propagated via the 'Early' exceptions.
evalRunnerAction ::
  Early HQError ->
  RunnerAction ->
  Query ->
  EncoderConfig ->
  Handle ->
  IO (BSStream IO ())
evalRunnerAction early (Runner runner) query config handle = runReaderT runner env
  where
    env = RunnerEnv early query (streamHandle 1024 handle) config

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

-- | Read 'ByteString' chunks lazily from a handle.
-- The chunk size defaults to 1024 bytes, which is a reasonable trade‑off for
-- typical inputs (around 100 MiB). The function yields a stream of ByteString
-- chunks until EOF.
streamHandle :: Int -> Handle -> StreamIO ByteString ()
streamHandle size handle = do
  chunk <- liftIO $ BS.hGetSome handle size
  if BS.null chunk
    then pure ()
    else S.yield chunk >> streamHandle size handle

-- | Run the JSON runner, producing a stream of encoded JSON documents.
jsonRunner :: RunnerAction
jsonRunner = do
  RunnerEnv {..} <- ask
  let cursor = Cursor [] initialDecoder (decodeUtf8Stream runnerEnvEarly runnerEnvInput)
      foldWith docs = void $ encode runnerEnvConfig 65536 docs
      rewriteWith rewriter optic =
        void
          $ rewriteDocuments
            runnerEnvEarly
            (runRewrite runnerEnvEarly rewriter optic runnerEnvConfig)
            cursor
            initialEmitState
  pure $ case runnerEnvQuery of
    Preview optic -> foldWith (takeFirstValue (runFold runnerEnvEarly optic cursor))
    Fold optic -> foldWith (foldDocuments runnerEnvEarly optic cursor)
    Over optic transformation -> rewriteWith (RewriteTransform transformation) optic
    Delete optic -> rewriteWith RewriteDelete optic

-- | Iterate over each top‑level document in the input stream.
-- The function abstracts the common pattern of fetching the next document,
-- projecting a cursor, and applying an action that may produce a new state.
-- It is used by both 'foldDocuments' and 'rewriteDocuments'.
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

-- | Perform a fold over each top‑level JSON document.
-- The fold consumes the input cursor and yields the focused values as a
-- stream of events. It is used for queries like `fold` and `preview`.
foldDocuments :: Early HQError -> Optic -> Cursor -> EventStream Cursor
foldDocuments early optic =
  forDocuments (nextDocument early) id (\cur _ -> runFold early optic cur)

-- | Perform a rewrite over each top‑level JSON document.
-- The rewrite uses a continuation that can emit transformed JSON.
-- It threads an 'EmitState' (initially empty) across documents, resetting at each document boundary.
rewriteDocuments ::
  Early HQError ->
  RewriteContinuation ->
  Cursor ->
  EmitState ->
  BSStream IO RewriteResult
rewriteDocuments early action cur st =
  forDocuments (nextDocument early) rewriteCursor go (RewriteResult st cur)
  where
    go cur' (RewriteResult st' _) = action cur' st'

-- | Decode a lazy ByteString stream to Text, handling UTF-8 errors.
-- Returns an empty stream on success, or aborts with a runner error on invalid UTF-8.
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
          unless (T.null text) $ S.yield text
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
