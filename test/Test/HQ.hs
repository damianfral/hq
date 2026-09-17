{-# LANGUAGE NoImplicitPrelude #-}

-- | Shared helpers for the @hq@ test suite.
module Test.HQ
  ( decodeStreaming,
  )
where

import HQ.JSON.Decoder (ParseError, decode)
import HQ.JSON.Event (JSONEvent)
import Relude hiding (Compose, id)
import Streaming (Of (..))
import qualified Streaming.Prelude as S

-- | Stream text chunks through the decode function.
decodeStreaming :: [Text] -> Either ParseError [JSONEvent]
decodeStreaming chunks = runIdentity $ do
  result <- S.toList (decode (S.each chunks))
  case result of
    _ :> Left err -> pure (Left err)
    events :> Right _ -> pure (Right events)
