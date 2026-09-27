{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Every error hq can report, as data.
--
-- Domain failures keep their own precise types ('DecodeError' in
-- "HQ.JSON.Decoder.Error" for the streaming grammar,
-- 'TransformationError' in "HQ.Transformation.Error" for value
-- rewrites, 'RunnerError' in "HQ.Runner.Error" for running a query
-- over the event stream); 'HQError' only wraps them. Use
-- 'renderHQError' at the CLI boundary; nothing else renders errors.
module HQ.Error where

import HQ.JSON.Decoder.Error (DecodeError)
import HQ.Runner.Error (RunnerError, renderRunnerError)
import HQ.Transformation.Error (TransformationError, renderTransformationError)
import Relude hiding (Compose, id, many, some, state)

data HQError
  = HQDecodeError DecodeError
  | HQTransformationError TransformationError
  | HQRunnerError RunnerError
  deriving (Eq, Show)

-- | Render an error exactly as hq has always printed it: decoder
-- failures keep their derived-'Show' shape, everything else keeps its
-- historical message.
renderHQError :: HQError -> Text
renderHQError err = case err of
  HQDecodeError e -> show e
  HQTransformationError e -> renderTransformationError e
  HQRunnerError e -> renderRunnerError e
