{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Every error hq can report, with per-domain precise types.
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

-- | Render an error for the CLI boundary; nothing else renders errors.
renderHQError :: HQError -> Text
renderHQError err = case err of
  HQDecodeError e -> show e
  HQTransformationError e -> renderTransformationError e
  HQRunnerError e -> renderRunnerError e
