{-# LANGUAGE NoImplicitPrelude #-}

-- | Public API facade for @hq@, a JSON processor inspired by jq.
--
-- Re-exports the top-level entry point ('HQ.CLI'), the optic language
-- ('HQ.Optic'), the query/typecheck layer ('HQ.Query') and the runner
-- ('HQ.Runner').  The streaming JSON plumbing in "HQ.JSON" is
-- considered internal and is not re-exported here.
module HQ
  ( module HQ.CLI,
    module HQ.Optic,
    module HQ.Query,
    module HQ.Runner,
  )
where

import HQ.CLI
import HQ.Optic
import HQ.Query
import HQ.Runner
