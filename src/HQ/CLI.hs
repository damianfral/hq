{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.CLI (CLIOptions (..), optParserInfo, runCLI) where

import Data.Version (showVersion)
import HQ.JSON.Encoder (Join (..), Raw (..))
import qualified HQ.JSON.Encoder as Enc
import HQ.JSON.Event (JSONEvent)
import HQ.JSON.Parser (parseValueEvents)
import HQ.Optic (Optic)
import HQ.Optic.Parser (parseOptic)
import HQ.Query
import HQ.Runner (jsonRunner, runRunnerIOWith)
import Options.Applicative
import Paths_hq (version)
import Relude
import System.IO
import Text.Megaparsec (errorBundlePretty)

data NullInput = NoNullInput | NullInput deriving (Show, Eq)

data CLIOptions = CLIOptions
  { optQuery :: Query,
    optFile :: Maybe FilePath,
    optRaw :: Raw,
    optCompact :: Enc.EncodeStyle,
    optJoin :: Join,
    optNullInput :: NullInput
  }
  deriving (Show, Eq)

opticReader :: ReadM Optic
opticReader = eitherReader $ first errorBundlePretty . parseOptic . toText

valueReader :: ReadM [JSONEvent]
valueReader = eitherReader $ first toString . parseValueEvents . toText

queryParser :: Parser Query
queryParser =
  hsubparser
    $ command "fold" (info (Fold <$> opticArg) mempty)
    <> command "preview" (info (Preview <$> opticArg) mempty)
    <> command "set" (info (Set <$> opticArg <*> valueArg) mempty)
    <> command "delete" (info (Delete <$> opticArg) mempty)
  where
    opticArg = argument opticReader $ metavar "OPTIC"
    valueArg = argument valueReader $ metavar "VALUE"

optParser :: Parser CLIOptions
optParser = do
  file <- optional $ strOption fileMod
  raw <- fromBool NoRaw Raw <$> switch rawMod
  compact <- fromBool (Enc.Pretty 2) Enc.Compact <$> switch compactMod
  join' <- fromBool NoJoin Join <$> switch joinMod
  nullInput <- fromBool NoNullInput NullInput <$> switch nullMod
  query <- queryParser
  pure $ CLIOptions query file raw compact join' nullInput
  where
    fileHelp = "Input JSON file, or '-' for stdin"
    fileMod = long "file" <> short 'f' <> metavar "FILE" <> help fileHelp
    rawMod = long "raw" <> short 'r' <> help "Print strings without JSON quotes"
    compactMod = long "compact" <> short 'c' <> help "Print compact JSON"
    joinMod = long "join" <> short 'j' <> help "Print without separators"
    nullMod = long "null-input" <> short '0' <> help "Use null as input"
    fromBool falseV trueV b = if b then trueV else falseV

optParserInfo :: ParserInfo CLIOptions
optParserInfo = info (optParser <**> helper) infoMod
  where
    infoMod = fullDesc <> progDesc headerMsg <> header description
    headerMsg = "Query JSON using optics"
    description = "hq v" <> showVersion version

--------------------------------------------------------------------------------

readInput :: Maybe FilePath -> IO Handle
readInput Nothing = pure stdin
readInput (Just "-") = pure stdin
readInput (Just path) = openFile path ReadMode

--------------------------------------------------------------------------------

runCLI :: IO ()
runCLI = do
  CLIOptions {..} <- execParser optParserInfo
  handle <- case optNullInput of
    NullInput -> pure stdin
    NoNullInput -> readInput optFile
  let cfg = Enc.EncoderConfig optCompact $ Enc.ValueOptions optRaw optJoin
  runRunnerIOWith jsonRunner optQuery cfg handle
