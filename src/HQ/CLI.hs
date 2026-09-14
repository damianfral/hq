{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.CLI (runCLI) where

import Data.Aeson (Value)
import Data.Version (showVersion)
import HQ.JSON.Parser (parseValue)
import HQ.Optic (Optic)
import HQ.Optic.Parser (parseOptic)
import HQ.Query
import HQ.Runner (jsonRunner, runRunnerIO)
import Options.Applicative
import Paths_hq (version)
import Relude
import System.IO
import Text.Megaparsec (errorBundlePretty)

data Raw = NoRaw | Raw deriving (Show, Eq)

data Compact = NoCompact | Compact deriving (Show, Eq)

data Join = NoJoin | Join deriving (Show, Eq)

data NullInput = NoNullInput | NullInput deriving (Show, Eq)

data CLIOptions = CLIOptions
  { optQuery :: Query,
    optFile :: Maybe FilePath,
    optRaw :: Raw,
    optCompact :: Compact,
    optJoin :: Join,
    optNullInput :: NullInput
  }
  deriving (Show, Eq)

opticReader :: ReadM Optic
opticReader = eitherReader $ first errorBundlePretty . parseOptic . toText

valueReader :: ReadM Value
valueReader = eitherReader $ first errorBundlePretty . parseValue . toText

queryParser :: Parser Query
queryParser =
  hsubparser
    $ command "fold" (info (Fold <$> opticArg) mempty)
    <> command "set" (info (Set <$> opticArg <*> valueArg) mempty)
    <> command "delete" (info (Delete <$> opticArg) mempty)
  where
    opticArg = argument opticReader $ metavar "OPTIC"
    valueArg = argument valueReader $ metavar "VALUE"

optParser :: Parser CLIOptions
optParser = do
  file <- optional $ strOption fileMod
  raw <- fromBool NoRaw Raw <$> switch rawMod
  compact <- fromBool NoCompact Compact <$> switch compactMod
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
  opts <- execParser optParserInfo
  handle <- case optNullInput opts of
    NullInput -> pure stdin
    NoNullInput -> readInput $ optFile opts
  runRunnerIO jsonRunner (optQuery opts) handle
