{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.CLI (runCLI) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import Data.Version (showVersion)
import HQ
import Options.Applicative
import Paths_hq (version)
import Relude
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
opticReader = eitherReader $ first errorBundlePretty . HQ.parseOptic . toText

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

readInput :: Maybe FilePath -> IO BL.ByteString
readInput Nothing = BL.getContents
readInput (Just "-") = BL.getContents
readInput (Just path) = BL.readFile path

formatResult :: Raw -> Value -> Text
formatResult Raw val | String s <- val = s
formatResult _ val = decodeUtf8 $ BL.toStrict $ Aeson.encode $ Aeson.toJSON val

outputResults :: Raw -> Join -> [Value] -> IO ()
outputResults raw joinResults results
  | null results = pass
  | joinResults == Join = putText $ Text.intercalate "" parts
  | otherwise = mapM_ putTextLn parts
  where
    parts = formatResult raw <$> results

--------------------------------------------------------------------------------

runCLI :: IO ()
runCLI = do
  opts <- execParser optParserInfo
  input <- case optNullInput opts of
    NullInput -> pure "null"
    NoNullInput -> readInput $ optFile opts
  case Aeson.decode input of
    Nothing -> putTextLn "Failed to parse JSON input" >> exitFailure
    Just val -> do
      let results = executeQuery (optQuery opts) [val]
      outputResults (optRaw opts) (optJoin opts) results
