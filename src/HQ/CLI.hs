{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.CLI (CLIOptions (..), optParserInfo, runCLI) where

import Data.Aeson (Value)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Text.IO (hPutStrLn)
import Data.Version (showVersion)
import HQ.JSON.Encoder (Join (..), Raw (..))
import qualified HQ.JSON.Encoder as Enc
import HQ.JSON.Parser (parseValue)
import HQ.Optic (Optic)
import HQ.Optic.Parser (parseOptic)
import HQ.Query
import HQ.Runner (jsonRunner, runRunnerIOWith)
import HQ.Transformation (Transformation, constValue)
import HQ.Transformation.Parser (parseTransformation)
import Options.Applicative
import Paths_hq (version)
import Relude
import System.IO hiding (hPutStrLn, hSetBuffering)
import Text.Megaparsec (errorBundlePretty)

data CLIOptions = CLIOptions
  { optQuery :: Query,
    optFile :: Maybe FilePath,
    optRaw :: Raw,
    optCompact :: Enc.EncodeStyle,
    optJoin :: Join
  }
  deriving (Show, Eq)

opticReader :: ReadM Optic
opticReader = eitherReader $ first errorBundlePretty . parseOptic . toText

valueReader :: ReadM Value
valueReader = eitherReader $ first errorBundlePretty . parseValue . toText

transformationReader :: ReadM Transformation
transformationReader =
  eitherReader $ first errorBundlePretty . parseTransformation . toText

queryParser :: Parser Query
queryParser =
  hsubparser
    $ command "fold" (info (Fold <$> opticArg) mempty)
    <> command "preview" (info (Preview <$> opticArg) mempty)
    <> command "set" (info (Over <$> opticArg <*> (constValue <$> valueArg)) mempty)
    <> command "over" (info (Over <$> opticArg <*> transformationArg) mempty)
    <> command "delete" (info (Delete <$> opticArg) mempty)
  where
    opticArg = argument opticReader $ metavar "OPTIC"
    valueArg = argument valueReader $ metavar "VALUE"
    transformationArg = argument transformationReader $ metavar "TRANSFORMATION"

optParser :: Parser CLIOptions
optParser = do
  file <- optional $ strOption fileMod
  raw <- fromBool NoRaw Raw <$> switch rawMod
  compact <- fromBool (Enc.Pretty 2) Enc.Compact <$> switch compactMod
  join' <- fromBool NoJoin Join <$> switch joinMod
  query <- queryParser
  pure $ CLIOptions query file raw compact join'
  where
    fileHelp = "Input JSON file, or '-' for stdin"
    fileMod = long "file" <> short 'f' <> metavar "FILE" <> help fileHelp
    rawMod = long "raw" <> short 'r' <> help "Print strings without JSON quotes"
    compactMod = long "compact" <> short 'c' <> help "Print compact JSON"
    joinMod = long "join" <> short 'j' <> help "Print without separators"
    fromBool falseV trueV b = if b then trueV else falseV

langHelp :: String
langHelp = $(embedStringFile =<< makeRelativeToProject "./docs/LANG.txt")

optParserInfo :: ParserInfo CLIOptions
optParserInfo = info (optParser <**> helper) infoMod
  where
    infoMod =
      mconcat
        [ fullDesc,
          progDesc headerMsg,
          header description,
          footerDoc $ Just $ fromString langHelp
        ]
    headerMsg = "Query JSON using optics"
    description = "hq v" <> showVersion version

--------------------------------------------------------------------------------

readInput :: Maybe FilePath -> IO Handle
readInput Nothing = pure stdin
readInput (Just "-") = pure stdin
readInput (Just path) = do
  h <- openFile path ReadMode
  hSetBuffering h $ BlockBuffering Nothing
  hSetBinaryMode h True
  pure h

--------------------------------------------------------------------------------

runCLI :: IO ()
runCLI = do
  CLIOptions {..} <- execParser optParserInfo
  query <- case typecheckQuery optQuery of
    Left err -> hPutStrLn stderr (renderTypeError err) >> exitFailure
    Right q -> pure q
  handle <- readInput optFile
  let cfg = Enc.EncoderConfig optCompact $ Enc.ValueOptions optRaw optJoin
  runRunnerIOWith jsonRunner query cfg handle
