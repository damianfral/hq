{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Decoder where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Bits (Bits (..), shiftL)
import Data.Char (digitToInt, isDigit, isHexDigit)
import Data.Scientific (Scientific, scientific)
import qualified Data.Text as Text
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of, Stream)
import qualified Streaming.Prelude as S

data Decoder = Decoder
  { decoderInput :: Text,
    decoderContext :: [Context],
    decoderRoot :: RootState,
    decoderLex :: LexState
  }
  deriving (Show, Eq)

data RootState = RootExpectValue | RootDone
  deriving (Eq, Show)

data Context = Object ObjectState | Array ArrayState
  deriving (Eq, Show)

data ObjectState
  = ObjectExpectKey
  | ObjectExpectColon
  | ObjectExpectValue
  | ObjectExpectComma
  deriving (Eq, Show)

data ArrayState = ArrayExpectValue | ArrayExpectComma
  deriving (Eq, Show)

data LexState
  = LexNone
  | LexString StringState
  | LexNumber NumberState
  | LexKeyword KeywordState
  deriving (Eq, Show)

-- | JSON number state machine.
data NumberState = NumberState
  { numberBuffer :: Text,
    numberPhase :: NumberPhase
  }
  deriving (Eq, Show)

-- | Tracks how much of a JSON number has been consumed.
-- -? (0 | [1-9][0-9]*) (\.[0-9]+)? ([eE][+-]?[0-9]+)?
data NumberPhase
  = -- | Saw leading '-'
    NumSign
  | -- | Saw '0' (must not be followed by more digits)
    NumZero
  | -- | Saw [1-9] and possibly more digits
    NumNonZero
  | -- | Saw '.' after integer part (need fractional digits)
    NumDot
  | -- | Saw '.' + at least one digit
    NumFraction
  | -- | Saw 'e'/'E' (waiting for optional sign or digit)
    NumExpSign
  | -- | Saw 'e'/'E' then '+'/'-' (need digits)
    NumExpAfterSign
  | -- | Saw exponent digits
    NumExpDigit
  deriving (Eq, Show)

-- | Is this phase valid at end-of-input?
isValidNumberFinal :: NumberPhase -> Bool
isValidNumberFinal NumZero = True
isValidNumberFinal NumNonZero = True
isValidNumberFinal NumFraction = True
isValidNumberFinal NumExpDigit = True
isValidNumberFinal _ = False

-- | Result of checking a character against the number state machine.
data NumberStep
  = -- | Valid continuation, advance to this phase
    NumStep NumberPhase
  | -- | Delimiter, number is complete
    NumEnd
  | -- | Invalid character in this context
    NumError

-- | Advance the number state machine for one character.
advanceNumber :: NumberPhase -> Char -> NumberStep
advanceNumber phase c
  | isJsonDelimiter c = NumEnd
  | otherwise = case phase of
      NumSign
        | c == '0' -> NumStep NumZero
        | isDigit c -> NumStep NumNonZero
        | otherwise -> NumError
      NumZero
        | c == '.' -> NumStep NumDot
        | c == 'e' || c == 'E' -> NumStep NumExpSign
        | otherwise -> NumError
      NumNonZero
        | isDigit c -> NumStep NumNonZero
        | c == '.' -> NumStep NumDot
        | c == 'e' || c == 'E' -> NumStep NumExpSign
        | otherwise -> NumError
      NumDot
        | isDigit c -> NumStep NumFraction
        | otherwise -> NumError
      NumFraction
        | isDigit c -> NumStep NumFraction
        | c == 'e' || c == 'E' -> NumStep NumExpSign
        | otherwise -> NumError
      NumExpSign
        | isDigit c -> NumStep NumExpDigit
        | c == '+' || c == '-' -> NumStep NumExpAfterSign
        | otherwise -> NumError
      NumExpAfterSign
        | isDigit c -> NumStep NumExpDigit
        | otherwise -> NumError
      NumExpDigit
        | isDigit c -> NumStep NumExpDigit
        | otherwise -> NumError

-- | Is this character a valid JSON delimiter (one that can follow a number)?
isJsonDelimiter :: Char -> Bool
isJsonDelimiter c = isWhitespace c || c `elem` [',', ']', '}']

-- | Determine the starting number phase for the first character of a number.
numberPhaseFromFirstChar :: Char -> Maybe NumberPhase
numberPhaseFromFirstChar '-' = Just NumSign
numberPhaseFromFirstChar '0' = Just NumZero
numberPhaseFromFirstChar c
  | isDigit c = Just NumNonZero
  | otherwise = Nothing

-- | Create an initial NumberState from the first character.
startNumberState :: Char -> NumberState
startNumberState c =
  NumberState (one c) (fromMaybe NumSign (numberPhaseFromFirstChar c))

data StringTarget = StringKey | StringValue
  deriving (Eq, Show)

data BufferedStringTarget = BufferedStringTarget StringTarget Text
  deriving (Eq, Show)

data Unicode = Unicode {unicodeValue :: Int, unicodeDigits :: Int}
  deriving (Eq, Show)

data StringState
  = InString BufferedStringTarget
  | AfterEscape BufferedStringTarget
  | InUnicodeEscape BufferedStringTarget Unicode
  | AfterHighSurrogate BufferedStringTarget Int
  deriving (Eq, Show)

data KeywordState
  = KeywordNull !Int
  | KeywordTrue !Int
  | KeywordFalse !Int
  deriving (Eq, Show)

data DecoderResult
  = Emit JSONEvent Decoder
  | NeedInput Decoder
  | Done Decoder
  deriving (Eq, Show)

data ParseError
  = UnexpectedEnd
  | UnexpectedChar Char
  | UnexpectedToken Text
  | ExpectedColon
  | ExpectedCommaOrEnd
  | ExpectedObjectKey
  | ExpectedValue
  | InvalidEscape Char
  | InvalidUnicodeEscape
  | InvalidSurrogatePair
  | InvalidNumber Text
  | InvalidKeyword Text
  | TrailingInput
  deriving (Eq, Show)

initialDecoder :: Decoder
initialDecoder =
  Decoder
    { decoderInput = mempty,
      decoderContext = [],
      decoderRoot = RootExpectValue,
      decoderLex = LexNone
    }

feed :: Text -> Decoder -> Either ParseError DecoderResult
feed input decoder = step decoder {decoderInput = decoderInput decoder <> input}

finish :: Decoder -> Either ParseError DecoderResult
finish decoder = case decoderLex decoder of
  LexNone -> case decoderRoot decoder of
    RootExpectValue -> Left UnexpectedEnd
    RootDone -> case decoderContext decoder of
      [] ->
        let isNull = Text.null (decoderInput decoder)
         in if isNull then Right (Done decoder) else Left TrailingInput
      _ -> Left UnexpectedEnd
  LexString (AfterHighSurrogate _ _) -> Left InvalidSurrogatePair
  LexString _ -> Left UnexpectedEnd
  LexNumber numState
    | isValidNumberFinal (numberPhase numState) -> finalizeNumber decoder
    | otherwise -> Left (InvalidNumber (numberBuffer numState))
  LexKeyword _ -> finalizeKeyword decoder

step :: Decoder -> Either ParseError DecoderResult
step decoder = case decoderLex decoder of
  LexNone -> stepStructural decoder
  LexString state -> stepString decoder state
  LexNumber numState -> stepNumber decoder numState
  LexKeyword state -> stepKeyword decoder state

stepStructural :: Decoder -> Either ParseError DecoderResult
stepStructural decoder = case Text.uncons input of
  Nothing -> case decoderRoot decoder of
    RootExpectValue -> Right (NeedInput decoder {decoderInput = mempty})
    RootDone ->
      let newDecoder = decoder {decoderInput = mempty}
       in Right
            $ if null (decoderContext decoder)
              then Done newDecoder
              else NeedInput newDecoder
  Just (c, rest) -> parseStructuralChar c rest decoder
  where
    input = Text.dropWhile isWhitespace (decoderInput decoder)

parseStructuralChar ::
  Char -> Text -> Decoder -> Either ParseError DecoderResult
parseStructuralChar c rest decoder = case decoderContext decoder of
  [] -> parseRootChar c rest decoder
  Object state : contexts -> parseObjectChar c rest state decoder contexts
  Array state : contexts -> parseArrayChar c rest state decoder contexts

parseRootChar :: Char -> Text -> Decoder -> Either ParseError DecoderResult
parseRootChar c rest decoder = case decoderRoot decoder of
  RootDone -> Left TrailingInput
  RootExpectValue -> startValue c rest decoder

parseObjectChar ::
  Char ->
  Text ->
  ObjectState ->
  Decoder ->
  [Context] ->
  Either ParseError DecoderResult
parseObjectChar c rest state decoder contexts = case state of
  ObjectExpectKey
    | c == '}' ->
        let newDecoder = decoder {decoderContext = contexts}
         in emitContainerEnd JSONEndObject rest newDecoder
    | c == '"' -> startString StringKey rest decoder
    | otherwise -> Left ExpectedObjectKey
  ObjectExpectColon
    | c == ':' ->
        step
          decoder
            { decoderInput = rest,
              decoderContext = Object ObjectExpectValue : contexts
            }
    | otherwise -> Left ExpectedColon
  ObjectExpectValue -> startValue c rest decoder
  ObjectExpectComma
    | c == ',' ->
        step
          decoder
            { decoderInput = rest,
              decoderContext = Object ObjectExpectKey : contexts
            }
    | c == '}' ->
        let newDecoder = decoder {decoderContext = contexts}
         in emitContainerEnd JSONEndObject rest newDecoder
    | otherwise -> Left ExpectedCommaOrEnd

parseArrayChar ::
  Char ->
  Text ->
  ArrayState ->
  Decoder ->
  [Context] ->
  Either ParseError DecoderResult
parseArrayChar c rest state decoder contexts = case state of
  ArrayExpectValue
    | c == ']' ->
        emitContainerEnd JSONEndArray rest decoder {decoderContext = contexts}
    | otherwise -> startValue c rest decoder
  ArrayExpectComma
    | c == ',' ->
        step
          decoder
            { decoderInput = rest,
              decoderContext = Array ArrayExpectValue : contexts
            }
    | c == ']' ->
        emitContainerEnd JSONEndArray rest decoder {decoderContext = contexts}
    | otherwise -> Left ExpectedCommaOrEnd

startValue :: Char -> Text -> Decoder -> Either ParseError DecoderResult
startValue c rest decoder = case c of
  '{' ->
    Right
      $ Emit
        JSONBeginObject
        decoder
          { decoderInput = rest,
            decoderContext = Object ObjectExpectKey : decoderContext decoder,
            decoderRoot = rootAfterOpeningContainer decoder
          }
  '[' ->
    Right
      $ Emit
        JSONBeginArray
        decoder
          { decoderInput = rest,
            decoderContext = Array ArrayExpectValue : decoderContext decoder,
            decoderRoot = rootAfterOpeningContainer decoder
          }
  '"' -> startString StringValue rest decoder
  'n' -> startKeyword decoder rest "null" 1
  't' -> startKeyword decoder rest "true" 1
  'f' -> startKeyword decoder rest "false" 1
  _
    | isNumberStart c ->
        let numState = startNumberState c
         in stepNumber
              decoder {decoderInput = rest, decoderLex = LexNumber numState}
              numState
    | otherwise -> Left (UnexpectedChar c)

rootAfterOpeningContainer :: Decoder -> RootState
rootAfterOpeningContainer _ = RootExpectValue

startString :: StringTarget -> Text -> Decoder -> Either ParseError DecoderResult
startString target input = consumeString target input mempty

consumeString ::
  StringTarget -> Text -> Text -> Decoder -> Either ParseError DecoderResult
consumeString target input buffer decoder = case Text.uncons rest of
  Nothing ->
    let bufferedTarget = BufferedStringTarget target newBuffer
        lexString = LexString (InString bufferedTarget)
        decoder' = decoder {decoderInput = mempty, decoderLex = lexString}
     in Right $ NeedInput decoder'
  Just (c, rest')
    | c == '"' -> finishString target newBuffer rest' decoder
    | c == '\\' -> consumeStringEscape target rest' newBuffer decoder
    | otherwise -> Left (UnexpectedChar c)
  where
    (chunk, rest) =
      Text.span (\c -> c /= '"' && c /= '\\' && ord c >= 0x20) input
    !newBuffer = if Text.null buffer then chunk else buffer <> chunk

consumeStringEscape ::
  StringTarget -> Text -> Text -> Decoder -> Either ParseError DecoderResult
consumeStringEscape target input buffer decoder = case Text.uncons input of
  Nothing ->
    let lexString = LexString (AfterEscape (BufferedStringTarget target buffer))
     in Right $ NeedInput decoder {decoderInput = mempty, decoderLex = lexString}
  Just (c, rest) -> case c of
    '"' -> consumeString target rest (Text.snoc buffer '"') decoder
    '\\' -> consumeString target rest (Text.snoc buffer '\\') decoder
    '/' -> consumeString target rest (Text.snoc buffer '/') decoder
    'b' -> consumeString target rest (Text.snoc buffer '\b') decoder
    'f' -> consumeString target rest (Text.snoc buffer '\f') decoder
    'n' -> consumeString target rest (Text.snoc buffer '\n') decoder
    'r' -> consumeString target rest (Text.snoc buffer '\r') decoder
    't' -> consumeString target rest (Text.snoc buffer '\t') decoder
    'u' -> consumeUnicode target rest buffer decoder
    _ -> Left (InvalidEscape c)

consumeUnicode ::
  StringTarget -> Text -> Text -> Decoder -> Either ParseError DecoderResult
consumeUnicode target input buffer = consumeUnicode' target input buffer 0 0

consumeUnicode' ::
  StringTarget ->
  Text ->
  Text ->
  Int ->
  Int ->
  Decoder ->
  Either ParseError DecoderResult
consumeUnicode' target input buffer value digits decoder
  | digits == 4 = finishUnicode target input buffer value decoder
  | otherwise =
      case Text.uncons input of
        Nothing ->
          let unicode = Unicode value digits
              bufferedString = BufferedStringTarget target buffer
              lexString = LexString $ InUnicodeEscape bufferedString unicode
              newDecoder = decoder {decoderInput = mempty, decoderLex = lexString}
           in Right $ NeedInput newDecoder
        Just (c, rest)
          | isHexDigit c ->
              let digit = digitToInt c
                  newDigits = digits + 1
                  newValue = value * 16 + digit
               in consumeUnicode' target rest buffer newValue newDigits decoder
        _ -> Left InvalidUnicodeEscape

finishUnicode ::
  StringTarget ->
  Text ->
  Text ->
  Int ->
  Decoder ->
  Either ParseError DecoderResult
finishUnicode target input buffer value decoder
  | isHighSurrogate value =
      let bufferedStringT = BufferedStringTarget target buffer
          lexString = LexString $ AfterHighSurrogate bufferedStringT value
          newDecoder = decoder {decoderInput = input, decoderLex = lexString}
       in Right $ NeedInput newDecoder
  | isLowSurrogate value = Left InvalidSurrogatePair
  | otherwise =
      consumeString target input (Text.snoc buffer (chr value)) decoder

stepString :: Decoder -> StringState -> Either ParseError DecoderResult
stepString decoder state =
  case state of
    InString (BufferedStringTarget target buffer) ->
      consumeString target (decoderInput decoder) buffer decoder
    AfterEscape (BufferedStringTarget target buffer) ->
      consumeStringEscape target (decoderInput decoder) buffer decoder
    InUnicodeEscape (BufferedStringTarget target buffer) (Unicode value digits) ->
      consumeUnicode' target (decoderInput decoder) buffer value digits decoder
    AfterHighSurrogate (BufferedStringTarget target buffer) high ->
      consumeLowSurrogate target (decoderInput decoder) buffer high decoder

consumeLowSurrogate ::
  StringTarget ->
  Text ->
  Text ->
  Int ->
  Decoder ->
  Either ParseError DecoderResult
consumeLowSurrogate target input buffer high decoder = case Text.uncons input of
  Nothing -> Right $ NeedInput decoder {decoderInput = mempty}
  Just ('\\', rest) -> case Text.uncons rest of
    Nothing ->
      Right
        $ NeedInput
          decoder
            { decoderInput = mempty,
              decoderLex =
                LexString
                  $ AfterHighSurrogate (BufferedStringTarget target buffer) high
            }
    Just ('u', rest') ->
      consumeLowSurrogateDigits target rest' buffer high 0 0 decoder
    _ -> Left InvalidSurrogatePair
  _ -> Left InvalidSurrogatePair

consumeLowSurrogateDigits ::
  StringTarget ->
  Text ->
  Text ->
  Int ->
  Int ->
  Int ->
  Decoder ->
  Either ParseError DecoderResult
consumeLowSurrogateDigits target input buffer high value digits decoder
  | digits == 4 =
      if isLowSurrogate value
        then
          let codepoint = 0x10000 + ((high - 0xD800) `shiftL` 10) + (value - 0xDC00)
              newBuffer = Text.snoc buffer (chr codepoint)
           in consumeString target input newBuffer decoder
        else Left InvalidSurrogatePair
  | otherwise = case Text.uncons input of
      Nothing ->
        Right
          $ NeedInput
            decoder
              { decoderInput = mempty,
                decoderLex =
                  LexString
                    $ InUnicodeEscape
                      (BufferedStringTarget target buffer)
                      (Unicode value digits)
              }
      Just (c, rest)
        | isHexDigit c ->
            let newValue = value * 16 + digitToInt c
             in consumeLowSurrogateDigits target rest buffer high newValue (digits + 1) decoder
        | otherwise -> Left InvalidUnicodeEscape

finishString ::
  StringTarget -> Text -> Text -> Decoder -> Either ParseError DecoderResult
finishString target value remaining decoder = case target of
  StringValue -> emitScalar (JSONString value) remaining decoder
  StringKey ->
    Right
      $ Emit
        (JSONObjectKey value)
        decoder
          { decoderInput = remaining,
            decoderContext = setObjectState ObjectExpectColon (decoderContext decoder),
            decoderLex = LexNone
          }

setObjectState :: ObjectState -> [Context] -> [Context]
setObjectState state contexts = case contexts of
  Object _ : rest -> Object state : rest
  _ -> contexts

-- | Continue parsing a number from the saved state.
stepNumber :: Decoder -> NumberState -> Either ParseError DecoderResult
stepNumber decoder numState = case Text.uncons (decoderInput decoder) of
  Nothing ->
    Right
      $ NeedInput
        decoder
          { decoderInput = mempty,
            decoderLex = LexNumber numState
          }
  Just (c, rest) -> case advanceNumber (numberPhase numState) c of
    NumEnd ->
      finalizeNumber
        decoder
          { decoderInput = Text.cons c rest,
            decoderLex = LexNumber numState
          }
    NumError -> Left (InvalidNumber (numberBuffer numState <> one c))
    NumStep nextPhase ->
      let numState' =
            numState
              { numberBuffer = Text.snoc (numberBuffer numState) c,
                numberPhase = nextPhase
              }
       in stepNumber decoder {decoderInput = rest} numState'

finalizeNumber :: Decoder -> Either ParseError DecoderResult
finalizeNumber decoder = case decoderLex decoder of
  LexNumber numState
    | isValidNumberFinal (numberPhase numState) ->
        let value = parseNumberBuffer (numberBuffer numState)
         in emitScalar
              (JSONNumber value)
              (decoderInput decoder)
              decoder {decoderLex = LexNone}
    | otherwise -> Left (InvalidNumber (numberBuffer numState))
  _ -> Left (InvalidNumber mempty)

-- | Parse the accumulated number buffer into a Scientific value.
-- We build it manually to stay within the JSON grammar.
parseNumberBuffer :: Text -> Scientific
parseNumberBuffer buf = go (0 :: Integer) (0 :: Integer) False False False (0 :: Integer) (0 :: Int) 0
  where
    go :: Integer -> Integer -> Bool -> Bool -> Bool -> Integer -> Int -> Int -> Scientific
    go !intP !fracP !hasDot !hasExp !expNeg !expP !nFrac !pos
      | pos >= Text.length buf =
          let signed =
                if not (Text.null buf) && Text.take 1 buf == "-"
                  then negate
                  else identity
              coeff = signed (intP * 10 ^ nFrac + fracP)
              expVal = (if expNeg then negate else identity) expP
              finalExp = fromInteger (expVal - fromIntegral nFrac :: Integer) :: Int
           in coeff `scientific` finalExp
      | otherwise =
          let c = Text.index buf pos
           in case c of
                '-' -> go intP fracP hasDot hasExp True expP nFrac (pos + 1)
                '+' -> go intP fracP hasDot hasExp False expP nFrac (pos + 1)
                '.' -> go intP fracP True hasExp expNeg expP nFrac (pos + 1)
                'e' -> go intP fracP hasDot True expNeg expP nFrac (pos + 1)
                'E' -> go intP fracP hasDot True expNeg expP nFrac (pos + 1)
                _
                  | isDigit c ->
                      let d = fromIntegral (digitToInt c) :: Integer
                       in if hasExp
                            then
                              go
                                intP
                                fracP
                                hasDot
                                True
                                expNeg
                                (expP * 10 + d)
                                nFrac
                                (pos + 1)
                            else
                              if hasDot
                                then
                                  go
                                    intP
                                    (fracP * 10 + d)
                                    True
                                    hasExp
                                    expNeg
                                    expP
                                    (nFrac + 1)
                                    (pos + 1)
                                else
                                  go
                                    (intP * 10 + d)
                                    fracP
                                    hasDot
                                    hasExp
                                    expNeg
                                    expP
                                    nFrac
                                    (pos + 1)
                _ -> go intP fracP hasDot hasExp expNeg expP nFrac (pos + 1)

startKeyword :: Decoder -> Text -> Text -> Int -> Either ParseError DecoderResult
startKeyword decoder input keyword consumed =
  let state = keywordState keyword consumed
   in stepKeyword
        decoder {decoderInput = input, decoderLex = LexKeyword state}
        state

keywordState :: Text -> Int -> KeywordState
keywordState keyword n
  | keyword == "null" = KeywordNull n
  | keyword == "true" = KeywordTrue n
  | keyword == "false" = KeywordFalse n
  | otherwise = KeywordNull n

stepKeyword :: Decoder -> KeywordState -> Either ParseError DecoderResult
stepKeyword decoder state =
  let (keyword, index) = case state of
        KeywordNull i -> ("null", i)
        KeywordTrue i -> ("true", i)
        KeywordFalse i -> ("false", i)
   in if index == Text.length keyword
        then case Text.uncons (decoderInput decoder) of
          Nothing -> finalizeKeyword decoder
          Just (c, _) | isJsonDelimiter c -> finalizeKeyword decoder
          Just _ -> Left (InvalidKeyword keyword)
        else case Text.uncons (decoderInput decoder) of
          Nothing -> Right $ NeedInput decoder {decoderLex = LexKeyword state}
          Just (c, rest)
            | c == Text.index keyword index ->
                stepKeyword
                  decoder
                    { decoderInput = rest,
                      decoderLex = LexKeyword (advanceKeyword state)
                    }
                  (advanceKeyword state)
          _ -> Left (InvalidKeyword keyword)

advanceKeyword :: KeywordState -> KeywordState
advanceKeyword state = case state of
  KeywordNull n -> KeywordNull (n + 1)
  KeywordTrue n -> KeywordTrue (n + 1)
  KeywordFalse n -> KeywordFalse (n + 1)

finalizeKeyword :: Decoder -> Either ParseError DecoderResult
finalizeKeyword decoder = case decoderLex decoder of
  LexKeyword state ->
    let (keyword, index) = case state of
          KeywordNull i -> ("null", i)
          KeywordTrue i -> ("true", i)
          KeywordFalse i -> ("false", i)
     in if index == Text.length keyword
          then
            let event = case state of
                  KeywordNull _ -> JSONNull
                  KeywordTrue _ -> JSONBool True
                  KeywordFalse _ -> JSONBool False
             in emitScalar
                  event
                  (decoderInput decoder)
                  decoder {decoderLex = LexNone}
          else Left (InvalidKeyword keyword)
  _ -> Left (InvalidKeyword mempty)

emitScalar :: JSONEvent -> Text -> Decoder -> Either ParseError DecoderResult
emitScalar event remaining decoder =
  let newDec = decoder {decoderInput = remaining, decoderLex = LexNone}
   in Right $ Emit event $ completeValue newDec

completeValue :: Decoder -> Decoder
completeValue decoder = case decoderContext decoder of
  [] -> decoder {decoderRoot = RootDone}
  Object _ : contexts ->
    decoder {decoderContext = Object ObjectExpectComma : contexts}
  Array _ : contexts ->
    decoder {decoderContext = Array ArrayExpectComma : contexts}

emitContainerEnd :: JSONEvent -> Text -> Decoder -> Either ParseError DecoderResult
emitContainerEnd event remaining decoder =
  let decoder' = decoder {decoderInput = remaining, decoderLex = LexNone}
   in Right $ Emit event (completeContainer decoder')

completeContainer :: Decoder -> Decoder
completeContainer = completeValue

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\t', '\n', '\r']

isNumberStart :: Char -> Bool
isNumberStart c = c == '-' || isDigit c

isHighSurrogate :: Int -> Bool
isHighSurrogate x = x >= 0xD800 && x <= 0xDBFF

isLowSurrogate :: Int -> Bool
isLowSurrogate x = x >= 0xDC00 && x <= 0xDFFF

decode ::
  (Monad m) =>
  Stream (Of Text) m r -> Stream (Of JSONEvent) m (Either ParseError r)
decode = runDecoder initialDecoder

runDecoder ::
  (Monad m) =>
  Decoder ->
  Stream (Of Text) m r ->
  Stream (Of JSONEvent) m (Either ParseError r)
runDecoder decoder input = do
  result <- lift $ S.next input
  case result of
    Left r
      | decoderRoot decoder == RootDone || decoder == initialDecoder ->
          pure (Right r)
      | otherwise -> drainFinish decoder r
    Right (chunk, rest) -> case feed chunk decoder of
      Left err -> pure (Left err)
      Right dr -> drain dr rest

drain ::
  (Monad m) =>
  DecoderResult ->
  Stream (Of Text) m r ->
  Stream (Of JSONEvent) m (Either ParseError r)
drain (Emit event nextDecoder) rest =
  S.yield event >> drainStep (step nextDecoder) rest
drain (NeedInput nextDecoder) rest
  | decoderRoot nextDecoder == RootDone && null (decoderContext nextDecoder) =
      drainDone nextDecoder rest
  | otherwise = runDecoder nextDecoder rest
drain (Done nextDecoder) rest = drainDone nextDecoder rest

drainStep ::
  (Monad m) =>
  Either ParseError DecoderResult ->
  Stream (Of Text) m r ->
  Stream (Of JSONEvent) m (Either ParseError r)
drainStep (Left err) _ = pure (Left err)
drainStep (Right result) rest = drain result rest

drainDone ::
  (Monad m) =>
  Decoder ->
  Stream (Of Text) m r ->
  Stream (Of JSONEvent) m (Either ParseError r)
drainDone nextDecoder rest = do
  more <- lift $ S.next rest
  case more of
    Left r -> drainFinish nextDecoder r
    Right (chunk, rest')
      | decoderRoot nextDecoder == RootDone && null (decoderContext nextDecoder) ->
          if Text.all isWhitespace chunk
            then drainDone nextDecoder rest'
            else pure (Left TrailingInput)
      | otherwise -> pure (Left TrailingInput)

drainFinish ::
  (Monad m) => Decoder -> r -> Stream (Of JSONEvent) m (Either ParseError r)
drainFinish decoder r = case finish decoder of
  Left UnexpectedEnd
    | getAll
        $ foldMap
          All
          [ decoderRoot decoder == RootExpectValue,
            null $ decoderContext decoder,
            decoderLex decoder == LexNone
          ] ->
        pure (Right r)
  Left err -> pure (Left err)
  Right (Done _) -> pure (Right r)
  Right (NeedInput _) -> pure (Left UnexpectedEnd)
  Right (Emit event nextDecoder) ->
    S.yield event >> drainFinish nextDecoder r

type StreamIO s = Stream (Of s) (ExceptT Text IO)

decodeIO :: StreamIO Text () -> StreamIO JSONEvent ()
decodeIO input = do
  result <- decode input
  case result of
    Left err -> throwError (show err)
    Right () -> pure ()
